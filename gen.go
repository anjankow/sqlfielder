// SQL-fielder generates fields for SQL queries from scanners.
//
// It assumes the following:
// - SQL queries are raw, their result is scanned with the `Scan(` method
// - `Scan(` is invoked in a function `func scan<name of what is scanned>`
//
// The generated consts are meant to be used in the query and are automatically
// updated when a new `Scan` param is added.
// For example:
//
//	// scanAnimalsFields is the generated const
//	query := fmt.Sprintf("SELECT %s FROM animals as a", scanAnimalsFields)
//	row := sql.QueryRowContext(ctx, query)
//	res, err := scanAnimals(row)
//
// ...
//
//	func scanAnimals(row *sql.Row) Animal {
//		var a Animal
//		err := row.Scan(
//			&a.Name, &a.Kingdom, &a.Class,
//		)
//
// Based on https://cs.opensource.google/go/go/+/master:src/image/color/palette/gen.go
// and https://cs.opensource.google/go/x/tools/+/master:/cmd/stringer/stringer.go.
package main

import (
	"bytes"
	"context"
	"flag"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"io"
	"log"
	"os"
	"regexp"
	"strings"
	"unicode"

	"golang.org/x/tools/go/packages"
)

// Flags
var (
	outputFilename = flag.String("output", "sql_fields.go", "output file name")
	packageName    = flag.String("p", "", "package to process")
)

// Comments that can be used next to the Scan function arguments
const (
	// skipComment is used to skip this and the following arguments
	skipComment = "sqlfielder:skip"
	// prefixComment is used to indicate the SELECT query prefix of a given argument
	prefixComment = "sqlfielder:prefix="
)

func main() {
	log.SetFlags(0)
	log.SetPrefix("sqlfielder: ")

	flag.Parse()
	if packageName == nil || *packageName == "" {
		flag.Usage()
		os.Exit(1)
	}

	ctx := context.Background()
	// Load the package
	pkg, err := loadPackage(ctx, ".")
	if err != nil {
		log.Fatal("failed to load the package: ", err)
	}

	// Parse it searching for Scan invocations
	parsed, errs := parsePackage(pkg)
	if len(parsed) == 0 {
		if len(errs) == 0 {
			log.Fatal("no Scan invocations found")
		}

		log.Fatalf("failed to parse the package: %+v", errs)
	}

	// Convert the parsed functions into constants to be used in SQL queries.
	buf, err := generate(parsed, errs)
	if err != nil {
		log.Fatal("failed to generate: ", err)
	}

	toWrite := buf.Bytes()
	// Format if possible
	data, err := format.Source(toWrite)
	if err != nil {
		log.Println("failed to format output: ", err)
	} else {
		toWrite = data
	}

	err = os.WriteFile(*outputFilename, toWrite, 0644)
	if err != nil {
		log.Fatal(err)
	}
}

// loadPackage loads the package in the given directory parsing each file for further processing.
func loadPackage(ctx context.Context, dir string) (pkg *packages.Package, err error) {
	parseMode := packages.NeedName | packages.NeedFiles | packages.NeedTypes | packages.NeedTypesInfo | packages.LoadSyntax
	cfg := packages.Config{
		Context: ctx,
		Mode:    parseMode,
		Dir:     dir,
		ParseFile: func(fset *token.FileSet, filename string, data []byte) (*ast.File, error) {
			var mode parser.Mode
			switch {
			case filename == *outputFilename,
				strings.HasSuffix(filename, "_with_trace.go"),
				strings.HasSuffix(filename, "_test.go"):
				// No need for parsing
				mode = parser.PackageClauseOnly
			default:
				mode = parser.ParseComments | parser.SkipObjectResolution | parser.AllErrors
			}

			return parser.ParseFile(fset, filename, data, mode)
		},
	}
	pkgs, err := packages.Load(&cfg)
	if err != nil {
		return
	}

	for _, p := range pkgs {
		if p.Name == *packageName {
			return p, nil
		}
	}

	return nil, fmt.Errorf("package %s not found", *packageName)
}

// parsePackage goes through each file in the package and looks for scan... function declarations.
// If found, each declaration in these functions is parsed to find Scan invocations.
// Scan invocations are processed to find the arguments and their corresponding prefixes.
func parsePackage(pkg *packages.Package) ([]ParsedScanFunc, []ParsedScanFuncErr) {
	parsedScans := make([]ParsedScanFunc, 0)
	errs := make([]ParsedScanFuncErr, 0)

	files := make(map[string]FileContent, len(pkg.Syntax))

	for _, file := range pkg.Syntax {
		if len(file.Decls) == 0 {
			// No declarations, no need to load a comment map
			continue
		}

		cmap := ast.NewCommentMap(pkg.Fset, file, file.Comments)

		for _, decl := range file.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok {
				// Find `func scan...` functions declared in every file
				if strings.HasPrefix(fn.Name.Name, "scan") {

					// Found it!
					// Read the file content if not already read
					filePath := pkg.Fset.Position(file.Pos()).Filename
					fileContent, err := readFile(file, filePath, files)
					if err != nil {
						// This is fatal
						log.Panicf("failed to read a file %s/%s: %s", pkg.Dir, file.Name, err)
					}

					// Now do through each statement inside the function body.
					// Look for for lines like `err := s.Scan(`
					// so an assignment, which right hand side is a call expression to `Scan`.
					for _, stmt := range fn.Body.List {
						switch stmt := stmt.(type) {
						case *ast.ForStmt:
							// Some s.Scan calls might be placed in a for loop.
							// Then parse each declaration inside it.
							for _, stmt := range stmt.Body.List {
								assignment, ok := stmt.(*ast.AssignStmt)
								if !ok {
									continue
								}
								scanFunctionCall, err := parseAssignmentStatement(assignment, fn, cmap, fileContent)
								if err != nil {
									errs = append(errs, *err)
									continue
								}
								if scanFunctionCall != nil {
									parsedScans = append(parsedScans, *scanFunctionCall)
								}
							}
						case *ast.AssignStmt:
							scanFunctionCall, err := parseAssignmentStatement(stmt, fn, cmap, fileContent)
							if err != nil {
								errs = append(errs, *err)
								continue
							}
							if scanFunctionCall != nil {
								parsedScans = append(parsedScans, *scanFunctionCall)
							}
						}
					}
				}
			}
		}
	}

	return parsedScans, errs
}

type FileContent struct {
	Content io.ReaderAt
	BasePos int
}

func readFile(file *ast.File, filePath string, files map[string]FileContent) (FileContent, error) {
	if reader, ok := files[filePath]; ok {
		return reader, nil
	}

	f, err := os.Open(filePath)
	if err != nil {
		return FileContent{}, err
	}

	content := FileContent{
		Content: f,
		BasePos: int(file.FileStart),
	}
	files[filePath] = content

	return content, nil
}

// parseAssignmentStatement parses assignment statement, like `a = b`,
// searching for Scan method calls, so assignments like `res := s.Scan(...)`.
// If such assignment is found, it parses it, getting the arguments and corresponding prefixes.
func parseAssignmentStatement(stmt *ast.AssignStmt, fn *ast.FuncDecl, cmap ast.CommentMap, fileContent FileContent) (*ParsedScanFunc, *ParsedScanFuncErr) {

	for _, rhs := range stmt.Rhs {
		callExpr, ok := rhs.(*ast.CallExpr)
		if !ok {
			continue
		}
		selectorExprScan, ok := callExpr.Fun.(*ast.SelectorExpr)
		if !ok {
			continue
		}
		if selectorExprScan.Sel.Name != "Scan" {
			continue
		}

		// Found Scan method call!
		// There should be only one in a single scan... function.
		scanFunc, err := parseScanInvocation(callExpr, cmap, fn.Name.Name, fileContent)
		if err != nil {
			return nil, &ParsedScanFuncErr{
				FunctionName: fn.Name.Name,
				Errs:         err,
			}
		}
		return scanFunc, nil
	}

	return nil, nil
}

// ScanArg is a single argument to a Scan function
type ScanArg struct {
	NameWithPath string
	// Path represents all that stands before the field name,
	// e.g. in object.Field.ScanDst, the path is object.Field
	Path string
	// Expr is the underlying expression, used only for debugging purposes.
	Expr ast.Expr
}

func (arg ScanArg) FieldName() string {
	if idx := strings.LastIndex(arg.NameWithPath, "."); idx > 0 {
		return arg.NameWithPath[idx+1:]
	}
	return arg.NameWithPath
}

// ParsedScanFunc is the result of parsing a Scan function call
type ParsedScanFunc struct {
	// FuncName is the name of the function calling Scan method.
	// It's expected to be private and called `scan...`, e.g. `scanObject
	FuncName string
	// Args are the arguments to the Scan function
	Args []ScanArg
	// Prefixes maps a field "path" to a SQL prefix in a SELECT statement.
	// All arguments with the same path, e.g. object.Field1.Arg1 and object.Field1.Arg2,
	// share the same prefix, so it's enough to declare it once.
	Prefixes map[string]string
}

// parseScanInvocation parses a single `Scan` method invocation, gathering the arguments and corresponding prefixes.
func parseScanInvocation(scanCall *ast.CallExpr, cmap ast.CommentMap, funcName string, fileContent FileContent) (*ParsedScanFunc, []error) {
	// `.Scan(` arguments
	args := make([]ScanArg, 0)
	// sql prefixes given in the comments
	prefixes := make(map[string] /*path*/ string /*prefix*/)
	prefixes[""] = "" // use no prefix if not needed
	missingPathPrefixes := make(map[string]bool)

	// In case of error, continue to collect all the possible errors at once.
	errs := make([]error, 0)
	for i, arg := range scanCall.Args {

		// First parse it
		scanArg, err := parseArgument(arg, fileContent)
		if err != nil {
			errs = append(errs, fmt.Errorf("arg#%d: %s", i, err))
			// Continue parsing other args, we want to catch all errors at once
			continue
		}

		// Check if there's any relevant comment given
		skip, commentPrefix, err := parseArgComment(arg, cmap)
		if err != nil {
			errs = append(errs, fmt.Errorf("%q: %s", scanArg.NameWithPath, err))
			// Continue parsing other args, we want to catch all errors at once
			continue
		}

		// Check if there is a prefix for this path.
		// If none is found, it might be given for a next argument.
		// For now, just mark it as missing.
		if !validatePrefix(scanArg, prefixes, commentPrefix) {
			missingPathPrefixes[scanArg.Path] = true
		}

		if skip {
			log.Default().Printf("%s: skipping %q\n", funcName, scanArg.NameWithPath)
			continue
		}

		args = append(args, scanArg)
	}

	// Check if some prefixes are still missing
	for path := range missingPathPrefixes {
		if _, ok := prefixes[path]; !ok {
			errs = append(errs, fmt.Errorf("%q: needed prefix", path))
		}
	}

	if len(errs) > 0 {
		return &ParsedScanFunc{}, errs
	}
	if len(args) == 0 {
		return nil, []error{fmt.Errorf("%s: no Scan arguments found", funcName)}
	}
	return &ParsedScanFunc{
		FuncName: funcName,
		Args:     args,
		Prefixes: prefixes,
	}, nil
}

func parseArgument(arg ast.Expr, fileContent FileContent) (ScanArg, error) {
	argLen := int(arg.End() - arg.Pos())
	dst := make([]byte, argLen)

	position := int(arg.Pos()) - fileContent.BasePos
	n, err := fileContent.Content.ReadAt(dst, int64(position))
	if err != nil {
		return ScanArg{}, err
	}
	if n != argLen {
		return ScanArg{}, fmt.Errorf("short read %d/%d", n, argLen)
	}

	argName := string(dst)
	path := ""
	if idx := strings.LastIndex(argName, "."); idx > 0 {
		path = argName[:idx]
	}

	return ScanArg{
		NameWithPath: string(dst),
		Path:         strings.TrimPrefix(path, "&"),
		Expr:         arg,
	}, nil
}

var (
	prefixCommentRegex = regexp.MustCompile(fmt.Sprintf("%s(\\S+)", prefixComment))
)

func parseArgComment(arg ast.Expr, cmap ast.CommentMap) (skip bool, prefix string, err error) {

	// If a field is marked with a skip comment, we will stop here.
	// Next arguments are not included in the list.
	comment := cmap.Filter(arg)
	if strings.Contains(comment.String(), skipComment) {
		skip = true
	}

	// Check if there is a prefix comment.
	// The prefix is a string before the column name in a sql query.
	if strings.Contains(comment.String(), prefixComment) {
		match := prefixCommentRegex.FindStringSubmatch(comment.String())
		if len(match) != 2 {
			err = fmt.Errorf("unexpected comment format, should be \"%s<value>\"", prefixComment)
			return
		}

		prefix = match[1]
	}
	return
}

// validatePrefix for this path. It returns true if a SQL prefix exists.
// 1. If prefix is given explicitly in the comment, it's saved in prefixes map.
// 2. If the  path is possible to use it as a prefix, it's saved in prefixes map.
func validatePrefix(scanArg ScanArg, prefixes map[string]string, commentPrefix string) bool {
	if commentPrefix != "" {
		// If prefix is given explicitly, save it in prefixes map
		// overrwiting any existing prefix.
		// Explicit prefix always takes precedence.
		prefixes[scanArg.Path] = commentPrefix
		return true
	}

	if _, prefixExists := prefixes[scanArg.Path]; prefixExists {
		// There already is a prefix associated with this path
		return true
	}

	// No prefix given, use the argument path if consists of one part only
	// (e.g. an argument is given as &a.Field)
	if !strings.ContainsRune(scanArg.Path, '.') {
		prefixes[scanArg.Path] = scanArg.Path
		return true
	}

	// If there is more than one part, we need to have a prefix given.
	return false
}

type ParsedScanFuncErr struct {
	Errs         []error
	FunctionName string
}

// generate writes string constants for each `Scan` function
// invocation in a form of SELECT query parameters.
// In addition it writes the error messages for each scan function that failed to be parsed.
func generate(parsed []ParsedScanFunc, errs []ParsedScanFuncErr) (bytes.Buffer, error) {
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "// Code generated by go run gen.go -output %s; DO NOT EDIT.\n", *outputFilename)
	fmt.Fprintln(&buf)
	fmt.Fprintf(&buf, "package %s\n", *packageName)
	fmt.Fprintln(&buf)
	fmt.Fprintln(&buf, "const (")

	for _, p := range parsed {
		constName := p.FuncName + "Fields"
		args := make([]string, 0, len(p.Args))
		for _, arg := range p.Args {
			prefix, ok := p.Prefixes[arg.Path]
			if !ok {
				prefix = strings.Split(arg.Path, ".")[0]
			}
			name := toSnake(arg.FieldName())
			args = append(args, fmt.Sprintf("%s.%s", prefix, name))
		}
		fmt.Fprintf(&buf, "%s = %q\n", constName, strings.Join(args, ", "))
	}

	fmt.Fprintln(&buf)
	fmt.Fprintln(&buf, ")")
	fmt.Fprintln(&buf)

	if len(errs) == 0 {
		return buf, nil
	}
	fmt.Fprintln(&buf, "///////////////////////////////////////////////////////////")
	fmt.Fprintln(&buf, "// ERRORS:")
	for _, fnErr := range errs {
		fmt.Fprintln(&buf)
		fmt.Fprintln(&buf, "// ", fnErr.FunctionName)
		for _, argErr := range fnErr.Errs {
			fmt.Fprintln(&buf, "// ", argErr)
		}
	}
	return buf, nil
}

// toSnake converts go-style field names (e.g. IPAddr, ipAddr)
// to their snake representation (ip_addr)
func toSnake(field string) string {
	if len(field) == 0 {
		return ""
	}

	out := make([]rune, 0, len(field))
	runes := []rune(field)
	consecutiveUpper := 0
	for i := range len(runes) - 1 {
		currentRune := runes[i]
		next := runes[i+1]

		if unicode.IsLower(currentRune) {
			consecutiveUpper = 0
			out = append(out, unicode.ToLower(currentRune))
			if unicode.IsUpper(next) {
				out = append(out, '_')
			}
			continue
		}

		// The current rune is lower
		consecutiveUpper++
		if unicode.IsUpper(next) {
			out = append(out, unicode.ToLower(currentRune))
			continue
		}

		// Add an underscore here only if there was more than 1
		// consecutive upper runes, e.g. MFAEnabled -> mfa_enabled
		if consecutiveUpper > 1 {
			out = append(out, '_')
		}
		out = append(out, unicode.ToLower(currentRune))

	}
	// Add the last rune
	out = append(out, unicode.ToLower(runes[len(runes)-1]))
	return string(out)
}
