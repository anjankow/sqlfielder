//go:build ignore

package main

// Based on https://cs.opensource.google/go/go/+/master:src/image/color/palette/gen.go
// This program generates fields for SQL queries from scanners. Invoke it as
//	go run gen.go -output sql_fields.go

import (
	"bytes"
	"context"
	"errors"
	"flag"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"log"
	"os"
	"regexp"
	"slices"
	"strings"

	"github.com/iancoleman/strcase"
	"golang.org/x/tools/go/packages"
)

var outputFilename = flag.String("output", "sql_fields.go", "output file name")

// Comments that can be used next to the Scan function arguments
const (
	// skipComment is used to skip this and the following arguments
	skipComment = "sqlfieldgen:skip"
	// prefixComment is used to indicate the SELECT query prefix of a given argument
	prefixComment = "sqlfieldgen:prefix="
)

var (
	prefixCommentRegex = regexp.MustCompile(fmt.Sprintf("%s(\\S+)", prefixComment))
)

func main() {
	log.SetFlags(0)
	log.SetPrefix("sql-fields-gen: ")

	flag.Parse()

	ctx := context.Background()
	// Load mysql package
	pkg, err := loadPackage(ctx, ".")
	if err != nil {
		log.Fatal("failed to load the package: ", err)
	}

	// Parse it searching for Scan invocations
	parsed, errs := parsePackage(pkg)
	if len(parsed) == 0 {
		log.Fatal("failed to parse the package")
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
	parseMode := packages.NeedName | packages.NeedFiles | packages.NeedTypes | packages.NeedTypesInfo | packages.LoadAllSyntax
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
		if p.Name == "mysql" {
			return p, nil
		}
	}

	return nil, errors.New("package mysql not found")
}

// parsePackage goes through each file in the package and looks for scan... function declarations.
// If found, each declaration in these functions is parsed to find Scan invocations.
// Scan invocations are processed to find the arguments and their corresponding prefixes.
func parsePackage(pkg *packages.Package) ([]ParsedScanFunc, []ParsedScanFuncErr) {
	parsedScans := make([]ParsedScanFunc, 0)
	errs := make([]ParsedScanFuncErr, 0)

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

					// Found it! Now do through each statement inside the function body.
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
								scanFunctionCall, err := parseAssignmentStatement(assignment, fn, cmap)
								if err != nil {
									errs = append(errs, *err)
									continue
								}
								if scanFunctionCall != nil {
									parsedScans = append(parsedScans, *scanFunctionCall)
								}
							}
						case *ast.AssignStmt:
							scanFunctionCall, err := parseAssignmentStatement(stmt, fn, cmap)
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

// parseAssignmentStatement parses assignment statement, like `a = b`,
// searching for Scan method calls, so assignments like `res := s.Scan(...)`.
// If such assignment is found, it parses it, getting the arguments and corresponding prefixes.
func parseAssignmentStatement(stmt *ast.AssignStmt, fn *ast.FuncDecl, cmap ast.CommentMap) (*ParsedScanFunc, *ParsedScanFuncErr) {

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
		scanFunc, err := parseScanInvocation(callExpr, cmap, fn.Name.Name)
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
	// Name is the name of the field, e.g. in object.Field.ScanDst, the name is ScanDst
	Name string
	// Path represents all that stands before the field name,
	// e.g. in object.Field.ScanDst, the path is object.Field
	Path string
	// Expr is the underlying expression, used only for debugging purposes.
	Expr ast.Expr
}

func (a ScanArg) FullName() string {
	return strings.Join([]string{a.Path, a.Name}, ".")
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
func parseScanInvocation(scanCall *ast.CallExpr, cmap ast.CommentMap, funcName string) (*ParsedScanFunc, []error) {
	// `.Scan(` arguments
	args := make([]ScanArg, 0)
	// sql prefixes given in the comments
	prefixes := make(map[string]string)

	// In case of error, continue to collect all the possible errors at once.
	errs := make([]error, 0)
	for i, arg := range scanCall.Args {

		scanArg, err := parseScanArgument(i, arg, funcName)
		if err != nil {
			// Continue parsing, we want to catch all errors at once
			errs = append(errs, err)
			continue
		}

		skip, err := parseArgComment(scanArg, arg, cmap, prefixes)
		if err != nil {
			errs = append(errs, err)
			continue
		}

		if skip {
			log.Default().Printf("%s: skipping %q\n", funcName, scanArg.FullName())
			continue
		}

		args = append(args, scanArg)
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

func parseScanArgument(idx int, arg ast.Expr, funcName string) (ScanArg, error) {
	// Each of the arguments should be a unary expression: a pointer to a variable
	unaryExpl, ok := arg.(*ast.UnaryExpr)
	if !ok {
		return ScanArg{}, NewInvalidTypeErr(fmt.Sprintf("arg#%v", idx), arg)
	}

	// Get the argument.
	// If a field is simple variable, e.g. var count int,
	// then the type is ast.Ident.
	// If it's a struct member, it's a ast.SelectorExpr.
	var scanArg ScanArg
	switch expr := unaryExpl.X.(type) {
	case *ast.Ident:
		log.Default().Printf("%s: use '// %s' to skip %q if needed\n",
			funcName, skipComment, expr.Name)
		scanArg = ScanArg{
			Name: expr.Name,
			Path: "",
			Expr: expr,
		}
	case *ast.SelectorExpr:
		arg, err := parseScanArg(expr)
		if err != nil {
			return ScanArg{}, err
		}
		scanArg = arg
	default:
		return ScanArg{}, NewInvalidTypeErr(fmt.Sprintf("arg#%v)", idx), arg)
	}

	return scanArg, nil
}

func parseArgComment(scanArg ScanArg, arg ast.Expr, cmap ast.CommentMap, prefixes map[string]string) (skip bool, err error) {

	// If a field is marked with a skip comment, we will stop here.
	// Next arguments are not included in the list.
	comment := cmap.Filter(arg)
	if strings.Contains(comment.String(), skipComment) {
		return true, nil
	}

	// Check if there is a prefix comment.
	// The prefix is a string before the column name in a sql query.
	if strings.Contains(comment.String(), prefixComment) {
		match := prefixCommentRegex.FindStringSubmatch(comment.String())
		if len(match) != 2 {
			return false, NewInvalidCommentErr(scanArg, comment.String())
		}

		prefix := match[1]

		// Assign the prefix. It will be used by all the fields with the same path.
		prefixes[scanArg.Path] = prefix
	}

	return false, nil
}

type ParsedScanFuncErr struct {
	Errs         []error
	FunctionName string
}

func NewInvalidTypeErr(argument string, value any) error {
	return ParseArgError{
		Argument: argument,
		Value:    value,
		Reason:   ReasonWrongType,
	}
}

func NewInvalidCommentErr(scanArg ScanArg, comment string) error {
	return ParseArgError{
		Argument: scanArg.FullName(),
		Value:    comment,
		Reason:   ReasonWrongComment,
	}
}

const (
	ReasonWrongType = iota
	ReasonWrongComment
)

type ParseArgError struct {
	Argument string
	Value    any
	Reason   int
}

func (e ParseArgError) Error() string {
	switch e.Reason {
	case ReasonWrongType:
		err := fmt.Sprintf("%q: wrong type %T of the expression (%+v); expected a pointer to a struct field or a pointer to a variable",
			e.Argument, e.Value, e.Value)
		return err
	case ReasonWrongComment:
		return fmt.Sprintf("%q: unexpected comment format, should be \"%s<value>\"",
			e.Argument, prefixComment)
	default:
		panic("invalid error reason given, fix me please")
	}

}

// parseScanArg parses a single Scan argument that is a pointer to a member of a struct,
// e.g. `&object.Field`.
func parseScanArg(expr *ast.SelectorExpr) (ScanArg, error) {
	path := make([]string, 0)

	fieldName := expr.Sel.Name
	scanArg := ScanArg{
		Name: fieldName,
		Expr: expr,
	}

	// Use pointer to traverse the nested SelectorExpressions.
	var ptr *ast.SelectorExpr = expr

	// Parse the path - e.g. in &object.Field1.Field2.Field3,
	// the path is object.Field1.Field2 and the name is Field3
	for {

		switch inner := ptr.X.(type) {
		case *ast.SelectorExpr:
			path = append(path, inner.Sel.Name)
			// Move the pointer to the inner SelectorExpr
			ptr = inner
		case *ast.Ident:
			// If it's a ast.Ident, we reached the final path fragment.
			// In the example &object.Field1.Field2.Field3,
			// the final fragment is `object`.
			path = append(path, inner.Name)
			// Reverse the path, because the last fragment is actually the first one we read.
			slices.Reverse(path)
			scanArg.Path = strings.Join(path, ".")
			return scanArg, nil
		default:
			return ScanArg{}, NewInvalidTypeErr(fieldName, ptr.X)
		}
	}
}

// generate writes string constants for each `Scan` function
// invocation in a form of SELECT query parameters.
// In addition it writes the error messages for each scan function that failed to be parsed.
func generate(parsed []ParsedScanFunc, errs []ParsedScanFuncErr) (bytes.Buffer, error) {

	var buf bytes.Buffer

	fmt.Fprintf(&buf, "// Code generated by go run gen.go -output %s; DO NOT EDIT.\n", *outputFilename)
	fmt.Fprintln(&buf)
	fmt.Fprintln(&buf, "package mysql")
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
			name := strcase.ToSnake(arg.Name)
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
