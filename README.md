# sqlfielder

SQL-fielder generates fields for SQL queries from scanners.

It assumes the following:

- SQL queries are raw, their result is scanned with the `Scan(` method
- `Scan(` is invoked in a function `func scan<entity name>`

The generated consts are meant to be used in the query and are automatically
updated when a new `Scan` param is added.
For example:

```
    // scanAnimalsFields is the generated const
    query := fmt.Sprintf("SELECT %s FROM animals as a", scanAnimalsFields)
    row := sql.QueryRowContext(ctx, query)
    res, err := scanAnimals(row)

...

    func scanAnimals(row *sql.Row) Animal {
        var a Animal
        err := row.Scan(
            &a.Name, &a.Kingdom, &a.Class,
        )
```

Based on https://cs.opensource.google/go/go/+/master:src/image/color/palette/gen.go
and https://cs.opensource.google/go/x/tools/+/master:/cmd/stringer/stringer.go.
