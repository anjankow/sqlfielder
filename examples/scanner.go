package examples

import (
	"context"
	"database/sql"
	"fmt"
)

type Database interface {
	QueryRowContext(ctx context.Context, query string, args ...interface{}) *sql.Row
}

type Animal struct {
	Name    string
	Kingdom string
	Class   string
}

func GetAnimal(ctx context.Context, db Database, name string) (Animal, error) {
	query := fmt.Sprintf("SELECT %s FROM animals as a WHERE name = ?", scanAnimalsFields)
	row := db.QueryRowContext(ctx, query)
	return scanAnimals(row)
}

func scanAnimals(row *sql.Row) (Animal, error) {
	var a Animal
	err := row.Scan(&a.Name, &a.Kingdom, &a.Class)
	if err != nil {
		return Animal{}, err
	}

	return a, nil
}
