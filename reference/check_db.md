# Helper function to check database validity for running FVS

Helper function to check database validity for running FVS

## Usage

``` r
check_db(database)
```

## Arguments

- database:

  Character string. File path to the SQLite input database.

## Value

Nothing. Errors if FVS_StandInit and/or FVS_TreeInit not found in the
database.
