# Get a table of needed tree and/or stand list columns

These functions show you the default names for required columns (`col`),
their default value, if applicable (`default`; 9999 means so default),
whether or not the column has a default (`has_default`), acceptable
alternative column names (`alt_col`), and the required data type of the
column (`dtype`).

## Usage

``` r
get_tree_columns()

get_stand_columns()
```

## Value

A dataframe (32 rows x 5 columns for tree column table, 58 rows x 5
columns for stand column table.).

## Details

From Essential FVS: A User's Guide to the Forest Vegetation Simulator
(Dixon, updated March 2026), p. 47

## Examples

``` r
print(get_tree_columns())
#> Error in get_tree_columns(): object 'fvs_tree_cols' not found
print(get_stand_columns())
#> Error in get_stand_columns(): object 'fvs_stand_cols' not found

```
