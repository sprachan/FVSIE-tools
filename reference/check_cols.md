# Check for required columns

Check for required columns

## Usage

``` r
check_cols(user_df, col_df)
```

## Arguments

- user_df:

  User-supplied dataframe.

- col_df:

  Dataframe of columns to check against.

## Value

Error if required columns are missing with no defaults and warns if
there are missing columns that do have default values.
