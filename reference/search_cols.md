# Search a user-supplied dataframe for necessary columns

Search a user-supplied dataframe for necessary columns

## Usage

``` r
search_cols(user_df, col_df)
```

## Arguments

- user_df:

  User-supplied dataframe

- col_df:

  Dataframe containing column information (column name, alternative
  names, defaults)

## Value

6-column dataframe: *required_col*: Character. Name of required column
from col_df. *data_col*: Character. Name of the matching column found in
user_df. *default*: Character. Default value for the column, 9999 if no
default is available. *has_default*: Boolean. Does the column have a
default value? *dtype*: Character. FVS required datatype for the default
value.
