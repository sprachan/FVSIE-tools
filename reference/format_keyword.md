# Format an FVS keyword

Add whitespace and use optional datatypes to format an FVS keyword to
match requirements. This function does not format READCOR\* values,
which must be passed to
[`write_FVS_key()`](https://sprachan.github.io/FVSIE-tools/reference/write_FVS_key.md)
as vectors in order of the species in the variant.

## Usage

``` r
format_keyword(keyword, ..., dtypes = NULL, round_to = 2)
```

## Arguments

- keyword:

  A character string. The FVS keyword name.

- ...:

  Optional values for fields associated with the keyword. Will be
  coerced to the type specified by `dtypes` if specified or character if
  `dtypes` is not specified.

- dtypes:

  Optional character vector specifying the datatype of each field. The
  length needs to match the number of fields. 's' for string, 'i' for
  integer, and 'f' for float (decimals)

- round_to:

  An optional integer specifying how many places floats should be
  rounded to (default 2).

## Value

A character string.

## Examples

``` r
format_keyword('NOINGROW')
#> [1] "NOINGROW  "

format_keyword('SDICALC', 0, 1, 1)
#> [1] "SDICALC            0         1         1"

# Lower case will be converted to upper case,
#  and any spaces will be removed in the keyword:
format_keyword('sdi calc', 0, 1, 1)
#> [1] "SDICALC            0         1         1"

# not recommended to use for floats without specifying datatype:
# BAD
try(format_keyword('SDICALC', 1/3, 1, 1))
#> Error in format_keyword("SDICALC", 1/3, 1, 1) : 
#>   Fields must be at most 10 characters if dtype not provided

# OK
format_keyword('SDICALC', dtypes = c('f', 'i', 'i'), 1/3, 1, 1)
#> [1] "SDICALC         0.33         1         1"

# For multiple keywords to go one after another, use paste() with sep = '\n':
additionals <- paste(format_keyword('SDICALC', dtypes = c('f', 'i', 'i'), 1/3, 1, 1),
                     format_keyword('SDIMAX',
                                    dtypes = c('i', 'f', 's', 's', 'f', 'f', 'i'),
                                    1, 300, '', '', 65, 90, 0),
                     sep = "\n")
cat(additionals)
#> SDICALC         0.33         1         1
#> SDIMAX             1    300.00                         65.00     90.00         0
```
