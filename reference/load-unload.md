# Load/unload the specified FVS variant as a shared library

`load_FVS()` searches for the variant in the specified directory and
loads it if found. Errors if a different FVS variant is already loaded.

`unload_FVS()` searches for the specified FVS library in all loaded
libraries and unloads if found.

Both functions are automatically called within
[`run_FVS()`](https://sprachan.github.io/FVSIE-tools/reference/run_FVS.md).

## Usage

``` r
load_FVS(
  fvs_bin,
  program = c("FVSie", "FVSak", "FVSbm", "FVSca", "FVSci", "FVScr", "FVScs", "FVSec",
    "FVSem", "FVSnc", "FVSkt", "FVSls", "FVSne", "FVSpn", "FVSsn", "FVSso", "FVStt",
    "FVSut", "FVSwc", "FVSws"),
  verbose = FALSE
)

unload_FVS(
  program = c("FVSie", "FVSak", "FVSbm", "FVSca", "FVSci", "FVScr", "FVScs", "FVSec",
    "FVSem", "FVSnc", "FVSkt", "FVSls", "FVSne", "FVSpn", "FVSsn", "FVSso", "FVStt",
    "FVSut", "FVSwc", "FVSws"),
  verbose = FALSE
)
```

## Arguments

- fvs_bin:

  Character string. The file path where FVS DLLs are located.

- program:

  Character string, formatted as 'FVSxx' where xx is the two-letter
  variant code. Default is Inland Empire ('FVSie')

- verbose:

  Logical. Should FVS library loading/unloading be accompanied by a
  message? Default `FALSE`.

## Value

The variant name, invisibly, if successful.

## Examples
