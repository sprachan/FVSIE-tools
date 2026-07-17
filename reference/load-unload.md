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

``` r
if (FALSE) { # \dontrun{
load_FVS(program = 'FVSie', fvs_bin = 'C:/FVS/FVSSoftware/FVSbin')
#> FVSie loaded.

# Attempting to load an already loaded library will lead to a message:
load_FVS(program = 'FVSie', fvs_bin = 'C:/FVS/FVSSoftware/FVSbin')
load_FVS('FVSie', fvs_bin = 'C:/FVS/FVSSoftware/FVSbin')
#> FVSie already loaded.

# Similarly, loading a different variant when one is already loaded will error:
load_FVS(program = 'FVSie', fvs_bin = 'C:/FVS/FVSSoftware/FVSbin')
load_FVS(program = 'FVSak', fvs_bin = 'C:/FVS/FVSSoftware/FVSbin')
#> Error in `load_FVS()`:
#> ! Variant(s) FVSie already loaded.
#> ℹ Use unload_FVS() to unload, then try again.
#> Run `rlang::last_trace()` to see where the error occurred.
#> Called from: signal_abort(cnd, .file)

unload_FVS(program = 'FVSie')
#> FVSie unloaded.
} # }
```
