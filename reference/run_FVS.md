# Run FVS from a database with stand and tree information.

Run FVS from a database with stand and tree information.

## Usage

``` r
run_FVS(
  fvs_bin,
  variant = c("ie", "ak", "bm", "ca", "ci", "cr", "cs", "ec", "em", "nc", "kt", "ls",
    "ne", "pn", "sn", "so", "tt", "ut", "wc", "ws"),
  keyword_file,
  stop_year = NULL,
  stop_code = NULL,
  stop_file = NULL,
  verbose = FALSE
)
```

## Arguments

- fvs_bin:

  Character string. The file path where FVS DLLs are located.

- variant:

  Character string. The two-letter code corresponding to an FVS variant.
  Default is Inland Empire ('ie').

- keyword_file:

  Character string. Path to the keyword file controlling the simulation;
  include the .key extension. See
  [`write_FVS_key()`](https://sprachan.github.io/FVSIE-tools/reference/write_FVS_key.md)
  for keyword file generation.

- stop_year:

  Optional integer. Year to stop simulation and store simulation state.

- stop_code:

  Optional integer. State of the simulation to store.

- stop_file:

  Optional character string. Where to store the stand variables at the
  supplied stop point.

- verbose:

  Logical. Should FVS library loading/unloading be accompanied by a
  message? Default `FALSE`.

## Value

The FVS program name (e.g., 'FVSie'), invisibly. Simulation outputs are
sent to the file specified in the keyword file and to a '.out' file
(location and prefix specified in keyword file).

## Examples

``` r
if (FALSE) { # \dontrun{
out_dir <- tempdir()
database <- system.file(file.path('extdata', 'ex_data.db'), package = 'rFVSIEtools')
kwd <- write_FVS_key(out_dir = out_dir, file_prefix = 'example_kwd', database = database)

run_FVS(fvs_bin = 'C:/FVS/FVSSoftware/FVSbin', variant = 'ie', keyword_file = kwd)
} # }
```
