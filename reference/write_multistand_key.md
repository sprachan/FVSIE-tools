# Write a keyword file to run multiple stands with the same simulation parameters

This function writes two .key files: one that defines the keywords
shared by all simulations ('shared_kwds.key') and one that tells FVS to
use these keywords to run the stands specified in `STDIDENTs` (or all
stands in the input database; '\<file_prefix\>.key'). Default shared
simulation parameters match those in the FVS GUI except outputs are
automatically written to an external file.

By default, the shared keyword file specifies a simulation where:

- Simulation results are output to a SQLite database called FVSOut.db in
  the working directory.

- Cycles are 10 years in length.

- The simulation is run for 100 years total.

- Self-calibration is turned ON (see [Essential
  FVS](https://www.fs.usda.gov/sites/default/files/forest-management/essential-fvs.pdf)
  section 6.4 for details).

- Missing heights are dubbed using the height-diameter relationships
  from the input data.

- Tripling is turned ON (see [Essential
  FVS](https://www.fs.usda.gov/sites/default/files/forest-management/essential-fvs.pdf) p.
  156 for details).

- The regeneration establishment model is turned ON (see [Essential
  FVS](https://www.fs.usda.gov/sites/default/files/forest-management/essential-fvs.pdf)
  section 5.4 for details).

See details for simulation options.

## Usage

``` r
write_multistand_key(
  STDIDENTs = NULL,
  out_dir = getwd(),
  database,
  file_prefix = "all_stands",
  addfile_ref = 40,
  ...
)
```

## Arguments

- STDIDENTs:

  Optional character vector of stand identifiers specifying the stands
  to be run in the simulation. If not provided, all STAND_ID values in
  the stand table of the provided database will be used.

- out_dir:

  A character string specifying the directory for the keyword file and
  FVS output. Defaults to working directory.

- database:

  A character string specifying the full file path of the input SQLite
  database.

- file_prefix:

  An optional character string giving the keyword file name to go before
  the '.key' extension. Default for multistand runs is 'all_stands'.

- addfile_ref:

  Optional integer file reference number, for FVS use. See FVS keyword
  documentation for the ADDFILE keyword. Must be at least 31; default is
  40.

- ...:

  Parameters passed to
  [`write_FVS_key()`](https://sprachan.github.io/FVSIE-tools/reference/write_FVS_key.md).
  See details.

## Value

The name of the keyword file that runs all requested stands, invisibly.

## Details

The following named arguments can be passed to the `...` argument (see
[`write_FVS_key()`](https://sprachan.github.io/FVSIE-tools/reference/write_FVS_key.md)
for details):

- `n_years`: Integer, default 100.

- `CYCLEAT`: Optional integer.

- `calibrate`: Logical, default `TRUE`.

- `htd_reg`: Logical, default `TRUE`.

- `triple`: Logical, default `TRUE`.

- `add_regen`: Logical, default `TRUE`.

- `carbon_report`: Logical, default `FALSE`.

- `estab_keywords`: Optional character vector of keywords.

- `ffe_keywords`: Optional character vector of keywords.

- `random_seed`: Optional odd integer.

- `db_tables`: Optional character vector.

- `READCORD`, `READCORH`, `READCORR`: Optional numeric vectors.

- `READCORH`: Optional numeric vector.

- `additionals`: Optional character vector.

## See also

[`write_FVS_key()`](https://sprachan.github.io/FVSIE-tools/reference/write_FVS_key.md)
