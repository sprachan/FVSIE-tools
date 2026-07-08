# Write an FVS Keyword file.

Given simulation parameters and file paths, this function writes a .key
file that can be used to control an FVS simulation. Automatic formatting
is supported for a variety of keywords; additional keywords may be
passed but must be formatted according to the [FVS Keyword
guide](https://www.fs.usda.gov/sites/default/files/fvs-keyword.pdf).
Default simulation parameters (cycle length, number of years,
calibration, etc) are designed to match the FVS GUI defaults specified
in Essential FVS, except outputs are automatically written to a SQLite
database.

By default, the keyword file specifies a simulation where:

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

## Usage

``` r
write_FVS_key(
  out_dir = getwd(),
  file_prefix,
  database,
  STDIDENT = NULL,
  n_years = 100,
  CYCLEAT = NULL,
  calibrate = TRUE,
  htd_reg = TRUE,
  triple = TRUE,
  add_regen = TRUE,
  carbon_report = FALSE,
  estab_keywords = NULL,
  ffe_keywords = NULL,
  random_seed = NULL,
  db_tables = NULL,
  READCORD = NULL,
  READCORH = NULL,
  READCORR = NULL,
  additionals = NULL
)
```

## Arguments

- out_dir:

  A character string specifying the directory for the keyword file and
  FVS output. Defaults to working directory.

- file_prefix:

  A character string giving the keyword file name to go before the
  '.key' extension.

- database:

  A character string specifying the full file path of the input SQLite
  database.

- STDIDENT:

  Optional character string specifying the stand identity. Should be
  NULL (default) for multistand projections; see *THIS WOULD BE A GOOD
  VIGNETTE*

- n_years:

  Integer specifying the number of years into the future to project.
  Default 100.

- CYCLEAT:

  Optional integer specifying additional years to add cycles. Useful for
  getting stand/tree information in specific years.

- calibrate:

  Logical. Should self-calibration be on (`TRUE`, default) or turned off
  for the simulation (`FALSE`)?

- htd_reg:

  Logical. Should missing heights be dubbed using heights in the data
  (`TRUE`, default) or using regional defaults (`FALSE`)?

- triple:

  Logical. Should tree records be split into three identical records
  whose TPA sums to the original tree TPA in order to stabilize
  estimates in small stands (`TRUE`, default) or not (`FALSE`)?

- add_regen:

  Logical. Should the regeneration/establishment model associated with
  the variant be used (`TRUE`, default) or turned off (`FALSE`)?

- carbon_report:

  Logical. Should the Fire and Fuels extension carbon report be
  generated (`TRUE`) or not (`FALSE`, default)?

- estab_keywords:

  Optional character vector of keywords to control the
  regeneration/establishment model (see Essential FVS 5.4). Each element
  in the vector (or string if only one keyword is passed) must be
  formatted according to the FVS keyword guide.

- ffe_keywords:

  Optional character vector of keywords to pass to the Fire and Fuels
  Extension. Each element in the vector (or string if only one keyword
  is passed) must be formatted according to the FVS keyword guide.

- random_seed:

  Optional odd integer to reseed the random number generator for the
  base FVS model. Useful for reproducible simulation results.

- db_tables:

  Optional character vector of keywords to request additional output
  tables.

- READCORD:

  Optional numeric vector of multipliers for the large tree diameter
  growth model. The length and order of the vector must correspond to
  the species in the FVS variant in order for multipliers to apply
  correctly. See Essential FVS 6.5.2.2.

- READCORH:

  Optional numeric vector of multipliers for the large tree height
  growth model. The length and order of the vector must correspond to
  the species in the FVS variant in order for multipliers to apply
  correctly. See Essential FVS 6.5.2.2.

- READCORR:

  Optional numeric vector of multipliers for the small tree height
  growth model. The length and order of the vector must correspond to
  the species in the FVS variant in order for multipliers to apply
  correctly. See Essential FVS 6.5.2.2.

- additionals:

  Optional character vector of additional keywords. Each element must be
  formatted according to the FVS keyword guide.

## Value

The keyword file name, invisibly.

## See also

[`format_keyword()`](https://sprachan.github.io/FVSIE-tools/reference/format_keyword.md)
for formatting keywords;
[`write_multistand_key()`](https://sprachan.github.io/FVSIE-tools/reference/write_multistand_key.md)
for writing a keyword file to run multiple stands with the same
simulation parameters.
