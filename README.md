
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rFVSIEtools

<!-- badges: start -->

<!-- badges: end -->

rFVSIEtools provides a well-documented set of tools designed to
facilitate running the Forest Vegetation Simulator-Inland Empire Variant
(FVS-IE), including tools for processing Forest Inventory and Analysis
(FIA) data into FVS-ready formats.

## Intended Use

This package is primarily intended to streamline the use of FIA data
with FVS and making multiple slightly different FVS runs (e.g., for
testing calibration techniques).

A typical workflow for running FVS on FIA data without this package
would be something like the following:

1.  Download FIA data.

2.  Use the FIA COND table to subset FVS_STANDINIT and FVS_TREEINIT to
    desired FIA plots (using SQL/dbplyr tools).

3.  Do a bunch of formatting to make the data FVS-ready, including
    setting variant-specific defaults as needed.

4.  Define custom functions for fetching tree lists from an FVS run.

5.  Run a large for loop over each stand:

    1)  Subset the large FIA treelist to the tree list corresponding to
        each stand.

    2)  Write keyword files by hand, using `sprintf()`, or using
        `rFVS::fvsMakeKeyFile()` (not described on FVS Wiki).

    3)  Write tree files by hand or with `sprintf()`.

    4)  Load FVS with `rFVS::fvsLoad()`

    5)  Start an FVS run using `rFVS::fvsSetCmdLine()` followed by
        `rFVS::fvsInteractRun()` or `rFVS::interactRun()`

6.  Combine each individually-run stand into one big stand dataframe,
    combine all trees into one big tree dataframe, and save results.

This workflow is doable, but it gets quite long and error-prone when you
have to do slightly different runs on the same set of stands.
`rFVSIEtools` simplifies the workflow to the following:

1.  Filter the FIA COND table to reflect the conditions from which to
    get stand and tree information. This can now be done in a single
    line with `fetch_cond()`.

2.  Get properly FVS-formatted stand and tree tables from state-level
    FIA data using `get_FIA_state()`.

3.  For many stands, use `run_FVS_parallel()` and provide the entire
    stand and tree list at once. This runs FVS in parallel, which is
    much faster than a for loop for many (\>100 or so) stands.

4.  For just a few stands, run a for loop over each stand and:

<!-- -->

1)  Use `run_FVS()` to run FVS.

## Example: Project growth for all FIA stands on forested public land in Montana

First, we subset the FIA data using the COND table (see [the NFI user
guide](https://research.fs.usda.gov/sites/default/files/2025-08/wo-v9-4_Aug2025_UG_FIADB_database_description_NFI.pdf)
for a description of this table).

``` r
library(rFVSIEtools)
library(dplyr)

# Data directories and files
fia_path <- file.path('data', 'raw_data', 'fia')
db_path <- file.path(fia_path, 'SQLite_FIADB_MT.db')

# open database connection and subset COND table
cond_subset <- fetch_cond(db_loc, 'STATECD == 30, INVYEAR >= 2001')
```

Then, we get FVS-ready stand and tree data from the selected FIA stands:

``` r
fvs_ready <- get_FIA_state(db_path, cond_subset, add_identifier = TRUE)
FVS_standInit <- fvs_ready$FVS_StandInit
FVS_treeInit <- fvs_ready$FVS_TreeInit
```

And finally, we run FVS in parallel using the default settings (project
to 100 years in the future, self-calibration is on, tripling is turned
off, and regeneration is turned off):

``` r
future::plan('multisession', workers = 5)
run_FVS_parallel(FVS_StandInit, FVS_treeInit, n_batches = 4, simple_output = TRUE, out_dir = file.path('outputs', 'test_run'), fvs_bin = fvs_bin)
```

We could also run just one stand using:

``` r
run_FVS(FVS_StandInit[1,], FVS_treeInit, out_dir = file.path('outputs', 'test_run'), fvs_bin = file.path('C:', 'FVS', 'FVSSoftware', 'FVSbin'))
```

## Installation

`rFVSIEtools` requires that the user have FVS on their machine. This is
relatively straightforward on Windows, where the program can be
downloaded from [the FVS
website](https://www.fs.usda.gov/fvs/software/complete.php) or built
using make following instructions on [the FVS
GitHub](https://github.com/USDAForestService/ForestVegetationSimulator/wiki/Build-Process-in-Windows-Using-make).
As of April 2026, the FVS GitHub page no longer includes instructions
for building FVS on any other system, though the [older version I forked
in September
2025](https://github.com/sprachan/ForestVegetationSimulator?tab=readme-ov-file#readme)
includes instructions for building on Unix.

`rFVSIEtools` also requires `rFVS`. `rFVS` installation into the
appropriate location on your machine should be done automatically on
installation of `rFVSIEtools`. If this does not work, first try:

``` r
# install.packages("remotes")
remotes::install_github("USDAForestService/ForestVegetationSimulator-Interface/rFVS")
```

Or:

``` r
# install.packages("pak")
pak::pak("USDAForestService/ForestVegetationSimulator-Interface/rFVS")
```

If you have downloaded the [complete FVS software package from the FVS
website](https://www.fs.usda.gov/fvs/software/complete.php), you can
load `rFVS` using:

``` r
# library(package = "rFVS", lib.loc =  file.path("<FVS Location>", "FVS", "FVSSoftware", "R", "R-<version number", "library", "rFVS")
```

If that doesn’t work, then follow [the instructions on the rFVS wiki to
build the package from the
source](https://github.com/USDAForestService/ForestVegetationSimulator-Interface/wiki/FVSOnline).

You can install the development version of rFVSIEtools from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sprachan/FVSIE-tools")
```

Or:

``` r
# install.packages("remotes")
remotes::install_github("sprachan/FVSIE-tools")
```

## Caveats

This package is still in development. Support for individual use cases
may vary; reporting functionality is particularly limited. See the
issues tab.
