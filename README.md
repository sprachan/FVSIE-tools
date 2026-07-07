
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rFVSIEtools

<!-- badges: start -->

<!-- badges: end -->

This package provides a well-documented set of tools designed to
facilitate running the Forest Vegetation Simulator, including tools for
processing Forest Inventory and Analysis (FIA) data into FVS-ready
formats.

## Intended Use

This package is primarily intended to streamline the use of FIA data
with FVS, make it easier to replicate runs, and reduce the amount of
work required to do many FVS runs with small tweaks (as one might need
to do for a simulation experiment).

## Workflow

### With FIA data

1.  Download state-level FIA data from the [FIA datamart
    website](https://research.fs.usda.gov/products/dataandtools/fia-datamart).

2.  Subset data to stands of interest using dplyr-style filter
    statements in `fetch_cond()` followed by `get_FIA_state()`.

3.  Write a keyword file specifying simulation parameters using one of
    two built-in functions: `write_FVS_key()` (for a single stand) or
    `write_multistand_key()` (for multiple stands).

4.  Run FVS with the desired variant using `run_FVS()`.

5.  Retrieve outputs from the resulting output (a SQL .db file is the
    default, but .xlsx and .xls outputs may also be requested).

<!--
## Example: Project growth for all FIA stands on forested public land in Montana
&#10;First, we subset the FIA data using the COND table (see [the NFI user guide](https://research.fs.usda.gov/sites/default/files/2025-08/wo-v9-4_Aug2025_UG_FIADB_database_description_NFI.pdf) for a description of this table).
&#10;``` r
library(rFVSIEtools)
library(dplyr)
&#10;# Data directories and files
fia_path <- file.path('data', 'raw_data', 'fia')
db_path <- file.path(fia_path, 'SQLite_FIADB_MT.db')
&#10;# open database connection and subset COND table
cond_subset <- fetch_cond(db_loc, 'STATECD == 30, INVYEAR >= 2001')
```
&#10;Then, we get FVS-ready stand and tree data from the selected FIA stands:
&#10;``` r
fvs_ready <- get_FIA_state(db_path, cond_subset, add_identifier = TRUE)
FVS_standInit <- fvs_ready$FVS_StandInit
FVS_treeInit <- fvs_ready$FVS_TreeInit
```
&#10;And finally, we run FVS in parallel using the default settings (project to 100 years in the future, self-calibration is on, tripling is turned off, and regeneration is turned off):
&#10;``` r
&#10;```
&#10;We could also run just one stand using:
&#10;``` r
run_FVS(FVS_StandInit[1,], FVS_treeInit, out_dir = file.path('outputs', 'test_run'), fvs_bin = file.path('C:', 'FVS', 'FVSSoftware', 'FVSbin'))
```
&#10;-->

## Installation

`rFVSIEtools` requires that the user have FVS on their machine. This is
relatively straightforward on Windows, where the program can be
downloaded from [the FVS
website](https://www.fs.usda.gov/fvs/software/complete.php) or built
using make following instructions on [the FVS
GitHub](https://github.com/USDAForestService/ForestVegetationSimulator/wiki/Build-Process-in-Windows-Using-make).
As of July 2026, the FVS GitHub page no longer includes instructions for
building FVS on non-Windows systems.

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

This package is still in development. See the issues tab.
