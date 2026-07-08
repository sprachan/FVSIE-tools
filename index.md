# rFVSIEtools

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
    statements in
    [`fetch_cond()`](https://sprachan.github.io/FVSIE-tools/reference/get_FIA_state.md)
    followed by
    [`get_FIA_state()`](https://sprachan.github.io/FVSIE-tools/reference/get_FIA_state.md).

3.  Write a keyword file specifying simulation parameters using one of
    two built-in functions:
    [`write_FVS_key()`](https://sprachan.github.io/FVSIE-tools/reference/write_FVS_KEY.md)
    (for a single stand) or
    [`write_multistand_key()`](https://sprachan.github.io/FVSIE-tools/reference/write_multistand_key.md)
    (for multiple stands).

4.  Run FVS with the desired variant using
    [`run_FVS()`](https://sprachan.github.io/FVSIE-tools/reference/run_FVS.md).

5.  Retrieve outputs from the resulting output (a SQL .db file is the
    default, but .xlsx and .xls outputs may also be requested).

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
