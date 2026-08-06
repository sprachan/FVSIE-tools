
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
    statements in `fetch_cond()` followed by `get_FVS_ready()`.

3.  Write a keyword file specifying simulation parameters using one of
    two built-in functions: `write_FVS_key()` (for a single stand) or
    `write_multistand_key()` (for multiple stands).

4.  Run FVS with the desired variant using `run_FVS()`.

5.  Retrieve outputs from the resulting SQL .db file.

## Worked Example

*Projecting carbon for all forested FIA plots in the Lolo National
Forest, Montana*

Suppose we want to run FVS and get 20-year carbon estimates for all 100%
forested FIA plots that lie in the Lolo National Forest in Western
Montana. We’ll project from the latest measurement to 20 years in the
future to avoid projecting the same plots forward multiple times.

After downloading state-level data from the FIA datamart to a folder
called ‘fia_data’, we would then filter the FIA state-level data by
condition to get 100% forested plots. Note that we set
`add_identifers = TRUE` in the call to `get_FVS_ready()`. This
automatically adds persistent unique plot and tree identifier columns
(PID and TUID, respectively) that allow easy tracking of a single plot
or tree over time.

``` r
library(rFVSIEtools)
library(dplyr)

# Filter FIA-ready data into an FVS-ready database

database <- file.path('fia_data', 'SQLite_FIADB_MT.db')
cond <- fetch_cond(database, "INVYR >= 2001, COND_STATUS_CD == 1, CONDPROP_UNADJ == 1")
state_data <- get_FVS_ready(database, cond, 
                             output_database = file.path('fia_data', 'fvs_ready.db'),
                             add_identifiers = TRUE) 
#> automatically writes to fia_data/fvs_ready.db

state_data$FVS_StandInit
```

    # A tibble: 6,376 × 84
       STAND_CN  STAND_ID VARIANT INV_DAY INV_YEAR INV_MONTH GROUPS ADDFILES FVSKEYWORDS GIS_LINK LATITUDE
       <chr>     <chr>    <chr>     <dbl>    <dbl>     <dbl> <chr>  <chr>    <chr>       <chr>       <dbl>
     1 23202000… 3003029… IE           15     2003         7 All_F… NA       NA          NA           48.0
     2 23202340… 3003029… IE           20     2003        10 All_F… NA       NA          NA           48.3
     3 23202890… 3003029… IE            4     2003         6 All_F… NA       NA          NA           48.0
     4 31762375… 3003029… IE           10     2003         9 All_F… NA       NA          NA           48.7
     5 23204140… 3003029… IE           15     2003         7 All_F… NA       NA          NA           48.0
     6 23205090… 3003029… IE           15     2003        10 All_F… NA       NA          NA           47.7
     7 23205270… 3003029… IE           22     2003        10 All_F… NA       NA          NA           48.6
     8 23205730… 3003029… IE            9     2003         9 All_F… NA       NA          NA           48.3
     9 23205940… 3003029… IE           21     2003         7 All_F… NA       NA          NA           48.2
    10 23207720… 3003029… IE           31     2003         7 All_F… NA       NA          NA           48.5
    # ℹ 6,366 more rows
    # ℹ 73 more variables: LONGITUDE <dbl>, DATUM <chr>, REGION <dbl>, FOREST <dbl>, DISTRICT <dbl>,
    #   COMPARTMENT <dbl>, LOCATION <dbl>, ECOREGION <chr>, PV_CODE <chr>, PV_REF_CODE <dbl>,
    #   PV_FIA_HABTYPCD1 <chr>, AGE <dbl>, ASPECT <dbl>, SLOPE <dbl>, ELEVATION <dbl>, ELEVFT <dbl>,
    #   TOPO <chr>, BASAL_AREA_FACTOR <dbl>, INV_PLOT_SIZE <dbl>, BRK_DBH <dbl>, NUM_PLOTS <dbl>,
    #   NONSTK_PLOTS <dbl>, SAM_WT <dbl>, STK_PCNT <dbl>, DG_TRANS <dbl>, DG_MEASURE <dbl>,
    #   HTG_TRANS <dbl>, HTG_MEASURE <dbl>, MORT_MEASURE <dbl>, MAX_BA <dbl>, MAX_SDI <dbl>, …
    # ℹ Use `print(n = ...)` to see more rows

The next step is to filter the Stand_Init dataframe so that we only have
plots in the Lolo National Forest and so that each FIA plot is
represented only by its most recent measurement. For example, if a plot
was measured under the new protocol first in 2009 and then again in
2019, we would want to project from 2019 to 2039 only. This can easily
be done with tools from `dplyr`, though it may be done in base R as
well.

> Note: the `|>` operator is called the “pipe”. It feeds the output of
> the left-hand side into the first argument of the right-hand side
> operation. See R documentation:
> <https://stat.ethz.ch/R-manual/R-devel/library/base/html/pipeOp.html>

``` r
stand_ids <- state_data$FVS_StandInit |>
  group_by(PID)
  filter(INV_YEAR == max(INV_YEAR), # take the biggest inventory year for each plot
         LOCATION == 116) |>
         pull(STAND_ID) # get the STAND_ID column as a vector
```

To free up some RAM ahead of running FVS, we’ll remove the large list
created by `get_FIA_state()` and the intermediate dataframe of filtered
stands. This step is optional.

``` r
rm(state_data, stands)
```

Now, we’ll write keyword files. We tell FVS that the relevant data is in
the database output from our earlier `get_FIA_state` call, which is
`fia_data/fvs_ready.db`. We also specify the stands we wish to run using
the `STDIDENTs` argument as well as the number of years in the
simulation. We also request the carbon report.

``` r
lolo_key <- write_multistand_key(STDIDENTs = stand_ids,
                                out_dir = 'fia_data', 
                                file_prefix = 'lolo_kwd',
                                database = file.path('fia_data', 'fvs_ready.db'),
                                n_years = 21, carbon_report = TRUE)                      
```

The final step is to run FVS using the specified keywords. This will
take a bit of because there are 500 stands to run.

``` r
fvs_bin <- 'C:/FVS/FVSSoftware/FVSbin'
run_FVS(fvs_bin = fvs_bin, variant = 'ie', keyword_file = lolo_key)
```

The simulation results are in the database FVSOut.db. The list of
available database tables can be viewed with:

``` r
conn <- DBI::dbConnect(RSQLite::SQLite(), 'fia_data/FVSOut.db')
DBI::dbListTables(conn)
#> [1] "FVS_CalibStats" "FVS_Carbon"     "FVS_Cases"      "FVS_Error"      "FVS_Hrv_Carbon"
#> [6] "FVS_TreeList"  
DBI::dbDisconnect(conn)
```

The FVS_Error table is always worth inspecting if present, using code
like:

``` r
conn <- DBI::dbConnect(RSQLite::SQLite(), 'fia_data/FVSOut.db')
fvs_errs <- tbl(conn, 'FVS_Error') |>
  collect()
DBI::dbDisconnect(conn)
```

In this case, we have some stands with very high density (from saplings)
or higher stocking than FVS expects, some stands where FVS counts the
number of plots incorrectly and relies on the data in the database, some
stands with no projectable records (i.e., all the trees are dead), and
one stand where the potential vegetation/potential vegetation reference
combination was unrecognized.

After inspecting the error table, carbon results can be pulled into R
with a few lines of code:

``` r
conn <- DBI::dbConnect(RSQLite::SQLite(), 'FVSOut.db')
carbon <- tbl(conn, 'FVS_Carbon') |>
  collect()
DBI::dbDisconnect(conn)
```

And the `dplyr::tbl(conn, <table_name>) |> dplyr::collect()` pipeline
will work to pull any of the listed tables into R provided that the FVS
output database is connected.

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

This package is still in development. See
<https://github.com/sprachan/FVSIE-tools/issues>
