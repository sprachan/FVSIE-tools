# Fetch filtered FIA condition table for subsetting

`fetch_cond()` is a convenience function for fetching subsets of COND
tables for use with `get_FIA_state()`.

`get_FIA_state()` fetches FVS Stand and FVS Tree tables from a
downloaded state-level FIA database (from the [FIA datamart
website](https://apps.fs.usda.gov/fia/datamart/datamart.html)). Note
that these tables are only available at the state-level, so this
function only works with state-level databases.

## Usage

``` r
fetch_cond(db_loc, filter_statements = "")

get_FIA_state(
  database,
  fia_cond_subset,
  verbose = FALSE,
  add_identifiers = FALSE
)
```

## Arguments

- db_loc:

  Character string. Location for the FIA database.

- filter_statements:

  Character string of dplyr-style filter statements. If not provided,
  entire (large!) condition table will be returned. Column names in the
  filter_statements arguments must match columns in the COND table from
  the [NFI
  database](https://research.fs.usda.gov/sites/default/files/2025-08/wo-v9-4_Aug2025_UG_FIADB_database_description_NFI.pdf).
  See examples.

- database:

  Character string. Location for the FIA database.

- fia_cond_subset:

  Dataframe. A subset of an FIA COND table.

- verbose:

  Boolean value. If TRUE, will print SQL queries to console.

- add_identifiers:

  Boolean value. If TRUE, will add a PID (Plot IDentifier) column to the
  stand table and a TUID (Tree Unique IDentifier) column to the tree
  table. PID and TUID are unique, persistent identifiers. They uniquely
  identify each FIA plot and each tree in each plot. Unlike FIA-provided
  identifiers, these stay the same across all years.

## Value

`fetch_cond()`: COND data frame for use as a filter for
`get_FIA_state()`.

`get_FIA_state()`: List of 2. \$FVS_StandInit is a dataframe of the
stand information. \$FVS_TreeInit is a dataframe of all tree
measurements. A single stand from this list selected with STAND_CN and
the associated tree list (matching STAND_CN) can be passed to
[`run_FVS()`](https://sprachan.github.io/FVSIE-tools/reference/run_FVS.md).

## Examples

``` r

# get just the condition table:
database <- system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools')
fetch_cond(database, 'STATECD == 30, INVYR >= 2001')
#> # A tibble: 5 × 153
#>   CN            PLT_CN INVYR STATECD UNITCD COUNTYCD  PLOT CONDID COND_STATUS_CD
#>   <chr>         <chr>  <int>   <int>  <int>    <int> <int>  <int>          <int>
#> 1 303115670489… 18876…  2014      30      1       29 85318      1              1
#> 2 196394991020… 40395…  2012      30      4       43 82908      1              1
#> 3 196396140020… 40395…  2012      30      1       47 83061      1              1
#> 4 196408590020… 40395…  2012      30      3       63 89014      1              1
#> 5 196382142020… 40394…  2012      30      2        9 88715      1              1
#> # ℹ 144 more variables: COND_NONSAMPLE_REASN_CD <int>, RESERVCD <int>,
#> #   OWNCD <int>, OWNGRPCD <int>, ADFORCD <int>, FORTYPCD <int>, FLDTYPCD <int>,
#> #   MAPDEN <int>, STDAGE <int>, STDSZCD <int>, FLDSZCD <int>, SITECLCD <int>,
#> #   SICOND <int>, SIBASE <int>, SISP <int>, STDORGCD <int>, STDORGSP <dbl>,
#> #   PROP_BASIS <chr>, CONDPROP_UNADJ <dbl>, MICRPROP_UNADJ <dbl>,
#> #   SUBPPROP_UNADJ <dbl>, MACRPROP_UNADJ <dbl>, SLOPE <int>, ASPECT <int>,
#> #   PHYSCLCD <int>, GSSTKCD <int>, ALSTKCD <int>, DSTRBCD1 <int>, …

# if filtering by a character column, use "" to surround filter statement and
#> '' to surround the string:
fetch_cond(database, "CN == '303115670489998'")
#> # A tibble: 1 × 153
#>   CN            PLT_CN INVYR STATECD UNITCD COUNTYCD  PLOT CONDID COND_STATUS_CD
#>   <chr>         <chr>  <int>   <int>  <int>    <int> <int>  <int>          <int>
#> 1 303115670489… 18876…  2014      30      1       29 85318      1              1
#> # ℹ 144 more variables: COND_NONSAMPLE_REASN_CD <int>, RESERVCD <int>,
#> #   OWNCD <int>, OWNGRPCD <int>, ADFORCD <int>, FORTYPCD <int>, FLDTYPCD <int>,
#> #   MAPDEN <int>, STDAGE <int>, STDSZCD <int>, FLDSZCD <int>, SITECLCD <int>,
#> #   SICOND <int>, SIBASE <int>, SISP <int>, STDORGCD <int>, STDORGSP <dbl>,
#> #   PROP_BASIS <chr>, CONDPROP_UNADJ <dbl>, MICRPROP_UNADJ <dbl>,
#> #   SUBPPROP_UNADJ <dbl>, MACRPROP_UNADJ <dbl>, SLOPE <int>, ASPECT <int>,
#> #   PHYSCLCD <int>, GSSTKCD <int>, ALSTKCD <int>, DSTRBCD1 <int>, …

# fetch_cond() dataframe can go directly into get_FIA_state():
database <- system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools')
cond <- fetch_cond(database, 'STATECD == 30, INVYR >= 2001')
get_FIA_state(database, cond)
#> $FVS_StandInit
#> # A tibble: 5 × 35
#>   STAND_CN  STAND_ID VARIANT STATE INV_DAY INV_YEAR INV_MONTH LATITUDE LONGITUDE
#>   <chr>     <chr>    <chr>   <dbl>   <dbl>    <dbl>     <dbl>    <dbl>     <dbl>
#> 1 18876285… 3014029… IE         30      14     2015         8     47.8     -113.
#> 2 40395300… 3012043… EM         30      16     2013         8     46.0     -112.
#> 3 40395358… 3012047… IE         30      24     2013         9     47.9     -114.
#> 4 40395823… 3012063… IE         30      24     2012         5     46.9     -114.
#> 5 40394253… 3012009… EM         30      17     2013         8     45.1     -108.
#> # ℹ 26 more variables: REGION <dbl>, FOREST <dbl>, PV_CODE <chr>,
#> #   ECOREGION <chr>, BASAL_AREA_FACTOR <dbl>, INV_PLOT_SIZE <dbl>,
#> #   BRK_DBH <dbl>, AGE <dbl>, ASPECT <dbl>, SLOPE <dbl>, TOPO <chr>,
#> #   ELEVFT <dbl>, NUM_PLOTS <dbl>, MAX_SDI <dbl>, DG_TRANS <dbl>,
#> #   DG_MEASURE <dbl>, HTG_TRANS <dbl>, HTG_MEASURE <dbl>, MORT_MEASURE <dbl>,
#> #   SITE_SPECIES <chr>, SITE_INDEX <dbl>, PID <chr>, COUNTYCD <int>,
#> #   UNITCD <int>, PLOT <int>, OWNCD <int>
#> 
#> $FVS_TreeInit
#> # A tibble: 170 × 32
#>    STAND_CN     STAND_ID STANDPLOT_ID PLOT_ID PLOT_CN TREE_ID HISTORY TREE_COUNT
#>    <chr>        <chr>    <chr>          <dbl> <chr>     <dbl>   <dbl>      <dbl>
#>  1 18876285302… 3014029… 30140298531…       3 188762…       4       1          1
#>  2 18876285302… 3014029… 30140298531…       3 188762…       1       1          1
#>  3 18876285302… 3014029… 30140298531…       3 188762…       2       1          1
#>  4 18876285302… 3014029… 30140298531…       3 188762…       3       1          1
#>  5 18876285302… 3014029… 30140298531…       3 188762…       5       8          1
#>  6 18876285302… 3014029… 30140298531…       3 188762…       6       8          1
#>  7 18876285302… 3014029… 30140298531…       3 188762…       7       1          1
#>  8 18876285302… 3014029… 30140298531…       3 188762…       8       1          1
#>  9 18876285302… 3014029… 30140298531…       3 188762…      10       1          1
#> 10 18876285302… 3014029… 30140298531…       4 188762…       1       1          1
#> # ℹ 160 more rows
#> # ℹ 24 more variables: SPECIES <dbl>, DIAMETER <dbl>, DG <dbl>, HT <dbl>,
#> #   HTTOPK <dbl>, HTG <dbl>, HT_TO_CROWN_BASE <dbl>, CRRATIO <dbl>,
#> #   DEFECT_CUBIC <dbl>, DEFECT_BOARD <dbl>, DAMAGE1 <dbl>, SEVERITY1 <dbl>,
#> #   DAMAGE2 <dbl>, SEVERITY2 <dbl>, DAMAGE3 <dbl>, SEVERITY3 <dbl>, AGE <dbl>,
#> #   BH_YEARS <dbl>, PID <chr>, TUID <chr>, COUNTYCD <int>, UNITCD <int>,
#> #   PLOT <int>, OWNCD <int>
#> 
```
