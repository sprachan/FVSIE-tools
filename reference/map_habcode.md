# Map PV/PV ref code pairs to associated FVS-IE habitat type

Note that this function is not (currently) vectorized, so applying to a
dataframe takes an extra step. See examples.

## Usage

``` r
map_habcode(pv_code, pv_ref, verbose = FALSE)
```

## Arguments

- pv_code:

  Potential vegetation code (scalar)

- pv_ref:

  Potential vegetation reference code (scalar)

- verbose:

  If TRUE, report when NAs are returned and why (no matches are found or
  PV ref code is blank). Default FALSE.

## Value

3-digit integer representing FVS-IE habitat type. See [FVS-IE
Overview](https://www.fs.usda.gov/sites/default/files/forest-management/fvs-ie-overview.pdf)

## Details

Note that this function relies on lookup tables created for internal
use. To see the source code for creating these tables, please see
<https://github.com/spracha/FVSIE-tools/data-raw/ref_tables.R>.

## Examples

``` r
# can be used for a single pair of values:
map_habcode('578', 110)
#> Error in map_habcode("578", 110): object 'pv_key' not found
# <int> 570

# more realistically, you'll want to apply to a whole dataset:
habs <- data.frame(pv_code = c('578', 'CES313', '10'),
                   pv_ref = c(110, 626, 110))

# dplyr-style:
habs |> dplyr::rowwise() |> dplyr::mutate(hab_cd = map_habcode(pv_code, pv_ref))
#> Error in dplyr::mutate(dplyr::rowwise(habs), hab_cd = map_habcode(pv_code,     pv_ref)): ℹ In argument: `hab_cd = map_habcode(pv_code, pv_ref)`.
#> ℹ In row 1.
#> Caused by error in `map_habcode()`:
#> ! object 'pv_key' not found
# A tibble: 3x3
# pv_code    pv_ref     hab_cd
# <chr>      <dbl>      <int>
# 578        110        570
# CES313     626        720
# 10         110        130

# Base R:

habs$hab_cd <- unname(mapply(map_habcode, pv_code = habs$pv_code, pv_ref = habs$pv_ref))
#> Error in (function (pv_code, pv_ref, verbose = FALSE) {    stopifnot(`pv_code and pv_ref must both be length 1 (not vectors)` = length(pv_code) ==         1, length(pv_ref) == 1)    pv_code <- fmt_pv(pv_code)    pv_ref <- fmt_pv(pv_ref)    match <- NA    if (grepl("C", pv_code)) {        match <- pa_lookup[trimws(pa_lookup$plant_assoc) == trimws(pv_code),             ]$fvs_hab[1]    }    else if (trimws(pv_ref) != "") {        hab <- pv_key[pv_code == pv_key$pv_code & pv_ref == pv_key$pv_ref,             ]$hab[1]        match <- hab_lookup[hab_lookup$ie_hab == as.integer(hab),             ]$fvs_hab    }    else {        if (verbose)             message("Blank PV ref code, returning NA")        match <- NA    }    if (verbose & is.null(match)) {        message("No matches found, returning NA")    }    ifelse(is.null(match), as.integer(NA), as.integer(match))})(pv_code = dots[[1L]][[1L]], pv_ref = dots[[2L]][[1L]]): object 'pv_key' not found
habs
#>   pv_code pv_ref
#> 1     578    110
#> 2  CES313    626
#> 3      10    110
# a dataframe
# pv_code    pv_ref     hab_cd
# <chr>      <dbl>      <int>
# 578        110        570
# CES313     626        720
# 10         110        130
```
