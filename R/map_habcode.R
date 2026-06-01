#' Map PV/PV ref code pairs to associated FVS-IE habitat type
#'
#' @description Note that this function is not (currently) vectorized, so
#'   applying to a dataframe takes an extra step. See examples.
#'
#' @param pv_code Potential vegetation code (scalar)
#' @param pv_ref Potential vegetation reference code (scalar)
#' @param verbose If TRUE, report when NAs are returned and why (no matches are
#'   found or PV ref code is blank). Default FALSE.
#'
#' @returns 3-digit integer representing FVS-IE habitat type. See
#'   \href{https://www.fs.usda.gov/sites/default/files/forest-management/fvs-ie-overview.pdf}{FVS-IE
#'   Overview}
#'
#' @details Note that this function relies on lookup tables created for internal
#'   use. To see the source code for creating these tables, please see
#'   <https://github.com/spracha/FVSIE-tools/data-raw/ref_tables.R>.
#'
#' @export
#'
#' @examples
#' # can be used for a single pair of values:
#' map_habcode('578', 110)
#' # <int> 570
#'
#' # more realistically, you'll want to apply to a whole dataset:
#' habs <- data.frame(pv_code = c('578', 'CES313', '10'),
#'                    pv_ref = c(110, 626, 110))
#'
#' # dplyr-style:
#' habs |> dplyr::rowwise() |> dplyr::mutate(hab_cd = map_habcode(pv_code, pv_ref))
#' # A tibble: 3x3
#' # pv_code    pv_ref     hab_cd
#' # <chr>      <dbl>      <int>
#' # 578        110        570
#' # CES313     626        720
#' # 10         110        130
#'
#' # Base R:
#'
#' habs$hab_cd <- unname(mapply(map_habcode, pv_code = habs$pv_code, pv_ref = habs$pv_ref))
#' habs
#' # a dataframe
#' # pv_code    pv_ref     hab_cd
#' # <chr>      <dbl>      <int>
#' # 578        110        570
#' # CES313     626        720
#' # 10         110        130

map_habcode <- function(pv_code, pv_ref, verbose = FALSE){
  stopifnot('pv_code and pv_ref must both be length 1 (not vectors)' =
              length(pv_code) == 1, length(pv_ref) == 1)
  # PV code format has to match lookup table
  pv_code <- fmt_pv(pv_code)
  pv_ref <- fmt_pv(pv_ref)

  match <- NA
  if(grepl('C', pv_code)){
    match <- pa_lookup[trimws(pa_lookup$plant_assoc) == trimws(pv_code),]$fvs_hab[1]
  }else if(trimws(pv_ref) != ''){
    hab <- pv_key[pv_code == pv_key$pv_code & pv_ref == pv_key$pv_ref,]$hab[1]
    match <- hab_lookup[hab_lookup$ie_hab == as.integer(hab),]$fvs_hab
  }else{
    if(verbose) message('Blank PV ref code, returning NA')
    match <- NA
  }

  if(verbose&is.null(match)){
    message('No matches found, returning NA')
  }
  ifelse(is.null(match), as.integer(NA), as.integer(match))
}

#' Format PV codes to match lookup tables
#'
#' @param pv_code
#'
#' @returns Character string of 10 characters.
#' @keywords internal
#'

fmt_pv <- function(pv_code){
  dplyr::if_else(nchar(trimws(pv_code)) == 2,
                 true = paste0('0', pv_code),
                 false = as.character(pv_code),
                 missing = '') |>
    trimws() |>
    stringr::str_pad(width = 10, side = 'right')
}
