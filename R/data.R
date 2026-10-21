#' Example Stand Information Table
#'
#' A subset of FIA data for just one plot in western Montana. These data come
#' from the FVS_STANDINIT_PLOT table in the MT database and have been processed
#' using [get_FVS_ready()] with the option "add_identifiers" set to TRUE.
#'
#' @format ## `ex_stand`
#' A data frame with 1 rows and 34 columns.
"ex_stand"

#' Example Tree list
#'
#' A subset of FIA data for just one plot in western Montana. These data come
#' from the FVS_TREEINIT_PLOT table and have been processed using
#' [get_FVS_ready()] with the option "add_identifiers" set to TRUE.
#'
#' @format ## `ex_trees`
#' A data frame with 51 rows and 31 columns.
"ex_trees"


#' Bark ratio coefficients
#'
#' R dataframe of bark ratio coefficients.
#' See FVS-IE variant overview table 4.2.1 and ie/bratio.f in FVS source code.
#'
#' @format ## `bark_ratios`
#' A data frame with 23 rows and 3 columns.
"bark_ratios"
