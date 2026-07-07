#' Get a table of needed tree and/or stand list columns
#'
#'
#' @description These functions show you the default names for required columns
#'   (`col`), their default value, if applicable (`default`; 9999 means so
#'   default), whether or not the column has a default (`has_default`),
#'   acceptable alternative column names (`alt_col`), and the required data type
#'   of the column (`dtype`).
#'
#' @returns A dataframe (32 rows x 5 columns for tree column table, 58 rows x 5
#'   columns for stand column table.).
#' @export
#'
#' @details From Essential FVS: A User's Guide to the Forest Vegetation
#'   Simulator (Dixon, updated March 2026), p. 47
#'
#'
#' @examples
#' print(get_tree_columns())
#' print(get_stand_columns())
#'
#'
#' @rdname needed_columns

get_tree_columns <- function(){
  fvs_tree_cols
}

#' @rdname needed_columns
#' @export

get_stand_columns <- function(){
  fvs_stand_cols
}
