#' Get a table of needed tree and/or stand list columns
#'
#'
#' @description [run_FVS()] and [run_FVS_parallel()] assumes that columns have
#'   certain names. These assumed names come from FIA's FVS_TREE_INIT and
#'   FVS_STAND_INIT tables, with the exception of `FVS_HAB` (the FVS habitat
#'   type associated with the PV Code/PV Ref code combination, or obtained by
#'   coercing PV code to an integer -- see [map_habcode()]). These functions
#'   show you the default names for required columns (`col`), their default
#'   value, if applicable (`default`; 9999 means so default), whether or not the
#'   column has a default (`has_default`), acceptable alternative column names
#'   (`alt_col`), and the required data type of the column (`dtype`).
#'
#' @returns A dataframe (32 rows x 5 columns for tree column table, 58 rows x 5
#'   columns for stand column table.).
#' @export
#'
#' @details From Essential FVS: A User's Guide to the Forest Vegetation
#'   Simulator (Dixon, updated March 2026), p. 47
#'
#' @examplesIf interactive()
#' View(get_tree_columns())
#'
#' @examples
#' print(get_tree_columns())
#' print(get_stand_columns())
#'
#' @seealso [set_tree_cols()], [set_stand_cols()]
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
