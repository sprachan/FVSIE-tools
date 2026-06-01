#' Set tree columns to use default names and fill in default values
#'
#' @param tree_list Tree list to set names/values for.
#' @param quiet Boolean. Should defaults be set without messages (TRUE) or with
#'   messages (FALSE)?
#' @param map_habcode Boolean. Should PV Code/PV reference code pairs be mapped
#'   to FVS habitat type codes? Default TRUE. If this is turned off, PV codes
#'   will be coerced to integers, so plant association codes ('CXXXX') will be
#'   filled with NA and treated as default habitat type (260).
#' @param map_verbose Boolean. Should habitat type mapping be done verbosely?
#'   Default FALSE.
#'
#' @description Small differences in column naming conventions can cause FVS to
#'   fail due to "column not found" errors. This function searches input tree
#'   list for the necessary columns, with some alternative names possible. If
#'   necessary columns aren't found, it attempts to fill in default values. If
#'   the needed column is missing and there is no default value, this function
#'   errors. See [get_tree_columns()]
#'
#' @returns Tree list dataframe containing all required columns for FVS to run.
#'   Returned dataframe has at least 32 columns (the number of required columns)
#'   and the same number of rows as input.
#' @export
#'
set_tree_cols <- function(tree_list, map_habcode = TRUE, quiet = TRUE,
                          map_verbose = FALSE){
  col_info <- search_cols(tree_list, fvs_tree_cols)
  missing <- col_info |>
    dplyr::filter(is.na(.data$data_col), !.data$has_default)

  if(nrow(missing != 0)){
    stop('Missing required column(s) ',
         paste(missing$required_col, collapse = ', '),
         ' and no default values are available.')
  }

  df <- lapply(col_info$required_col, \(x) set_default_values(x,
                                                              col_info,
                                                              tree_list,
                                                              quiet)) |>
    setNames(col_info$required_col) |>
    as.data.frame()
  df$fvs.TREE_ID <- 1:nrow(df)

  if(map_habcode){
    df$FVS_HAB <- mapply(map_habcode,
                         df$PV_CODE, df$PV_REF_CODE, verbose = map_verbose) |>
      unname()
  }else{
    df$FVS_HAB <-suppressWarnings(as.integer(df$PV_CODE))
  }

  if(!quiet) message ('Done setting tree columns.')
  df
}

#' Set stand columns to use default names and values.
#'
#' @param stand_info Stand information dataframe to clean.
#' @param quiet Boolean. Should defaults be set without messages (TRUE) or with
#'   messages (FALSE)?
#' @param map_habcode Boolean. Should PV Code/PV reference code pairs be mapped
#'   to FVS habitat type codes? Default TRUE. If this is turned off, PV codes
#'   will be coerced to integers, so plant association codes ('CXXXX') will be
#'   filled with NA and treated as default habitat type (260).
#' @param map_verbose Boolean. Should habitat type mapping be done verbosely?
#'   Default FALSE.
#'
#' @returns Stand information data frame with all columns that FVS needs to
#'   run.This dataframe has at least 58 columns (# of required stand columns) and
#'   the same number of rows as the input.
#' @export

set_stand_cols <- function(stand_info, quiet = TRUE,
                           map_habcode = TRUE, map_verbose = FALSE){
  col_info <- search_cols(stand_info, fvs_stand_cols)
  missing <- col_info |>
    dplyr::filter(is.na(.data$data_col), !.data$has_default)

  if(nrow(missing != 0)){
    stop('Missing required column(s) ',
         paste(missing$required_col, collapse = ', '),
         ' and no default values are available.')
  }

  df <- lapply(col_info$required_col, \(x) set_default_values(x,
                                                              col_info,
                                                              stand_info,
                                                              quiet)) |>
    setNames(col_info$required_col) |>
    as.data.frame()

  if(map_habcode){
    df$FVS_HAB <- mapply(map_habcode,
                         df$PV_CODE, df$PV_REF_CODE, verbose = map_verbose) |>
      unname()
  }else{
    df$FVS_HAB <- suppressWarnings(as.integer(df$PV_CODE))
  }

  df$ELEVATION <- ifelse(is.na(df$ELEVATION), df$ELEV_FT/100, df$ELEVATION)

  df
}

