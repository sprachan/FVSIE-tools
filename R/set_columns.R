#' Set tree columns to use default names and fill in default values
#'
#' @param tree_list Dataframe. Tree list to set names/values for.
#' @param stand_info (Optional) Dataframe. Stand information for the tree list.
#'   If supplied (recommended), used to fill missing values for SLOPE, ASPECT,
#'   PV_CODE, and PV_REF_CODE.
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
#'   errors. See [get_tree_columns()] for a list of all required FVS columns.
#'
#' @returns Tree list dataframe containing all required columns for FVS to run.
#'   Returned dataframe has at least 32 columns (the number of required columns)
#'   and the same number of rows as input.
#' @export
#'
set_tree_cols <- function(tree_list, stand_info = NULL,
                          map_habcode = TRUE, map_verbose = FALSE){

  # Look for a crown ratio column and rename to the default name, CR_RATIO
  if(!'CR_RATIO' %in% colnames(tree_list)){
    stopifnot("Couldn't find a crown ratio column, e.g., CR_RATIO" =
                any(check_name(colnames(tree_list), 'CR_RATIO')))
    idx <- which(check_name(name = colnames(tree_list),
                            check_against = 'CR_RATIO'))
    colnames(tree_list)[idx] <- 'CR_RATIO'
  }

  # use stand info first to maximize non-default information
  if(!is.null(stand_info)){
    tree_list <- fill_tree_list(tree_list, stand_info)
  }

  # then search for missing information
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
                                                              quiet = FALSE)) |>
    setNames(col_info$required_col) |>
    as.data.frame()
  df$fvs.TREE_ID <- seq_len(nrow(df))

  if(map_habcode){
    df$FVS_HAB <- mapply(map_habcode,
                         df$PV_CODE, df$PV_REF_CODE, verbose = map_verbose) |>
      unname()
    if(any(is.na(df$FVS_HAB))){
      warning('Filling in default habitat types for ', sum(is.na(df$FVS_HAB)),
              ' of ', nrow(df), ' trees. Supply stand_info if possible.')
      }
    df$FVS_HAB <- ifelse(is.na(df$FVS_HAB), 570, df$FVS_HAB)
  }else{
    df$FVS_HAB <- suppressWarnings(as.integer(df$PV_CODE))
  }

  # Retain unmatched input columns
  matched_cols <- col_info$data_col[!is.na(col_info$data_col)]
  extra_cols <- tree_list[, !colnames(tree_list) %in% matched_cols]
  df <- cbind(df, extra_cols)
  df
}

#' Set stand columns to use default names and values.
#'
#' @param stand_info Stand information dataframe to clean.
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

set_stand_cols <- function(stand_info,
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
                                                              quiet = FALSE)) |>
    setNames(col_info$required_col) |>
    as.data.frame()

  if(!'FVS_HAB' %in% colnames(stand_info)){
    if(map_habcode){
      df$FVS_HAB <- mapply(map_habcode,
                           df$PV_CODE, df$PV_REF_CODE, verbose = map_verbose) |>
        unname()
    }else{
      df$FVS_HAB <- suppressWarnings(as.integer(df$PV_CODE))
    }
  }

  df$ELEVATION <- ifelse(is.na(df$ELEVATION), df$ELEV_FT/100, df$ELEVATION)

  # Retain unmatched input columns
  matched_cols <- col_info$data_col[!is.na(col_info$data_col)]
  extra_cols <- stand_info[, !colnames(stand_info) %in% matched_cols]
  df <- cbind(df, extra_cols)
  df
}

#' Using a dataframe of requirements and defaults, set default values where needed.
#'
#' @param req_var Character. Required variable to set default for.
#' @param col_info Dataframe, from running search_cols().
#' @param user_df User-supplied data frame
#' @param quiet Boolean. Should defaults be set without messages (TRUE) or with
#'   messages (FALSE)?
#'
#' @returns Vector, length 1 if default filled in, length(nrow(data_col)) if no
#'   default needed.
#' @keywords internal
#'
set_default_values <- function(req_var, col_info, user_df, quiet){
  row <- col_info[col_info$required_col == req_var,]

  if(is.na(row$data_col)){
    stopifnot(row$has_default)
    if(!quiet) message('Filling in default value, ', row$default, ', for column ', req_var)
    out <- row$default
  }else{
    out <- unlist(unname(user_df[row$data_col]))
  }

  # one default is 'IE', so everything else in the column is a string
  if(typeof(out) != row$dtype){
    return(suppressWarnings(as.vector(out, mode = row$dtype)))
  }
  return(out)
}
