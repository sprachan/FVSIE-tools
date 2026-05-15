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

  df <- lapply(col_info$required_col, \(x) set_default(x, col_info, tree_list, quiet)) |>
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

  df <- lapply(col_info$required_col, \(x) set_default(x, col_info, stand_info, quiet)) |>
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
set_default <- function(req_var, col_info, user_df, quiet){
  # row has 2 columns: required_col and data_col, each a character
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

#' Search a user-supplied dataframe for necessary columns
#'
#' @param user_df User-supplied dataframe
#' @param col_df Dataframe containing column information (column name,
#'   alternative names, defaults)
#'
#' @returns 2-column dataframe. First column is the required column, second
#'   column is the matching column found in the data.
#' @keywords internal
#'
search_cols <- function(user_df, col_df){
  col_match <- character(length = length(col_df$col))
  for(col in seq_along(col_df$col)){
    against <- col_df$col[col]
    matches <- vapply(colnames(user_df),
                      FUN = \(x) check_name(x, check_against = against),
                      FUN.VALUE = logical(1))

    if(sum(matches) != 1){
      if(nchar(col_df$alt_col[col]) > 0){
        matches <- vapply(colnames(user_df),
                          FUN = \(x) check_name(x, check_against = col_df$alt_col[col]),
                          FUN.VALUE = logical(1))
      }else{
        # look for exact match if there are no possible alternatives
          matches <- colnames(user_df) == against
      }
    }
    col_match[col] <- ifelse(sum(matches) == 1,
                             colnames(user_df)[matches],
                             NA)
  }
  merge(data.frame(required_col = col_df$col, data_col = unname(col_match)),
        col_df, by.x = 'required_col', by.y = 'col')
}

#' A convenience function for column name-checking.
#'
#' @param name String to check
#' @param check_against Pattern to check against.
#'
#' @returns A single Boolean value.
#' @keywords internal

check_name <- function(name, check_against){
  stopifnot(!is.na(name), !is.na(check_against))
  # checking names is way easier after standardizing i.e., all same case,
  #> removing separator characters like '.' and '_', and replacing words that
  #> have common abbreviations or are interchangeable with one another
  clean_name <- toupper(name) |>
    gsub('[^[:alnum:]]', '', x = _) |>
    gsub('CD', 'CODE', x = _) |>
    gsub('CODE', '', x = _) |>
    gsub('CLASS', '', x = _)

  clean_against <- toupper(check_against) |>
    gsub('[^[:alnum:]]', '', x = _) |>
    gsub('CD', 'CODE', x = _) |>
    gsub('CODE', '', x = _) |>
    gsub('CLASS', '', x = _)

  clean_against == clean_name
}

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
