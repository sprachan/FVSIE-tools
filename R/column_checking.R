#' Set tree columns to use default names and values.
#'
#' @param tree_list Tree list to clean.
#' @param quiet Boolean. Should defaults be set without messages (TRUE) or with
#'   messages (FALSE)?
#'
#' @returns Tree list data frame containing all required columns for FVS to run.
#' @export
#'
set_tree_cols <- function(tree_list, quiet = TRUE){
  col_info <- search_cols(tree_list, fvs_tree_cols)
  missing <- col_info |>
    dplyr::filter(is.na(.data$data_col), !.data$has_default)
  if(nrow(missing) != 0){
    stop('Missing required column(s) ',
         paste(missing$required_col, collapse = ', '),
         ' and no default values are available.')
  }else{
    lapply(col_info$required_col, \(x) set_default(x, col_info, tree_list, quiet)) |>
      setNames(col_info$required_col) |>
      as.data.frame()
  }
}

#' Set stand columns to use default names and values.
#'
#' @param stand_info Stand information dataframe to clean.
#' @param quiet Boolean. Should defaults be set without messages (TRUE) or with
#'   messages (FALSE)?
#'
#' @returns Stand information data frame with all columns that FVS needs to run.
#' @export

set_stand_cols <- function(stand_info, quiet = TRUE){
  col_info <- search_cols(stand_info, fvs_stand_cols)
  missing <- col_info |>
    dplyr::filter(is.na(.data$data_col), !.data$has_default)
  if(nrow(missing) != 0){
    stop('Missing required column(s) ',
         paste(missing$required_col, collapse = ', '),
         ' and no default values are available.')
  }else{
    lapply(col_info$required_col, \(x) set_default(x, col_info, stand_info, quiet)) |>
      setNames(col_info$required_col) |>
      as.data.frame()
  }
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
  row <- col_info[col_info$required_col == req_var,]
  if(!is.na(row$data_col)){
    out <- unlist(unname(user_df[row$data_col]))
  }else{
    stopifnot(row$has_default)
    if(!quiet){
      message('Filling in default value, ', row$default,
              ', for column ', req_var)
    }
    # one default is 'IE', so everything else in the column is a string
    out <- row$default
  }
  if(typeof(out) != row$dtype){
    return(suppressWarnings(as.vector(out, mode = row$dtype)))
  }
  return(out)
}

#' Search a user-supplied dataframe for necessary columns
#'
#' TODO: refactor to use matrix multiplication for speed improvements.
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
    # some columns have well-defined alternative names that we'll want to check
    #> if the default name isn't found in the dataset column names.
    against <- col_df$col[col]
    matches <- vapply(colnames(user_df),
                      FUN = \(x) check_name(x, check_against = against),
                      FUN.VALUE = logical(1))

    if(nchar(col_df$alt_col[col]) > 0 & sum(matches) != 1){
      against <- col_df$alt_col[col]
      matches <- vapply(colnames(user_df),
                        FUN = \(x) check_name(x, check_against = against),
                        FUN.VALUE = logical(1))
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
    stringr::str_replace('[^[:alnum:]]', '') |>
    stringr::str_replace('CD', 'CODE') |>
    stringr::str_replace('CLASS', '') |>
    stringr::str_replace('CODE', '')
  clean_against <- toupper(check_against) |>
    stringr::str_replace('[^[:alnum:]]', '') |>
    stringr::str_replace('CD', 'CODE') |>
    stringr::str_replace('CODE', '') |>
    stringr::str_replace('CLASS', '')

  clean_against == clean_name
}

