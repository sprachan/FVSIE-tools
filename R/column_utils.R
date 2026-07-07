#' Check for required columns
#'
#' @param user_df User-supplied dataframe.
#' @param col_df Dataframe of columns to check against.
#'
#' @keywords internal
#' @returns Error if required columns are missing with no defaults and warns if
#'   there are missing columns that do have default values.
#'
check_cols <- function(user_df, col_df){
  col_info <- search_cols(user_df, col_df)
  missing <- col_info |>
    dplyr::filter(is.na(.data$data_col))
  missing_no_def <- dplyr::filter(missing, !.data$has_default)

  if(nrow(missing_no_def != 0)){
    rlang::abort(message = paste('Missing required column(s) ',
                                 paste(missing_no_def$required_col, collapse = ', '),
                                 ' and no default values are available.'),
                 class = 'fvs_err_no_def')
  }else if(nrow(missing) != 0){
    rlang::warn(message = paste('Missing required column(s) ',
                                paste(missing$required_col, collapse = ', '),
                                ', but defaults may be available.',
                                '\n See get_stand_columns() and set_stand_cols().'),
                class = 'fvs_err_def_avail')

  }
}

#' Search a user-supplied dataframe for necessary columns
#'
#' @param user_df User-supplied dataframe
#' @param col_df Dataframe containing column information (column name,
#'   alternative names, defaults)
#'
#' @returns 6-column dataframe:
#'  *required_col*: Character. Name of required column from col_df.
#'  *data_col*: Character. Name of the matching column found in user_df.
#'  *default*: Character. Default value for the column, 9999 if no default is available.
#'  *has_default*: Boolean. Does the column have a default value?
#'  *dtype*: Character. FVS required datatype for the default value.
#'
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
