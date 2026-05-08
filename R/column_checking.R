#' Search a user-supplied dataframe for necessary columns
#'
#' @param user_df User-supplied dataframe
#' @param col_df Dataframe containing column information (column name, alternative names, defaults)
#'
#' @returns 2-column dataframe. First column is the required column, second column is the matching column found in the data.
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
  data.frame(required_col = col_df$col, data_col = unname(col_match))
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

