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

#' Rename a column to match the default name.
#'
#' @param req_var Character.
#' @param user_df Dataframe.
#'
#' @returns Dataframe, same dimensions as user_df and all data preserved.
#' @keywords internal
#'
rename_col <- function(req_var, user_df){
  idx <- which(check_name(name = colnames(user_df),
                   check_against = req_var))
  colnames(user_df)[idx] <- req_var
  user_df
}


check_cols <- function(user_df, col_df){
  col_info <- search_cols(user_df, col_df)
  missing <- col_info |>
    dplyr::filter(is.na(.data$data_col), !.data$has_default)
  if(nrow(missing != 0)){
    stop('Missing required column(s) ',
         paste(missing$required_col, collapse = ', '),
         ' and no default values are available.')
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
