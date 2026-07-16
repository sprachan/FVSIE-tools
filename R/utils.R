#' Helper function to check database validity for running FVS
#'
#' @param database Character string. File path to the SQLite input database.
#'
#' @returns Nothing. Errors if FVS_StandInit and/or FVS_TreeInit not found in
#'   the database.
#' @keywords internal
#' @noRd
check_db_run <- function(database){
  conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  stopifnot('Stand table must be named FVS_StandInit' =
              'FVS_StandInit' %in% DBI::dbListTables(conn),
            'Tree table must be named FVS_TreeInit' =
              'FVS_TreeInit' %in% DBI::dbListTables(conn))

  # ensure that database is disconnected regardless of error status
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
}

#' Helper function to check database validity for getting FIA data
#'
#' @param database Character string. File path to the SQLite input database.
#'
#' @returns Nothing. Errors if FVS_STANDINIT_PLOT and/or FVS_TREEINIT_PLOT not found in
#'   the database.
#' @keywords internal
#' @noRd
check_db_get <- function(database){
  conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  stopifnot('Stand table must be named FVS_STANDINIT_PLOT' =
              'FVS_STANDINIT_PLOT' %in% DBI::dbListTables(conn),
            'Tree table must be named FVS_TREEINIT_PLOT' =
              'FVS_TREEINIT_PLOT' %in% DBI::dbListTables(conn))

  # ensure that database is disconnected regardless of error status
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
}

#' Adjust inventory year to reflect the year that tree growth occurred in
#'
#' @param df Dataframe with `INV_YEAR` and `INV_MONTH` columns
#'
#' @returns Dataframe.
#' @keywords internal
#' @noRd
adjust_gy <- function(df){
  stopifnot(any(check_name(colnames(df), 'INV_YEAR')))
  if(any(check_name(colnames(df), 'INV_MONTH'))){
    df$INV_YEAR + as.integer(df$INV_MONTH >= 7)
    df
  }else{
    df$INV_YEAR
    df
  }
}

#' Filter a table by a remote table using an inner join
#'
#' @param conn SQLite connection that contains both the table to filter and the
#'   remote table to use as the filter via inner join.
#' @param table Character string naming the table to filter.
#' @param remote Character string naming the table to use as the filter.
#' @param verbose Boolean. If `TRUE`, the SQL query content is displayed on the
#'   console.
#' @param ... Additional parameters (e.g., `by`) to pass to `dplyr::inner_join`
#'
#' @returns Dataframe resulting from executing the inner join.
#'
#' @keywords internal
#' @noRd
filter_by_remote <- function(conn, table, remote, verbose = FALSE, ...){
  sql_Q <- dplyr::tbl(conn, table)|>
    dplyr::inner_join(dplyr::tbl(conn, remote), ...)
  if(verbose){
   rlang::inform(message = paste0('SQL query: ', dplyr::show_query(sql_Q)))
  }
  dplyr::collect(sql_Q)
}

#' Add persistent unique Plot IDentifier column to a stand table
#'
#' Plot Identifiers are made up of the FIA State Code, County Code, Unit Code,
#' and Plot Code. Whereas CNs vary by years, PIDs are identical across years,
#' allowing FIA plots to be easily tracked over time.
#'
#' @param stand Dataframe. Stand table.
#'
#' @returns Dataframe. Identifical to `stand` with one additional column, `PID`.
#' @keywords internal
#' @noRd
add_pid <- function(stand){
  stand_cols <- colnames(stand)
  stopifnot(all(c('STATE', 'COUNTYCD', 'UNITCD', 'PLOT') %in% stand_cols))
  stand$PID <- paste0(stand$STATE,
                      stringr::str_pad(stand$COUNTYCD, 3, pad = '0'),
                      stand$UNITCD,
                      stand$PLOT)
  stand
}

#' Add persistent Tree Unique IDentifier column to a tree list
#'
#' @param stand Dataframe. Stand table with a plot unique identifier column.
#' @param tree_list Dataframe.
#'
#' @returns Dataframe. Identifical to `tree_list` with three additional columns,
#'   `INV_YEAR`, `PID` and `TUID`
#' @keywords internal
#' @noRd
add_tuid <- function(stand, tree_list){
  tree_list |>
    dplyr::left_join(stand[c('PID', 'STAND_CN', 'INV_YEAR')],
                     by = dplyr::join_by('STAND_CN')) |>
    dplyr::mutate(TUID = paste0(.data$PID, .data$PLOT_ID, .data$TREE_ID))
}

#' A convenience function for column name-checking.
#'
#' @param name String to check
#' @param check_against Pattern to check against.
#'
#' @returns A single Boolean value.
#' @keywords internal
#' @noRd

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

#' Format a vector of READCOR* values
#'
#' @param readcorx Vector of READCORD, READCORH, or READCORR values.
#'
#' @returns Character vector, length `length(readcorx)`
#' @keywords internal
#' @noRd
format_readcorx <- function(readcorx){
  vapply(readcorx, FUN = \(x) sprintf('%10.2f', x),
                          FUN.VALUE = character(1),
                          USE.NAMES = FALSE)
}

