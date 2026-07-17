#' Fetch filtered FIA condition table for subsetting
#'
#' `fetch_cond()` is a convenience function for fetching subsets of COND tables
#' for use with [get_FIA_state()].
#'
#' @param db_loc Character string. Location for the FIA database.
#' @param filter_statements Character string of dplyr-style filter statements.
#'   If not provided, entire (large!) condition table will be returned. Column
#'   names in the filter_statements arguments must match columns in the COND
#'   table from the [NFI
#'   database](https://research.fs.usda.gov/sites/default/files/2025-08/wo-v9-4_Aug2025_UG_FIADB_database_description_NFI.pdf).
#'   See examples.
#'
#' @returns `fetch_cond()`: COND data frame for use as a filter for
#'   `get_FIA_state()`.
#' @rdname get_FIA_state
#' @export
#'
#'
fetch_cond <- function(db_loc, filter_statements = ''){
  fia_db_conn <- DBI::dbConnect(RSQLite::SQLite(), db_loc)
  on.exit(DBI::dbDisconnect(fia_db_conn), add = TRUE)

  stopifnot('COND table not found in database.' = 'COND' %in% DBI::dbListTables(fia_db_conn),
            "Filter statement must be a character string or vector" = typeof(filter_statements) == 'character',
            length(filter_statements) == 1)
  if(nzchar(filter_statements)){
    filt <- stringr::str_replace_all(filter_statements, ',', ';')
    dplyr::tbl(fia_db_conn, 'COND') |>
      dplyr::collect() |>
      dplyr::filter(!!!rlang::parse_exprs(filt))
  }else{
    dplyr::tbl(fia_db_conn, 'COND') |>
      dplyr::collect()
  }

}
