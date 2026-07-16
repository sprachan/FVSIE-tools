#' Get FVS-ready FIA data from a state-level database
#'
#' `get_FIA_state()` fetches FVS Stand and FVS Tree tables from a downloaded state-level FIA
#' database (from the [FIA datamart website](https://apps.fs.usda.gov/fia/datamart/datamart.html)). Note that these tables are only available at the state-level, so
#' this function only works with state-level databases.
#'
#' @param database Character string. Location for the FIA database.
#' @param fia_cond_subset Dataframe. A subset of an FIA COND table.
#' @param verbose Boolean value. If TRUE, will print SQL queries to console.
#' @param add_identifiers Boolean value. If TRUE, will add a PID (Plot IDentifier) column to the stand table and a TUID (Tree Unique IDentifier) column to the tree table. PID and TUID are unique, persistent identifiers. They uniquely identify each FIA plot and each tree in each plot. Unlike FIA-provided identifiers, these stay the same across all years.
#'
#' @returns `get_FIA_state()`: List of 2. $FVS_StandInit is a dataframe of the stand information. $FVS_TreeInit is a dataframe of all tree measurements. A single stand from this list selected with STAND_CN and the associated tree list (matching STAND_CN) can be passed to `run_FVS()`.
#' @examples
#'
#' database <- system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools')
#' cond <- fetch_cond(database, 'STATECD == 30, INVYR >= 2001')
#' get_FIA_state(database, cond)
#'
#' @export
get_FIA_state <- function(database, fia_cond_subset, verbose = FALSE,
                          add_identifiers = FALSE){
  on.exit(DBI::dbDisconnect(fia_db_conn), add = TRUE)
  check_db_get(database)
  fia_db_conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  pcn_remote <- dplyr::copy_to(dest = fia_db_conn,
                               df = fia_cond_subset[c('PLT_CN',
                                                      'COUNTYCD',
                                                      'UNITCD',
                                                      'PLOT',
                                                      'OWNCD')],
                               name = 'temp_pcn',
                               overwrite = TRUE,
                               temporary = TRUE)

  stand_init <- filter_by_remote(fia_db_conn,
                                 table = 'FVS_STANDINIT_PLOT',
                                 remote = 'temp_pcn',
                                 by = c('STAND_CN' = 'PLT_CN')) |>
    adjust_gy()
  tree_init <- filter_by_remote(fia_db_conn,
                                table = 'FVS_TREEINIT_PLOT',
                                remote = 'temp_pcn',
                                by = c('STAND_CN' = 'PLT_CN'))
  stopifnot(is.data.frame(stand_init), is.data.frame(tree_init))
  # did we return anything?
  if(nrow(stand_init) == 0){
    rlang::inform('No matching stands found, returning NULL')
    return(NULL)
  }
  if(nrow(tree_init) == 0){
    warning('No matching tree info found, returning NULL')
    return(NULL)
  }

  if(add_identifiers){
    stand_init <- add_pid(stand_init)
    tree_init <- add_tuid(stand_init, tree_init)
  }

  return(list(FVS_StandInit = stand_init, FVS_TreeInit = tree_init))
}

#' Fetch filtered FIA condition table for subsetting
#'
#' `fetch_cond()` is a convenience function for fetching subsets of COND tables for use with [get_FIA_state()].
#'
#' @param db_loc Character string. Location for the FIA database.
#' @param filter_statements Character string of dplyr-style filter statements.
#'   Column names in the filter_statements arguments must match columns in the
#'   COND table from the [NFI
#'   database](https://research.fs.usda.gov/sites/default/files/2025-08/wo-v9-4_Aug2025_UG_FIADB_database_description_NFI.pdf).
#'   See examples.
#'
#' @returns `fetch_cond()`: COND data frame for use as a filter for `get_FIA_state()`.
#' @rdname get_FIA_state
#' @export
#'
#'
fetch_cond <- function(db_loc, filter_statements){
  fia_db_conn <- DBI::dbConnect(RSQLite::SQLite(), db_loc)
  on.exit(DBI::dbDisconnect(fia_db_conn), add = TRUE)

  stopifnot('COND table not found in database.' = 'COND' %in% DBI::dbListTables(fia_db_conn),
            "Filter statement must be a character string or vector" = typeof(filter_statements) == 'character',
            length(filter_statements) == 1)
  filt <- stringr::str_replace_all(filter_statements, ',', ';')
  dplyr::tbl(fia_db_conn, 'COND') |>
    dplyr::collect() |>
    dplyr::filter(!!!rlang::parse_exprs(filt))
  }
