#' Get FVS-ready FIA data from a state-level database
#'
#' `get_FIA_state()` fetches FVS Stand and FVS Tree tables from a downloaded
#' state-level FIA database (from the [FIA datamart
#' website](https://apps.fs.usda.gov/fia/datamart/datamart.html)). Note that
#' these tables are only available at the state-level, so this function only
#' works with state-level databases.
#'
#' @param database Character string. Location for the FIA database.
#' @param fia_cond_subset Dataframe. A subset of an FIA COND table.
#' @param verbose Boolean value. If TRUE, will print SQL queries to console.
#' @param add_identifiers Boolean value. If TRUE, will add a PID (Plot
#'   IDentifier) column to the stand table and a TUID (Tree Unique IDentifier)
#'   column to the tree table. PID and TUID are unique, persistent identifiers.
#'   They uniquely identify each FIA plot and each tree in each plot. Unlike
#'   FIA-provided identifiers, these stay the same across all years.
#' @param output_database Character string. Location for filtered data tables.
#'   Default is 'fvs_ready.db'
#'
#' @returns `get_FIA_state()`: List of 2. $FVS_StandInit is a dataframe of the
#'   stand information. $FVS_TreeInit is a dataframe of all tree measurements. A
#'   single stand from this list selected with STAND_CN and the associated tree
#'   list (matching STAND_CN) can be passed to `run_FVS()`.
#' @examples
#'
#' # get just the condition table:
#' database <- system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools')
#' fetch_cond(database, 'STATECD == 30, INVYR >= 2001')
#'
#' # if filtering by a character column, use "" to surround filter statement and
#' #> '' to surround the string:
#' fetch_cond(database, "CN == '303115670489998'")
#'
#' # fetch_cond() dataframe can go directly into get_FIA_state():
#' database <- system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools')
#' cond <- fetch_cond(database, 'STATECD == 30, INVYR >= 2001')
#' get_FIA_state(database, cond)
#'
#' @export
get_FIA_state <- function(database, fia_cond_subset, verbose = FALSE,
                          add_identifiers = FALSE, output_database = 'fvs_ready.db'){
  stopifnot(file.exists(database))
  on.exit(try(DBI::dbDisconnect(fia_db_conn)), add = TRUE)
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

  if(!file.exists(output_database)){file.create(output_database)}
  conn <- DBI::dbConnect(RSQLite::SQLite(), output_database)
  DBI::dbWriteTable(conn, 'FVS_StandInit', stand_init, overwrite = TRUE)
  DBI::dbWriteTable(conn, 'FVS_TreeInit', tree_init, overwrite = TRUE)
  DBI::dbDisconnect(conn)
  return(list(FVS_StandInit = stand_init, FVS_TreeInit = tree_init))
}
