#' Get FVS-ready FIA data from a state-level database
#'
#' `get_FVS_ready()` fetches FVS Stand and FVS Tree tables from a downloaded
#' state-level FIA database (from the [FIA datamart
#' website](https://apps.fs.usda.gov/fia/datamart/datamart.html)). Note that
#' these tables are only available at the state-level, so this function only
#' works with state-level databases.
#'
#' @param database Character string. Location for the FIA database.
#' @param fia_cond_subset Dataframe. A subset of an FIA COND table; will be used
#'   in an inner join to filter the FVS_StandInit and FVS_TreeInit tables.
#' @param type Character string, one of "PLOT" or "COND" (default "PLOT").
#'   Specifies whether FVS_<Stand/Tree>Init_PLOT or FVS_<Stand/Tree>Init_COND
#'   should be returned.
#' @param verbose Boolean value. If TRUE, will print SQL queries to console.
#' @param add_identifiers Boolean value. If TRUE, will add a PID (Plot
#'   IDentifier) column to the stand table and a TUID (Tree Unique IDentifier)
#'   column to the tree table. PID and TUID are unique, persistent identifiers.
#'   They uniquely identify each FIA plot and each tree in each plot. Unlike
#'   FIA-provided identifiers, these stay the same across all years.
#' @param to_db Boolean. If TRUE (default), will write the filtered dataframes
#'   to an FVS-ready database that can be passed to [write_FVS_key()] or
#'   [write_multistand_key()].
#' @param output_database Character string. External location for filtered data
#'   tables. Default is 'fvs_ready.db'
#'
#' @returns `get_FVS_ready()`: List of 2. $FVS_StandInit is a dataframe of the
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
#'
#' database <- system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools')
#' cond <- fetch_cond(database, 'STATECD == 30, INVYR >= 2001')
#' get_FVS_ready(database, cond, type = 'PLOT', to_db = FALSE)
#'
#'
#' @export
get_FVS_ready <- function(database, fia_cond_subset, type = c('PLOT', 'COND'),
                          verbose = FALSE, add_identifiers = FALSE,
                          to_db = TRUE, output_database = 'fvs_ready.db'){
  stopifnot(file.exists(database))
  on.exit(try(DBI::dbDisconnect(fia_db_conn)), add = TRUE)

  type <- rlang::arg_match(type)
  if(type == 'PLOT'){
    tables <- c('FVS_STANDINIT_PLOT', 'FVS_TREEINIT_PLOT')
    by <- dplyr::join_by('STAND_CN' == 'PLT_CN')

  }else{
    tables <- c('FVS_STANDINIT_COND', 'FVS_TREEINIT_COND')
    by <- dplyr::join_by('STAND_CN' == 'CN')
  }
  check_db_get(database, tables)
  fia_db_conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  dplyr::copy_to(dest = fia_db_conn,
                 df = fia_cond_subset[c('CN', 'PLT_CN')],
                 name = 'cond_filt',
                 overwrite = TRUE,
                 temporary = TRUE)

  stand_init <- filter_by_remote(fia_db_conn,
                                 table = tables[1],
                                 remote = 'cond_filt',
                                 by = by) |>
    adjust_gy()
  tree_init <- filter_by_remote(fia_db_conn,
                                table = tables[2],
                                remote = 'cond_filt',
                                by = by)
  stopifnot(is.data.frame(stand_init), is.data.frame(tree_init))
  # did we return anything?
  if(nrow(stand_init) == 0){
    rlang::inform('No matching stands found, returning NULL')
    return(NULL)
  }else if(nrow(tree_init) == 0){
    warning('No matching tree info found, returning NULL')
    return(NULL)
  }

  if(add_identifiers){
    stand_init <- add_pid(stand_init)
    tree_init <- add_tuid(stand_init, tree_init)
  }

  if(to_db){
    if(!file.exists(output_database)) file.create(output_database)
    conn <- DBI::dbConnect(RSQLite::SQLite(), output_database)
    DBI::dbWriteTable(conn, 'FVS_StandInit', stand_init, overwrite = TRUE)
    DBI::dbWriteTable(conn, 'FVS_TreeInit', tree_init, overwrite = TRUE)
    DBI::dbDisconnect(conn)
  }
  return(list(FVS_StandInit = stand_init, FVS_TreeInit = tree_init))
}
