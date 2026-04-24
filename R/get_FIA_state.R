#' Get FVS-ready FIA data from a state-level database
#'
#' `get_FIA_state()` fetches FVS Stand and FVS Tree tables from a downloaded state-level FIA
#' database (from the [FIA datamart website](https://apps.fs.usda.gov/fia/datamart/datamart.html)). Note that these tables are only available at the state-level, so
#' this function only works with state-level databases.
#'
#' @param db_loc Character string. Location for the FIA database.
#' @param fia_cond_subset Dataframe. A subset of an FIA COND table.
#' @param verbose Boolean value. If TRUE, will print SQL queries to console.
#' @param add_identifiers Boolean value. If TRUE, will add a PID (Plot IDentifier) column to the stand table and a TUID (Tree Unique IDentifier) column to the tree table. PID and TUID are unique, persistent identifiers. They uniquely identify each FIA plot and each tree in each plot. Unlike FIA-provided identifiers, these stay the same across all years.
#'
#' @returns `get_FIA_state()`: List of 2. $FVS_StandInit is a dataframe of the stand information. $FVS_TreeInit is a dataframe of all tree measurements. A single stand from this list selected with STAND_CN and the associated tree list (matching STAND_CN) can be passed to `run_FVS()`.
#' @examples
#'
#' db_loc <- system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools')
#' cond <- fetch_cond(db_loc, 'STATECD == 30, INVYR >= 2001')
#' get_FIA_state(db_loc, cond)
#'
#' @export
get_FIA_state <- function(db_loc, fia_cond_subset, verbose = FALSE,
                          add_identifiers = FALSE){

  fia_db_conn <- DBI::dbConnect(RSQLite::SQLite(), db_loc)
  stopifnot(c('FVS_STANDINIT_PLOT', 'FVS_TREEINIT_PLOT') %in% DBI::dbListTables(fia_db_conn),
            is.data.frame(fia_cond_subset),
            all(c('PLT_CN', 'COUNTYCD', 'UNITCD', 'PLOT', 'OWNCD') %in% colnames(fia_cond_subset)))
  # from Gemini: safer way to make sure database is disconnected
  on.exit(DBI::dbDisconnect(fia_db_conn), add = TRUE)

  pcn_remote <- dplyr::copy_to(dest = fia_db_conn,
                               df = fia_cond_subset[c('PLT_CN',
                                                      'COUNTYCD',
                                                      'UNITCD',
                                                      'PLOT',
                                                      'OWNCD')],
                               name = 'temp_pcn',
                               overwrite = TRUE,
                               temporary = TRUE)
  if(!any(c('COUNTYCD', 'PLOT') %in% colnames(pcn_remote))){
    cat(sort(colnames(pcn_remote)), '\n',
        sort(colnames(dplyr::tbl(fia_db_conn, 'FVS_STANDINIT_PLOT'))))
    stop()
  }
  # construct and execute SQL query using tidyverse-style piping
  # from Gemini: use inner_join with temporary remote table. It's faster than
  #> filtering by a large vector.
  stand_initQ <- dplyr::tbl(fia_db_conn, 'FVS_STANDINIT_PLOT')|>
    dplyr::inner_join(pcn_remote, by = c('STAND_CN' = 'PLT_CN'))
  if(verbose){
    message('SQL query: ', dplyr::show_query(stand_initQ))
  }

  stand_init <- dplyr::collect(stand_initQ)

  # did we return anything?
  if(nrow(stand_init) == 0){
    warning('No matching stands found, returning NULL')
    return(NULL)
  }

  # adjust growth  year. If later than July, add 1 (b/c in next growth year)
  stand_init$INV_YEAR <- stand_init$INV_YEAR + as.integer(stand_init$INV_MONTH >= 7)
  # get tree info
  fia_treeQ <- dplyr::tbl(fia_db_conn, 'FVS_TREEINIT_PLOT') |>
    dplyr::inner_join(pcn_remote, by = c('STAND_CN' = 'PLT_CN'))
  if(verbose){
    message('SQL query to get tree info: ', dplyr::show_query(fia_treeQ))
  }
  fia_tree <- dplyr::collect(fia_treeQ)
  fia_tree$SPECIES <- as.numeric(fia_tree$SPECIES)

  if(nrow(fia_tree) == 0){
    warning('No matching tree info found, returning NULL')
    return(NULL)
  }

  if(add_identifiers){
    stand_init$PID <- paste0(stand_init$STATE,
                             stringr::str_pad(stand_init$COUNTYCD, 3, pad = '0'),
                             stand_init$UNITCD,
                             stand_init$PLOT)
    fia_tree <- fia_tree |>
      dplyr::left_join(stand_init[c('PID', 'STAND_CN', 'INV_YEAR')],
                       by = dplyr::join_by('STAND_CN')) |>
      dplyr::mutate(TUID = paste0(.data$PID, .data$PLOT_ID, .data$TREE_ID))
  }

  return(list(FVS_StandInit = stand_init, FVS_TreeInit = fia_tree))
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
  on.exit(DBI::dbDisconnect(fia_db_conn))

  stopifnot('COND table not found in database.' = 'COND' %in% DBI::dbListTables(fia_db_conn))
  stopifnot("Filter statement must be a character string or vector" = typeof(filter_statements) == 'character',
            length(filter_statements) == 1)
  filt <- stringr::str_replace_all(filter_statements, ',', ';')
  dplyr::tbl(fia_db_conn, 'COND') |>
    dplyr::collect() |>
    dplyr::filter(!!!rlang::parse_exprs(filt))
  }
