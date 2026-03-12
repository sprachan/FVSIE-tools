#' Get FVS-ready FIA data from a state-level database
#'
#' `get_FIA_state()` fetches FVS Stand and FVS Tree tables from a downloaded FIA
#' database. Note that these tables are only available at the state-level, so
#' this function only works with state-level databases.
#'
#' @param db_loc Location for the FIA database.
#' @param fia_cond_subset Dataframe. A subset of an FIA COND table.
#' @param verbose Boolean value. If TRUE, will print SQL queries to console.
#'
#' @returns List of 2. FVS_StandInit is the stand table that can be used for
#' @examples
#'
#' db_loc <- system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools')
#' con <- DBI::dbConnect(RSQLite::SQLite(), db_loc)
#' cond <- dplyr::tbl(con, 'COND') |>
#'         dplyr::filter(STATECD == 30) |>
#'         dplyr::collect()
#' DBI::dbDisconnect(con)
#' get_FIA_state(db_loc, cond)
#'
#' @export
get_FIA_state <- function(db_loc, fia_cond_subset, verbose = FALSE){

  fia_db_conn <- DBI::dbConnect(RSQLite::SQLite(), db_loc)
  stopifnot(c('FVS_STANDINIT_PLOT', 'FVS_TREEINIT_PLOT') %in% DBI::dbListTables(fia_db_conn),
            is.data.frame(fia_cond_subset))

  # from Gemini: safer way to make sure database is disconnected
  on.exit(DBI::dbDisconnect(fia_db_conn), add = TRUE)

  # from Gemini: use a temporary table in SQLite instead of massive 'IN'
  #> clause to improve performance.

  pcn_remote <- dplyr::copy_to(dest = fia_db_conn,
                               df = fia_cond_subset[c('PLT_CN', 'COUNTYCD', 'UNITCD')],
                               name = 'temp_pcn',
                               overwrite = TRUE,
                               temporary = TRUE)
  if(!('COUNTYCD' %in% colnames(pcn_remote))){
    cat(sort(colnames(pcn_remote)), '\n',
        sort(colnames(dplyr::tbl(fia_db_conn, 'FVS_STANDINIT_PLOT'))))
    stop()
  }
  # construct and execute SQL query using tidyverse-style piping
  # from Gemini: use inner_join with temporary remote table. It's faster than
  #> filtering by a large vector.
  stand_initQ <- dplyr::tbl(fia_db_conn, 'FVS_STANDINIT_PLOT')|>
    dplyr::inner_join(pcn_remote, by = c('STAND_CN' = 'PLT_CN')) |>
    dplyr::select(.data$STAND_CN, .data$STAND_ID,
                  .data$VARIANT,
                  .data$STATE, .data$COUNTYCD, .data$UNITCD, .data$PLOT,
                  .data$INV_DAY, .data$INV_YEAR, .data$INV_MONTH,
                  .data$LATITUDE, .data$LONGITUDE, .data$REGION,
                  .data$FOREST, .data$PV_CODE, .data$ECOREGION,
                  .data$BASAL_AREA_FACTOR, .data$INV_PLOT_SIZE,
                  .data$BRK_DBH,
                  .data$AGE,
                  .data$ASPECT, .data$SLOPE, .data$TOPO, .data$ELEVFT,
                  .data$NUM_PLOTS,
                  .data$MAX_SDI,
                  .data$DG_TRANS, .data$DG_MEASURE,
                  .data$HTG_TRANS, .data$HTG_MEASURE,
                  .data$MORT_MEASURE,
                  .data$SITE_SPECIES, .data$SITE_INDEX)
  if(verbose){
    message('SQL query:', dplyr::show_query(stand_initQ))
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
    dplyr::inner_join(pcn_remote, by = c('STAND_CN' = 'PLT_CN')) |>
    dplyr::select(.data$STAND_CN, .data$STAND_ID, .data$STANDPLOT_ID,
                  .data$PLOT_ID, .data$PLOT_CN,
                  .data$TREE_ID, .data$HISTORY, .data$TREE_COUNT,
                  .data$SPECIES,
                  .data$DIAMETER, .data$DG,
                  .data$HT, .data$HTTOPK, .data$HTG, .data$HT_TO_CROWN_BASE,
                  .data$CRRATIO,
                  .data$DEFECT_CUBIC, .data$DEFECT_BOARD,
                  .data$DAMAGE1, .data$SEVERITY1, .data$DAMAGE2, .data$SEVERITY2,
                  .data$DAMAGE3, .data$SEVERITY3,
                  .data$AGE,
                  .data$BH_YEARS)
  if(verbose){
    message('SQL query to get tree info: ', dplyr::show_query(fia_treeQ))
  }
  fia_tree <- dplyr::collect(fia_treeQ)
  fia_tree$SPECIES <- as.numeric(fia_tree$SPECIES)

  if(nrow(fia_tree) == 0){
    warning('No matching tree info found, returning NULL')
    return(NULL)
  }

  return(list(FVS_StandInit = stand_init, FVS_TreeInit = fia_tree))
}
