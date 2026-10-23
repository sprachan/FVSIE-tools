#' Get a dataframe of FVS errors/warnings associated with each stand
#'
#' @param out_dir Character string. Location of the output database. Defaults to
#'   working directory.
#' @param out_db Character string. Name of the FVS output database. Defaults to
#'   'FVSOut.db', matching the default in [write_FVS_key()].
#' @param stand_df Dataframe. Stand information for stands run through FVS.
#'   Errors will only be reported for stands that appear in this dataframe.
#'   Requires a column named "STAND_ID" (matching STDIDENT passed to
#'   write_FVS_files).
#'
#' @returns Dataframe. Columns: Stand ID; any additional stand information that
#'   is in `stand_df`; `warncd`, the FVS warning code that can be looked up in
#'   FVS documentation; `warn_type`, which categorizes FVS warning codes into
#'   'Keyword' warnings (codes 1, 4, or 16), 'Forest Bounds' warnings (code 3),
#'   'Insufficient Trees' warnings (code 8), 'Plot Counts' (code 9), 'Hab Type'
#'   (short for habitat type; code 14, 23, and any between 32-35), 'High tree
#'   count' (code 40), 'Stocking' (code 41), and 'Other' (any other codes).
#' @export
#'

get_FVS_messages <- function(out_dir = getwd(), out_db, stand_df){
  stopifnot('STAND_ID' %in% colnames(stand_df))
  out_conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, out_db))
  if('FVS_Error' %in% DBI::dbListTables(out_conn)){
    errs <- dplyr::tbl(out_conn, 'FVS_Error') |>
      dplyr::collect() |>
      dplyr::mutate(warncd = as.integer(substr(.data$Message, 4, 5)),
                    warn_type = dplyr::case_when(.data$warncd == 1|.data$warncd == 4|.data$warncd == 16 ~ 'Keyword',
                                          .data$warncd == 3 ~ 'Forest Bounds',
                                          .data$warncd == 8 ~ 'Insufficient Trees',
                                          .data$warncd == 9 ~ 'Plot Counts',
                                          .data$warncd == 14|.data$warncd == 23|(.data$warncd>31&.data$warncd<36) ~ 'Hab Type',
                                          .data$warncd == 40 ~ 'High Tree Count',
                                          .data$warncd == 41 ~ 'Stocking',
                                          .default = 'Other')) |>
      dplyr::inner_join(stand_df, by = dplyr::join_by('StandID' == 'STAND_ID'))
    return(errs)
  }else{
    message('FVS_Error table not found in output DB; perhaps no errors or warnings to report.')
    return(NULL)
  }

  on.exit(DBI::dbDisconnect(out_conn))
}
