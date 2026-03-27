#' Run FVS-IE.
#'
#' @description When used with no additional arguments, runs FVS with these
#' default parameters:
#' *  100 years (modify with `proj_len`)
#' * Self-calibration turned ON (use `calibrate = FALSE` to disable)
#' * Tripling turned off (use `triple = TRUE` to turn on)
#' * Regeneration turned off (use `add_regen = TRUE ` to turn on).
#'
#' See [write_FVS_files()] for details on additional arguments to control the
#' simulation. Currently only supports reporting after the first cycle. Future
#' development to expand this functionality.
#'
#' @param tree_list Tree list.
#' @param stand_info Stand information for the single stand corresponding to
#'   `tree_list`.
#' @param out_dir Directory to write keyword, tree, and .out files to.
#' @param fvs_bin FVS software location.
#' @param ... Additional arguments passed to `write_FVS_files()` to control
#'   simulation. See [write_FVS_files()].
#' @param verbose If TRUE, report the names of the summary table and the number
#'   of rows in each year.
#'
#' @returns A list of two. `$tree_list` is the combined tree list from year 0 and
#'   the final simulation year; `$summary` is the FVS summary table.
#' @export

run_FVS <- function(tree_list, stand_info, out_dir, fvs_bin, ..., verbose = FALSE){
  # argument checking
  stopifnot('Ensure that fvs_bin is a character string' = is.character(fvs_bin))
  if(!file.exists(file.path(fvs_bin, 'FVSie.dll'))){
    stop('FVSie.dll not found in ', fvs_bin, '. Check FVS installation.')
  }

  tr <- as.data.frame(tree_list[tree_list$STAND_CN == stand_info$STAND_CN,])
  if(!('fvs.TREE_ID' %in% colnames(tr))){
    tr$fvs.TREE_ID <- seq_len(nrow(tr))
  }
  if(sum(tr$HISTORY %in% 6:9) == nrow(tr)|nrow(tr) == 0){
    message('Skipping stand ', stand_info$STAND_CN, sep = '')
    return(list(tree_list = NULL, summary = NULL))
  }else{
    rFVS::fvsLoad("FVSie", fvs_bin)
    f <- write_FVS_files(tree_list = tr, stand_info = stand_info,
                         out_dir = out_dir, ...)

    rFVS::fvsSetCmdLine(paste0('--keywordfile=', f, '.key'))

    fvs_output <- rFVS::fvsInteractRun(AfterEM1 = 'rFVSIEtools::fetch_trees()',
                                       SimEnd = rFVS::fvsGetSummary)

    if(verbose) print(names(fvs_output))

    # year 0 tree list
    tl0 <- dplyr::left_join(fvs_output[[1]]$AfterEM1,
                            dplyr::select(tr, .data$fvs.TREE_ID, .data$TUID,
                                          .data$PID),
                            by = c('id' = 'fvs.TREE_ID'))

    # end of projection cycle tree list
    tl1 <- dplyr::left_join(fvs_output[[2]]$AfterEM1,
                            dplyr::select(tr, .data$fvs.TREE_ID, .data$TUID,
                                          .data$PID),
                            by = c('id' = 'fvs.TREE_ID'))

    if(verbose) cat('Year 0 nrow: ', nrow(tl0), '\n Year N nrow: ', nrow(tl1))

    tl <- rbind(tl0, tl1)
    plt_summary <- cbind(fvs_output[[length(fvs_output)]],
                         data.frame(PID = stand_info$PID))

    rFVS::fvsLoad('FVSie', fvs_bin)

    list(tree_list = tl, summary = plt_summary)
  }
}
#' Get treelist from FVS run.
#'
#' Exported only because of a quirk in rFVS.
#'
#' @export

fetch_trees <- function(){
  tree_list <- rFVS::fvsGetTreeAttrs(c("id",
                                       "plot",
                                       "age",
                                       "species",
                                       "dbh",
                                       "ht",
                                       "cratio",
                                       "tpa",
                                       "mcuft",
                                       "bdft"))
  tree_list$year <- rFVS::fvsGetEventMonitorVariables("year")
  return(tree_list)
}
