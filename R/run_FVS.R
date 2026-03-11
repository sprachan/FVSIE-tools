#' Run FVS-IE.
#'
#' @param trees Tree list, e.g., from get_FIA_state().
#' @param standinfo Stand information for the single stand corresponding to `treelist`.
#' @param outdir Directory to write keyword, tree, and .out files to.
#' @param fvs_bin FVS software location.
#' @param ... Additional arguments passed to write_FVS_files() to control simulation.
#' @param verbose If TRUE, report the names of the summary table and the number of rows in each year.
#'
#' @returns A list of two. $treelist is the combined tree list from year 0 and the final simulation year; summary is the FVS summary table.
#' @export

run_FVS <- function(trees, standinfo, outdir, fvs_bin,
                    ..., verbose = FALSE){

  rFVS::fvsLoad("FVSie", fvs_bin)
  f <- write_FVS_files(trees = trees, standinfo = standinfo,
                      outdir = outdir,
                      ...)

  rFVS::fvsSetCmdLine(paste0('--keywordfile=', f, '.key'))

  fvs_output <- rFVS::fvsInteractRun(AfterEM1 = 'fetchTrees()',
                                     SimEnd = rFVS::fvsGetSummary)

  if(verbose){
    print(names(fvs_output))
  }

  # year 0 tree list
  tl0 <- dplyr::left_join(fvs_output[[1]]$AfterEM1,
                          dplyr::select(trees, .data$fvs.TREE_ID, .data$TUID, .data$PID),
                          by = c('id' = 'fvs.TREE_ID'))

  # end of projection cycle tree list
  tl1 <- dplyr::left_join(fvs_output[[2]]$AfterEM1,
                          dplyr::select(trees, .data$fvs.TREE_ID, .data$TUID, .data$PID),
                          by = c('id' = 'fvs.TREE_ID'))

  if(verbose){
    cat('Year 0 nrow: ', nrow(tl0), '\n Year N nrow: ', nrow(tl1))
  }

  tl <- rbind(tl0, tl1)
  plt_summary <- cbind(fvs_output[[length(fvs_output)]],
                       data.frame(PID = standinfo$PID))

  rFVS::fvsLoad('FVSie', fvs_bin)

  list(treelist = tl, summary = plt_summary)
}

#' Get treelist from FVS run.
#'
#' @keywords internal

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
