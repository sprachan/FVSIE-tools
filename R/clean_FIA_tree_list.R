#' Clean an FIA tree list to prepare for FVS.
#'
#' @param treelist Tree list, from FIA_TREEINIT_PLOT table (FVS_TreeInit from get_FIA())
#' @param standinfo Stand information associated with the tree list (FVS_StandInit from get_FIA())
#'
#' @returns A dataframe containing all tree information necessary to be input into FVS.
#' @export
#'

clean_FIA_treeList <- function(treelist, standinfo){
  # copy over information from the stand list
  out <- dplyr::select(standinfo,
                            .data$SLOPE, .data$ASPECT, .data$PV_CODE, .data$TOPO,
                            .data$STAND_CN) |>
    dplyr::right_join(treelist, by = 'STAND_CN') |>
    dplyr::mutate(SPREP = 0,
                  TVAL = 0,
                  CUT = 0,
                  # make crown ratio into 10% classes, per Essential FVS p. 41:
                  #> 1: 0-10%; 2: 11-20%; ..., 9: 81-100%
                  #> Because they say 0-10%, 11-20%, I assume that e.g., 10.5% counts as 10%...
                 CRcode = cut(.data$CRRATIO, breaks = c(0, 11, 21, 31, 41, 51, 61, 71, 81, 100),
                                         labels = FALSE,
                                         right = FALSE,
                                         include.lowest = TRUE),
                  DAMAGE1 = ifelse(!is.na(.data$HTTOPK),
                                         yes = 97,
                                         no = 0),
                  DAMAGE2 = 0,
                  DAMAGE3 = 0,
                  SEVERITY1 = 0,
                  SEVERITY2 = 0,
                  SEVERITY3 = 0)
  out$fvs.TREE_ID <- 1:nrow(treelist)
  return(out)
}
