#' Fill missing values from tree list using stand information.
#'
#' @param tree_list Tree list. Dataframe.
#' @param stand_info Stand information associated with the tree list. Dataframe.
#'
#' @keywords internal
#'
#' @returns A dataframe containing all tree information necessary to be input
#' into FVS.
#'

fill_tree_list <- function(tree_list, stand_info){
  stopifnot('STAND_CN column required to match tree and stand information' =
              'STAND_CN' %in% colnames(tree_list)&'STAND_CN' %in% colnames(stand_info))
  stand_cols <- c('STAND_CN', 'PV_CODE', 'PV_REF_CODE', 'SLOPE', 'ASPECT')
  stopifnot('Stand data frame missing a necessary column' =
              all(stand_cols %in% colnames(stand_info)))

  # Look for a crown ratio column and rename to the default name, CR_RATIO
  if(!'CR_RATIO' %in% colnames(tree_list)){
    stopifnot("Couldn't find a crown ratio column, e.g., CR_RATIO" =
                any(check_name(colnames(tree_list), 'CR_RATIO')))
    idx <- which(check_name(name = colnames(tree_list),
                            check_against = 'CR_RATIO'))
    colnames(tree_list)[idx] <- 'CR_RATIO'
  }

  # Fill in missing site information from stand list
  out <- tree_list |>
    dplyr::mutate(PV_CODE = ifelse(is.na(.data$PV_CODE), stand_info$PV_CODE, .data$PV_CODE),
                  PV_REF_CODE = ifelse(is.na(.data$PV_REF_CODE), stand_info$PV_REF_CODE, .data$PV_REF_CODE),
                  SLOPE = ifelse(is.na(.data$SLOPE), stand_info$SLOPE, .data$SLOPE),
                  ASPECT = ifelse(is.na(.data$ASPECT), stand_info$ASPECT, .data$ASPECT),
                  # make crown ratio into 10% classes, per Essential FVS p. 41:
                  #> 1: 0-10%; 2: 11-20%; ..., 9: 81-100%
                  #> Because they say 0-10%, 11-20%, I assume that e.g., 10.5% counts as 10%...
                  CR_RATIO = cut(.data$CR_RATIO, breaks = c(0, 11, 21, 31, 41, 51, 61, 71, 81, 100),
                                 labels = FALSE,
                                 right = FALSE,
                                 include.lowest = TRUE),
                  DAMAGE1 = ifelse(!is.na(.data$HTTOPK),
                                   yes = 97,
                                   no = 0))
  if(!'FVS_HAB' %in% colnames(out)){
    out <- out |>
      dplyr::rowwise() |>
      dplyr::mutate(FVS_HAB = map_habcode(.data$PV_CODE, .data$PV_REF_CODE)) |>
      dplyr::ungroup()
  }else if(typeof(out$FVS_HAB) != 'integer'){
    warning('Converting FVS_HAB to integer, NAs may be introduced')
    out$FVS_HAB <- as.integer(out$FVS_HAB)
  }
  as.data.frame(out)
}
