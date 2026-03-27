#' Run multiple stands through FVS, in parallel.
#'
#' This function uses the `future.apply` and `future` packages to support
#' parallel processing. Consult `future` package documentation for more details.
#'
#' @param stand_info A dataframe where each row is a stand. Must contain
#'   STAND_CN column.
#' @param tree_list A dataframe where each row is a tree record. Must contain
#'   STAND_CN column. If the trees in this dataframe are not associated with a
#'   stand in `stand_info`, or if no trees are alive, the stand will be skipped.
#' @param n_batches Optional. If a positive integer is supplied, the stands will
#'   be grouped into `n_batches` groups. Each group will be run in parallel, but
#'   groups will be run sequentially. This is faster than attempting to run all
#'   stands in parallel when there are a large number of stands (>1000 or so).
#' @param simple_output Optional, default TRUE. See Values.
#' @param out_dir Directory to put .key, .tre, and .out files.
#' @param fvs_bin Location of FVS software.
#' @param year_col Optional character string. If additional years to report
#'   varies by stand, this argument specifies the column in the stand
#'   information dataframe that contains those additional reporting years.
#' @param ... Additional parameters to pass on to control simulation. See
#'   [write_FVS_files()] for details.
#'
#' @returns If `simple_output` is `TRUE`, returns a list of two dataframes:
#'   `treelist` and `summary`. These dataframes are the outputs from
#'   [run_FVS()], with all stands and trees bound together into a single
#'   dataframe.
#'
#'   If `simple_output` is `FALSE`, returns a list of nested lists. Each nested
#'   list contains both `treelist` and `summary` for a single stand from
#'   [run_FVS()].
#' @export
#'
run_FVS_parallel <- function(stand_info, tree_list, n_batches = 1,
                             simple_output = TRUE, out_dir, fvs_bin,
                             year_col = NULL, ...){
  opt_args <- list(...)
  # argument checking
  if('sequential' %in% class(future::plan())){
    warning("Detected future backend is 'sequential', so stands will not be run in parallel. Call future::plan() before running run_FVS_parallel()",
            immediate. = TRUE)
    ans <- menu(c('Yes', 'No'), title = 'Should execution continue?')
    if(ans == 2) stop('Execution stopped.')
  }
  if(any(length(n_batches) != 1, !is.numeric(n_batches),
         n_batches >= nrow(stand_info), n_batches <= 0, n_batches%%1 != 0)){
    stop('n_batches must be a positive integer less than the number of rows in the dataset.')
  }

  if(n_batches == 1){
    batches <- rep(1, nrow(stand_info))
  }else{
    batches <- cut(seq_len(nrow(stand_info)), breaks = n_batches, labels = FALSE)
  }

  trls_list <- vector('list', n_batches)

  for(i in unique(batches)){
    batch_index <- which(batches == i)
    if(is.null(year_col)){
      trls_list[[i]] <- future.apply::future_lapply(batch_index,
                                                    .run_FVS_worker,
                                                    stand_info = stand_info,
                                                    tree_list = tree_list,
                                                    out_dir = out_dir,
                                                    fvs_bin = fvs_bin,
                                                    ...,
                                                    future.seed = TRUE)
    }else{
      trls_list[[i]] <- future.apply::future_lapply(batch_index,
                                                    .run_FVS_worker_cycleat,
                                                    stand_info = stand_info,
                                                    tree_list = tree_list,
                                                    out_dir = out_dir,
                                                    fvs_bin = fvs_bin,
                                                    year_col = year_col,
                                                    ...,
                                                    future.seed = TRUE)
    }


    message('Completed batch ', i, ' of ', n_batches)
  }
  trls <- unlist(trls_list, recursive = FALSE)

  # return results
  if(simple_output) simplify_outputs(trls) else trls
}

.run_FVS_worker <- function(index, stand_info, tree_list, out_dir, fvs_bin, ...){
  run_FVS(stand_info = stand_info[index,], tree_list = tree_list, out_dir = out_dir,
          fvs_bin = fvs_bin, ...)
}

.run_FVS_worker_cycleat <- function(index, stand_info, tree_list, out_dir,
                                    year_col, fvs_bin, ...){
  run_FVS(stand_info = stand_info[index,], tree_list = tree_list, out_dir = out_dir,
          fvs_bin = fvs_bin,
          CYCLEAT = unlist(stand_info[index,][year_col]), ...)
}

#' Simplify nested lists of FVS outputs from running FVS in parallel.
#'
#' @param output_lists
#'
#' @keywords internal
#'
#' @returns List of 2. `$tree_list` is a list of all trees at the initial and
#' projected time(s), with one row for each tree. `$summary` is the stand
#' summary; see `rFVS::fvsGetSummary()` for details.

simplify_outputs <- function(output_lists){
  full_tree_list <- lapply(output_lists, `[[`, 'tree_list') |>
    dplyr::bind_rows()
  full_summary <- lapply(output_lists, `[[`, 'summary') |>
    dplyr::bind_rows()
  return(list(tree_list = full_tree_list, summary = full_summary))
}
