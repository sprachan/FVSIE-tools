#' Visualize FVS Message Frequencies
#'
#' @inheritParams get_FVS_messages
#' @param verbose Boolean. If TRUE, unique error messages will be printed to the
#'   console. Default FALSE.
#' @param ... Optional parameters to pass to plot(). Only used if ggplot is not
#'   installed.
#'
#' @returns If ggplot2 is installed, a ggplot object. Otherwise, a plot on the
#'   current base R plotting device.
#' @export
#'
plot_FVS_messages <- function(out_dir, out_db, stand_df, verbose = FALSE,
                              ...){
  errs <- get_FVS_messages(out_dir, out_db, stand_df)
  if(verbose){
    if(length(unique(errs$Message)) == 0){
      message('No FVS messages')
    }else{
      message('Error messages: \n', paste(sort(unique(errs$Message)), collapse = '\n'))
    }
  }

  if(requireNamespace('ggplot2', quietly = TRUE)){
    ggplot2::ggplot(errs)+ggplot2::geom_bar(ggplot2::aes(x = .data$warn_type))
  }else{
    err_plt <- as.data.frame(table(errs$warn_type),
                             col.names = c('warn_type', 'count'))
    plot(x = err_plt$warn_type, y = err_plt$count, type = 'h', ...)
  }
}
