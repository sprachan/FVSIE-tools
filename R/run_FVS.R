#' Run FVS from a database with stand and tree information.
#'
#' @inheritParams load_FVS
#' @param variant Character string. The two-letter code corresponding to an FVS
#'   variant. Default is Inland Empire ('ie').
#' @param keyword_file Character string. Path to the keyword file controlling
#'   the simulation; include the .key extension. See [write_FVS_key()] for
#'   keyword file generation.
#' @param stop_year Optional integer. Year to stop simulation and store
#'   simulation state.
#' @param stop_code Optional integer. State of the simulation to store.
#' @param stop_file Optional character string. Where to store the stand
#'   variables at the supplied stop point.
#'
#' @returns The FVS program name (e.g., 'FVSie'), invisibly. Simulation outputs
#'   are sent to the file specified in the keyword file and to a '.out' file
#'   (location and prefix specified in keyword file).
#' @export
#'
#' @examples
#' \dontrun{
#' out_dir <- tempdir()
#' database <- system.file(file.path('extdata', 'ex_data.db'), package = 'rFVSIEtools')
#' kwd <- write_FVS_key(out_dir = out_dir, file_prefix = 'example_kwd', database = database)
#'
#' run_FVS(fvs_bin = 'C:/FVS/FVSSoftware/FVSbin', variant = 'ie', keyword_file = kwd)
#' }
#'
run_FVS <- function(fvs_bin,
                    variant=c('ie', 'ak', 'bm', 'ca', 'ci', 'cr', 'cs',
                              'ec', 'em', 'nc', 'kt', 'ls', 'ne', 'pn',
                              'sn', 'so', 'tt', 'ut', 'wc', 'ws'),
                    keyword_file,
                    stop_year = NULL, stop_code = NULL, stop_file = NULL,
                    verbose = FALSE){
  stopifnot('keyword file must not be empty' = nchar(keyword_file) > 0,
            'keyword_file must have a .key suffix' = substr(keyword_file, nchar(keyword_file)-3, nchar(keyword_file)) == '.key')
  variant <- rlang::arg_match(variant)
  program <- paste0('FVS', variant)

  # Need to load DLL to access functions for running FVS
  lib <- load_FVS(program = program, fvs_bin = fvs_bin, verbose = verbose)

  if(any(is.null(stop_year), is.null(stop_code))){
    command <-  paste0(' --keywordfile=', keyword_file)
  }else{
    cmd_stop <- paste0(stop_code, ',', stop_year,
                       if(stop_file == '') ''  else paste0(',', stop_file))
    command <- paste0('--keywordfile=', keyword_file,
                      '--stoppoint=', cmd_stop)
  }

  # Use tryCatch for error checking to ensure that shared library unloaded
  #> even if failure occurs early on
  tryCatch(stopifnot(is.loaded('CfvsSetCmdLine', PACKAGE = lib)),
           error = function(cond){
             unload_FVS(program, verbose = verbose)
             stop('Function CfvsSetCmdLine not found in shared library.')
           })
  .C('CfvsSetCmdLine', command, nchar(command), 0L,
     PACKAGE = lib)

  tryCatch(stopifnot(is.loaded('fvs', PACKAGE = lib)),
           error = function(cond){
             unload_FVS(program, verbose = verbose)
             stop('Function CfvsSetCmdLine not found in shared library.')
           })

  fvs_return <- 0
  while(fvs_return == 0){
    fvs_return <- .Fortran('fvs', 0L, PACKAGE = lib)
  }
  unload_FVS(program, verbose = verbose)
}
