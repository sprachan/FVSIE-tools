#' Load/unload the specified FVS variant as a shared library
#'
#' @description `load_FVS()` searches for the variant in the specified directory
#' and loads it if found. Errors if a different FVS variant is already loaded.
#'
#' `unload_FVS()` searches for the specified FVS library in all loaded libraries
#' and unloads if found.
#'
#' Both functions are automatically called within [run_FVS()].
#'
#' @param fvs_bin Character string. The file path where FVS DLLs are located.
#' @param program Character string, formatted as 'FVSxx' where xx is the
#'   two-letter variant code. Default is Inland Empire ('FVSie')
#' @param verbose Logical. Should FVS library loading/unloading be accompanied
#'   by a message? Default `FALSE`.
#'
#' @returns The variant name, invisibly, if successful.
#' @export
#'
#' @examples
#' \dontrun{
#' load_FVS(program = 'FVSie', fvs_bin = 'C:/FVS/FVSSoftware/FVSbin')
#' #> FVSie loaded.
#'
#' # Attempting to load an already loaded library will lead to a message:
#' load_FVS(program = 'FVSie', fvs_bin = 'C:/FVS/FVSSoftware/FVSbin')
#' load_FVS('FVSie', fvs_bin = 'C:/FVS/FVSSoftware/FVSbin')
#' #> FVSie already loaded.
#'
#' # Similarly, loading a different variant when one is already loaded will error:
#' load_FVS(program = 'FVSie', fvs_bin = 'C:/FVS/FVSSoftware/FVSbin')
#' load_FVS(program = 'FVSak', fvs_bin = 'C:/FVS/FVSSoftware/FVSbin')
#' #> Error in `load_FVS()`:
#' #> ! Variant(s) FVSie already loaded.
#' #> ℹ Use unload_FVS() to unload, then try again.
#' #> Run `rlang::last_trace()` to see where the error occurred.
#' #> Called from: signal_abort(cnd, .file)
#'
#' unload_FVS(program = 'FVSie')
#' #> FVSie unloaded.
#' }
#' @rdname load-unload
load_FVS <- function(fvs_bin,
                     program = c('FVSie', 'FVSak', 'FVSbm', 'FVSca', 'FVSci',
                                 'FVScr', 'FVScs', 'FVSec', 'FVSem', 'FVSnc',
                                 'FVSkt', 'FVSls', 'FVSne', 'FVSpn', 'FVSsn',
                                 'FVSso', 'FVStt', 'FVSut', 'FVSwc', 'FVSws'),
                     verbose = FALSE){
  stopifnot('Ensure that fvs_bin is a character string' = is.character(fvs_bin),
            'fvs_bin not found. Check where FVSSoftware/FVSbin is located' = dir.exists(fvs_bin))

  # want file names to be platform agnostic
  dynlib <- .Platform$dynlib.ext

  program <- rlang::arg_match(program)
  program_opts <- c('FVSie', 'FVSak', 'FVSbm', 'FVSca', 'FVSci',
                    'FVScr', 'FVScs', 'FVSec', 'FVSem', 'FVSnc',
                    'FVSkt', 'FVSls', 'FVSne', 'FVSpn', 'FVSsn',
                    'FVSso', 'FVStt', 'FVSut', 'FVSwc', 'FVSws')
  program_opts <- program_opts[program_opts != program]

  loaded_variants <- program_opts[program_opts %in% names(getLoadedDLLs())]
  lib_loc <- file.path(fvs_bin, paste0(program, dynlib))

  if(!file.exists(lib_loc)){
    rlang::abort(message = paste0(paste0(program, dynlib), 'not found in ', fvs_bin,
         '. Check file paths/FVS installation.'),
         class = 'dynload-err')
  }

  if(program %in% names(getLoadedDLLs())){
    rlang::inform(message = paste0(program, ' already loaded.'),
                  class = 'dynload-msg')
    invisible(program)
  }else if(length(loaded_variants) > 0){
    rlang::abort(message = c(paste0('Variant(s) ', loaded_variants, ' already loaded.'),
                             'i' = 'Use unload_FVS() to unload, then try again.'),
                 class = 'dynload-err')
  }else{
    tryCatch({dyn.load(lib_loc, local = TRUE, now = TRUE)
              if(verbose) message(program, ' loaded.')
              invisible(program)
              },
             error = function(cond){
               rlang::abort(paste0('Loading shared library with dyn.load() failed with error ',
                                   cond),
                            class = 'dynload-err')
             })
  }
}

#' Load/unload the specified FVS variant as a shared library
#' @rdname load-unload
unload_FVS <- function(program = c('FVSie', 'FVSak', 'FVSbm', 'FVSca', 'FVSci',
                                   'FVScr', 'FVScs', 'FVSec', 'FVSem', 'FVSnc',
                                   'FVSkt', 'FVSls', 'FVSne', 'FVSpn', 'FVSsn',
                                   'FVSso', 'FVStt', 'FVSut', 'FVSwc', 'FVSws'),
                       verbose = FALSE){
  program <- rlang::arg_match(program)
  if(program %in% names(getLoadedDLLs())){
    lib_loc <- getLoadedDLLs()[[program]][['path']]
    tryCatch({dyn.unload(lib_loc)
              if(verbose) message(program, ' unloaded.')
              invisible(program)
              },
      error = function(cond){
        rlang::abort(message = paste0('Unloading shared library with dyn.unload() failed with error ',
                                      cond),
                     class = 'dynload-err')
      })
  }else{
    rlang::inform(message = c(paste0(program, ' is not loaded so cannot be unloaded.'),
                              'i' = 'Consider running getLoadedDLLs() to see if any other variants are loaded.'),
                  class = 'dynload-msg')
  }
}
