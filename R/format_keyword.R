#' Format an FVS keyword
#'
#' Add whitespace and use optional datatypes to format an FVS keyword to match
#' requirements. To format READCORD/READCORR/READCORH values, see (...)
#'
#' @param keyword A character string. The FVS keyword name.
#' @param dtypes Optional character vector specifying the datatype of each
#'   field. The length needs to match the number of fields. 's' for string, 'i'
#'   for integer, and 'f' for float (decimals)
#' @param round_to An optional integer specifying how many places floats should be
#'   rounded to (default 2).
#' @param ... Optional values for fields associated with the keyword. Will be
#'   coerced to the type specified by `dtypes` if specified or character if
#'   `dtypes` is not specified.
#'
#' @returns A character string.
#' @export
#'
#' @examples
#' format_keyword('NOINGROW')
#'
#' format_keyword('SDICALC', 0, 1, 1)
#'
#' # Lower case will be converted to upper case,
#' #  and any spaces will be removed in the keyword:
#' format_keyword('sdi calc', 0, 1, 1)
#'
#' # not recommended to use for floats without specifying datatype:
#' # BAD
#' format_keyword('SDICALC', 1/3, 1, 1)
#'
#' # OK
#' format_keyword('SDICALC', dtypes = c('f', 'i', 'i'), 1/3, 1, 1)
#'
#' # For multiple keywords to go one after another, use paste() with sep = '\n':
#' additionals <- paste(format_keyword('SDICALC', dtypes = c('f', 'i', 'i'), 1/3, 1, 1),
#'                      format_keyword('SDIMAX',
#'                                     dtypes = c('i', 'f', 's', 's', 'f', 'f', 'i'),
#'                                     1, 300, '', '', 65, 90, 0),
#'                      sep = "\n")
#' cat(additionals)
#'

format_keyword <- function(keyword, ..., dtypes = NULL, round_to = 2){
  fields <- list(...)
  n_fields <- length(fields)
  if(!is.null(dtypes)){
    stopifnot('Length of dtypes must match number of fields provided' =
                length(dtypes) == n_fields)
    dtypes[dtypes == 'f'] <- paste0('.', round_to, 'f')
    fmt <- paste0('%-10s', paste0('%10', dtypes, collapse = ''))
  }else if(n_fields == 0){
    return(sprintf('%-10s', toupper(gsub(' ', '', keyword, fixed = TRUE))))
  }else{
    fmt <- paste0('%-10s', paste0(rep('%10s', n_fields), collapse = ''))
  }
  return(do.call(sprintf, c(fmt, append(fields, values = toupper(gsub(' ', '', keyword, fixed = TRUE)), after = 0))))
}
