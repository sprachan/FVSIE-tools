#' Write an FVS Keyword file.
#'
#' @description Given simulation parameters and file paths, this function writes
#'   a .key file that can be used to control an FVS simulation. Automatic
#'   formatting is supported for a variety of keywords; additional keywords may
#'   be passed but must be formatted according to the [FVS Keyword
#'   guide](https://www.fs.usda.gov/sites/default/files/fvs-keyword.pdf).
#'   Default simulation parameters (cycle length, number of years, calibration,
#'   etc) are designed to match the FVS GUI defaults specified in Essential FVS,
#'   except outputs are automatically written to a SQLite database.
#'
#'   By default, the keyword file specifies a simulation where:
#'   * Simulation results are output to a SQLite database called FVSOut.db in the working directory.
#'   * Cycles are 10 years in length.
#'   * The simulation is run for 100 years total.
#'   * Self-calibration is turned ON (see [Essential FVS](https://www.fs.usda.gov/sites/default/files/forest-management/essential-fvs.pdf) section 6.4 for details).
#'   * Missing heights are dubbed using the height-diameter relationships from the input data.
#'   * Tripling is turned ON (see [Essential FVS](https://www.fs.usda.gov/sites/default/files/forest-management/essential-fvs.pdf) p. 156 for details).
#'   * The regeneration establishment model is turned ON (see [Essential FVS](https://www.fs.usda.gov/sites/default/files/forest-management/essential-fvs.pdf) section 5.4 for details).
#'
#' @param out_dir A character string specifying the directory for the keyword
#'   file and FVS output. Defaults to working directory.
#' @param file_prefix A character string giving the keyword file name to go
#'   before the '.key' extension.
#' @param database A character string specifying the full file path of the input
#'   SQLite database.
#' @param STDIDENT Optional character string specifying the stand identity.
#'   Should be NULL (default) for multistand projections; see *THIS WOULD BE A
#'   GOOD VIGNETTE*
#' @param n_years Integer specifying the number of years into the future to
#'   project. Default 100.
#' @param CYCLEAT Optional integer specifying additional years to add cycles.
#'   Useful for getting stand/tree information in specific years.
#' @param calibrate Logical. Should self-calibration be on (`TRUE`, default) or
#'   turned off for the simulation (`FALSE`)?
#' @param htd_reg Logical. Should missing heights be dubbed using heights in the
#'   data (`TRUE`, default) or using regional defaults (`FALSE`)?
#' @param triple Logical. Should tree records be split into three identical
#'   records whose TPA sums to the original tree TPA in order to stabilize
#'   estimates in small stands (`TRUE`, default) or not (`FALSE`)?
#' @param add_regen Logical. Should the regeneration/establishment model
#'   associated with the variant be used (`TRUE`, default) or turned off
#'   (`FALSE`)?
#' @param carbon_report Logical. Should the Fire and Fuels extension carbon
#'   report be generated (`TRUE`) or not (`FALSE`, default)?
#' @param estab_keywords Optional character vector of keywords to control the
#'   regeneration/establishment model (see Essential FVS 5.4). Each element in
#'   the vector (or string if only one keyword is passed) must be formatted
#'   according to the FVS keyword guide.
#' @param ffe_keywords Optional character vector of keywords to pass to the Fire
#'   and Fuels Extension. Each element in the vector (or string if only one
#'   keyword is passed) must be formatted according to the FVS keyword guide.
#' @param random_seed Optional odd integer to reseed the random number generator
#'   for the base FVS model. Useful for reproducible simulation results.
#' @param db_tables Optional character vector of keywords to request additional
#'   output tables.
#' @param READCORD Optional numeric vector of multipliers for the large tree
#'   diameter growth model. The length and order of the vector must correspond
#'   to the species in the FVS variant in order for multipliers to apply
#'   correctly. See Essential FVS 6.5.2.2.
#' @param READCORH Optional numeric vector of multipliers for the large tree
#'   height growth model. The length and order of the vector must correspond to
#'   the species in the FVS variant in order for multipliers to apply correctly.
#'   See Essential FVS 6.5.2.2.
#' @param READCORR Optional numeric vector of multipliers for the small tree
#'   height growth model. The length and order of the vector must correspond to
#'   the species in the FVS variant in order for multipliers to apply correctly.
#'   See Essential FVS 6.5.2.2.
#' @param additionals Optional character vector of additional keywords. Each
#'   element must be formatted according to the FVS keyword guide.
#'
#' @returns The keyword file name, invisibly.
#'
#' @export
#'
#' @seealso [format_keyword()] for formatting keywords; [write_multistand_key()]
#'   for writing a keyword file to run multiple stands with the same simulation
#'   parameters.
#' @examples
#' \dontrun{
#'   out_dir <- tempdir()
#'   database <- system.file(file.path('extdata', 'ex_data.db'), package = 'rFVSIEtools')
#'   write_FVS_key(out_dir = out_dir, file_prefix = 'example_kwd', database = database)
#'
#' }
write_FVS_key <- function(out_dir = getwd(),
                          file_prefix,
                          database,
                          STDIDENT = NULL,
                          n_years = 100,
                          CYCLEAT = NULL,
                          calibrate = TRUE,
                          htd_reg = TRUE,
                          triple = TRUE,
                          add_regen = TRUE,
                          carbon_report = FALSE,
                          estab_keywords = NULL,
                          ffe_keywords = NULL,
                          random_seed = NULL,
                          db_tables = NULL,
                          READCORD = NULL,
                          READCORH = NULL,
                          READCORR = NULL,
                          additionals = NULL){
  stopifnot('Specified output directory does not exist' = dir.exists(out_dir))
  if(.Platform$OS.type == 'windows'){
    out_dir <- normalizePath(out_dir, winslash = '/')
  }else{
    out_dir <- normalizePath(out_dir)
  }

  # Output paths
  fvs_output <- file.path(out_dir, 'FVSOut.db')

  keyfile_name <- file.path(out_dir, paste0(file_prefix, '.key'))
  if(file.exists(keyfile_name)){
    rlang::warn(message = c('x' = paste0('A file already exists at ', keyfile_name, '.'),
                            'The existing file will be overwritten.'))
    file.remove(keyfile_name)
  }

  # Fail fast if database not found or tables incorrectly named
  stopifnot('Database file not found; check that it exists' = file.exists(database))
  check_db_run(database)

  # Write STDIDENT if provided -------------------------------------------------
  if(!is.null(STDIDENT)){
    write(sprintf('%-10s', 'STDIDENT'), file = keyfile_name, append = TRUE)
    write(sprintf('%-80s', STDIDENT), file = keyfile_name, append = TRUE)
  }

  # Request tree list output ---------------------------------------------------
  write(sprintf('%-10s%10i%10s%10i%10i%10s%10s%10i',
                'TREELIST',0,'',0,0,'','',1),
        file = keyfile_name, append = TRUE)

  # Database options -----------------------------------------------------------
  write(sprintf('%-10s', 'DATABASE'), file = keyfile_name, append = TRUE)

  write(sprintf('%-10s', 'DSNOUT'), file = keyfile_name, append = TRUE)
  write(paste0(fvs_output), file = keyfile_name, append = TRUE)

  write(sprintf('%-10s', 'DSNIN'), file = keyfile_name, append = TRUE)
  write(paste0(database),
        file = keyfile_name, append = TRUE)

  # automatically read current stand and tree information from the database
  write(sprintf('%-10s', 'StandSQL'), file = keyfile_name, append = TRUE)
  write('SELECT *', file = keyfile_name, append = TRUE)
  write('FROM FVS_StandInit', file = keyfile_name, append = TRUE)
  write(paste0('WHERE Stand_ID = ', "'%StandID%'"), file = keyfile_name, append = TRUE)
  write('EndSQL', file = keyfile_name, append = TRUE)

  write(sprintf('%-10s', 'TreeSQL'), file = keyfile_name, append = TRUE)
  write('SELECT *', file = keyfile_name, append = TRUE)
  write('FROM FVS_TreeInit', file = keyfile_name, append = TRUE)
  write(paste0('WHERE Stand_ID = ', "'%StandID%'"), file = keyfile_name, append = TRUE)
  write('EndSQL', file = keyfile_name, append = TRUE)

  # request additional tables
  if(!is.null(db_tables)){
    if(length(db_tables) > 1){
      vapply(db_tables, FUN = \(x) write(sprintf('%-10s', x),
                                         file = keyfile_name,
                                         append = TRUE),
             FUN.VALUE = character(1),
             USE.NAMES = FALSE)
    }else{
      write(sprintf('%-10s', db_tables), file = keyfile_name, append = TRUE)
    }
  }
  write(sprintf('%-10s', 'CALBSTDB'), file = keyfile_name, append = TRUE)
  write(sprintf('%-10s%10i%10i', 'TREELIDB', 2, 0), file = keyfile_name, append = TRUE)
  if(carbon_report){
    write(sprintf('%-10s', 'CARBREDB'), file = keyfile_name, append = TRUE)
  }

  write(sprintf('%-10s', 'END'), file = keyfile_name, append = TRUE)
  write('', file = keyfile_name, append = TRUE)

  # Replication: random seed, tripling -----------------------------------------
  if(!is.null(random_seed)) write(sprintf('%-10s%10i', 'RANNSEED', random_seed),
                                  file = keyfile_name, append = TRUE)
  if(!triple) write(sprintf('%-10s', 'NOTRIPLE'),
                    file = keyfile_name, append = TRUE)

  # Calibration options --------------------------------------------------------
  if(!calibrate) write(sprintf('%-10s', 'NOCALIB'),
                       file = keyfile_name, append = TRUE)
  if(!htd_reg) write(sprintf('%-10s', 'NOHTDREG'),
                     file = keyfile_name, append = TRUE)
  if(!is.null(READCORD)){
    write(sprintf('%-10s', 'READCORD'), file = keyfile_name, append = TRUE)
    readcord_fmtd <- format_readcorx(READCORD)
    cat(readcord_fmtd, file = keyfile_name, append = TRUE, fill = 80)
    write('', file = keyfile_name, append = TRUE)
  }
  if(!is.null(READCORH)){
    write(sprintf('%-10s', 'READCORH'), file = keyfile_name, append = TRUE)
    readcorh_fmtd <- format_readcorx(READCORH)
    cat(readcorh_fmtd, file = keyfile_name, append = TRUE, fill = 80)
    write('', file = keyfile_name, append = TRUE)
  }
  if(!is.null(READCORR)){
    write(sprintf('%-10s', 'READCORR'), file = keyfile_name, append = TRUE)
    readcorr_fmtd <- format_readcorx(READCORR)
    cat(readcorr_fmtd, file = keyfile_name, append = TRUE, fill = 80)
    write('', file = keyfile_name, append = TRUE)
  }

  write('', file = keyfile_name, append = TRUE)
  # Regeneration/Establishment options -----------------------------------------
  if(add_regen){
    write("ESTAB", file = keyfile_name, append = TRUE)
    if(length(estab_keywords) >= 1){
      write(estab_keywords, file = keyfile_name, append = TRUE)
      write('', file = keyfile_name, append = TRUE)
    }
  }else{
    write("NOAUTOES", file = keyfile_name, append = TRUE)
  }
  write("END", file = keyfile_name, append = TRUE)
  write('', file = keyfile_name, append = TRUE)
  # Fire and Fuels extension ---------------------------------------------------
  if(any(carbon_report, !is.null(ffe_keywords))){
    write(sprintf('%-10s', 'FMIN'), file = keyfile_name, append = TRUE)
    if(carbon_report){
      write(sprintf('%-10s', 'CARBREPT'), file = keyfile_name, append = TRUE)
    }
    if(!is.null(ffe_keywords)){
      write(ffe_keywords, file = keyfile_name, append = TRUE)
    }
    write(sprintf('%-10s', 'END'), file = keyfile_name, append = TRUE)
    write('', file = keyfile_name, append = TRUE)
  }
  # Additional keywords --------------------------------------------------------
  if(length(additionals) > 1){
    vapply(additionals, \(x) write(x, file = keyfile_name, append = TRUE),
           FUN.VALUE = character(1),
           USE.NAMES = FALSE)
  }else if(length(additionals) == 1){
    write(additionals, file = keyfile_name, append = TRUE)
  }
  write('', file = keyfile_name, append = TRUE)
  # Simulation length and additional years -------------------------------------
  if(any(grepl('TIMEINT', additionals, fixed = TRUE))){
    cycle_length <- as.integer(substr(grep('TIMEINT', additionals, fixed = TRUE, value = TRUE),
                                      21, 30))
  }else{
    cycle_length <- 10
  }

  n_cycles <- ceiling(n_years/cycle_length)
  write(sprintf('%-10s%10i', 'NUMCYCLE', n_cycles), file = keyfile_name, append = TRUE)

  if(length(CYCLEAT)>1){
    vapply(CYCLEAT, FUN = \(x) write(sprintf('%-10s%10i', 'CYCLEAT', x),
                               file = keyfile_name, append = TRUE),
           FUN.VALUE = character(1),
           USE.NAMES = FALSE)
  }else if(length(CYCLEAT) == 1){
    write(sprintf('%-10s%10i', 'CYCLEAT', CYCLEAT), file = keyfile_name, append = TRUE)
  }
  write('', file = keyfile_name, append = TRUE)
  # Process if single stand ----------------------------------------------------
  if(!is.null(STDIDENT)){
    write("PROCESS", file = keyfile_name, append = TRUE)
    write("STOP", file = keyfile_name, append = TRUE)
  }
  invisible(normalizePath(keyfile_name, winslash = '/'))
}

#' Write a keyword file to run multiple stands with the same simulation
#' parameters
#'
#' @description This function writes two .key files: one that defines the
#'   keywords shared by all simulations ('shared_kwds.key') and one that tells
#'   FVS to use these keywords to run the stands specified in `STDIDENTs` (or
#'   all stands in the input database; '<file_prefix>.key'). Default shared
#'   simulation parameters match those in the FVS GUI except outputs are
#'   automatically written to an external file.
#'
#'   By default, the shared keyword file specifies a simulation where:
#'   * Simulation results are output to a SQLite database called FVSOut.db in the working directory.
#'   * Cycles are 10 years in length.
#'   * The simulation is run for 100 years total.
#'   * Self-calibration is turned ON (see [Essential FVS](https://www.fs.usda.gov/sites/default/files/forest-management/essential-fvs.pdf) section 6.4 for details).
#'   * Missing heights are dubbed using the height-diameter relationships from the input data.
#'   * Tripling is turned ON (see [Essential FVS](https://www.fs.usda.gov/sites/default/files/forest-management/essential-fvs.pdf) p. 156 for details).
#'   * The regeneration establishment model is turned ON (see [Essential FVS](https://www.fs.usda.gov/sites/default/files/forest-management/essential-fvs.pdf) section 5.4 for details).
#'
#'   See details for simulation options.
#'
#' @param STDIDENTs Optional character vector of stand identifiers specifying
#'   the stands to be run in the simulation. If not provided, all STAND_ID
#'   values in the stand table of the provided database will be used.
#' @param out_dir A character string specifying the directory for the keyword
#'   file and FVS output. Defaults to working directory.
#' @param database A character string specifying the full file path of the input
#'   SQLite database.
#' @param file_prefix An optional character string giving the keyword file name
#'   to go before the '.key' extension. Default for multistand runs is
#'   'all_stands'.
#' @param addfile_ref Optional integer file reference number, for FVS use. See
#'   FVS keyword documentation for the ADDFILE keyword. Must be at least 31;
#'   default is 40.
#' @param ... Parameters passed to [write_FVS_key()]. See details.
#'
#' @details The following named arguments can be passed to the `...` argument (see
#'   [write_FVS_key()] for details):
#'
#'    * `n_years`: Integer, default 100.
#'    * `CYCLEAT`: Optional integer.
#'    * `calibrate`: Logical, default `TRUE`.
#'    * `htd_reg`: Logical, default `TRUE`.
#'    * `triple`: Logical, default `TRUE`.
#'    * `add_regen`:  Logical, default `TRUE`.
#'    * `carbon_report`: Logical, default `FALSE`.
#'    * `estab_keywords`: Optional character vector of keywords.
#'    * `ffe_keywords`: Optional character vector of keywords.
#'    * `random_seed`: Optional odd integer.
#'    * `db_tables`: Optional character vector.
#'    * `READCORD`, `READCORH`, `READCORR`: Optional numeric vectors.
#'    * `READCORH`: Optional numeric vector.
#'    * `additionals`: Optional character vector.
#'
#'
#'
#' @returns The name of the keyword file that runs all requested stands,
#'   invisibly.
#' @export
#' @seealso [write_FVS_key()]

write_multistand_key <- function(STDIDENTs = NULL, out_dir = getwd(), database,
                                 file_prefix = 'all_stands', addfile_ref = 40,
                                 ...){
  check_db_run(database)
  stopifnot(addfile_ref > 30)
  if(is.null(STDIDENTs)){
    conn <- DBI::dbConnect(RSQLite::SQLite(), database)
    STDIDENTs <- dplyr::tbl(conn, 'FVS_StandInit') |>
      dplyr::pull(.data$STAND_ID)
    DBI::dbDisconnect(conn)
  }
  stopifnot(length(STDIDENTs) > 1,
            'Specified output directory does not exist' = dir.exists(out_dir))
  if(.Platform$OS.type == 'windows'){
    out_dir <- normalizePath(out_dir, winslash = '/')
  }else{
    out_dir <- normalizePath(out_dir)
  }

  keyfile_name <- file.path(out_dir, paste0(file_prefix, '.key'))
  if(file.exists(keyfile_name)){
    rlang::warn(message = c('x' = paste0('A file already exists at ', keyfile_name, '.'),
                            'The existing file will be overwritten.'))
    file.remove(keyfile_name)
  }

  add_file <- write_FVS_key(STDIDENT = NULL, out_dir = out_dir,
                            file_prefix = 'shared_kwds', database = database, ...)

  for(st in STDIDENTs){
    write(sprintf('%-10s', 'STDIDENT'), file = keyfile_name, append = TRUE)
    write(sprintf('%80-s', st), file = keyfile_name, append = TRUE)

    # include shared keywords
    write(sprintf('%-10s%10i', 'OPEN', addfile_ref), file = keyfile_name, append = TRUE)
    write(sprintf('%10s', add_file), file = keyfile_name, append = TRUE)
    write(sprintf('%-10s%10i', 'ADDFILE', addfile_ref), file = keyfile_name, append = TRUE)
    write(sprintf('%-10s%10i', 'CLOSE', addfile_ref), file = keyfile_name, append = TRUE)

    write(sprintf('%-10s', 'PROCESS'), file = keyfile_name, append = TRUE)
    write('', file = keyfile_name, append = TRUE)
  }
  write('STOP', file = keyfile_name, append = TRUE)
  invisible(normalizePath(keyfile_name, winslash = '/'))
}


