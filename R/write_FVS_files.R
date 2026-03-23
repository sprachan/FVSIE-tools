#' Write keyword files and treelist files.
#'
#' This function, wrapped in [run_FVS()], writes and saves .key and .tre files
#'   that FVS-IE uses to initialize and carry out a simulation.
#'   Use outside of `run_FVS()` when diagnostics on these files are needed.
#'
#' @param treelist Tree list to be run through FVS, e.g., FVS_TreeInit output from get_FIA()
#' @param standinfo Stand data to be run through FVS, e.g., FVS_StandInit output from get_FIA()
#' @param years_out How many years ahead should we project?
#' @param calibrate Logical. Should self-calibration be used? Default TRUE.
#' @param triple Logical. Should tripling be turned on? Default FALSE.
#' @param add_regen Logical. Should regeneration be modeled? Default FALSE.
#' @param customSDImax Optional. Maximum stand density index.
#' @param randomseed For replicability. Default 2025. To turn off, set to NULL.
#' @param outdir Directory to write files to.
#' @param file_prefix Optional. If provided, KEY and TRE files with temp names will have this prefix.
#' @param STDIDENT Optional. Stand Identity keyword to pass to FVS.
#' @param ... Additional optional arguments passed to write_FVS_KEY(). See details.
#'
#' @details
#' Optional arguments:
#' * `customSDImax`: Dataframe. `customSDImax$SP` is the species, c`ustomSDImax$MaxSDI` `is that species' SDI.
#' * `TIMEINT`: Named list. `TIMEINT$CYCLE_NUM` is the cycle whose length is to be changed. 0 changes the length for all cycles. `TIMEINT$CYCLE_LEN` is the
#' cycle length to change to.
#' * `READCORD`, `READCORR`: Vectors of numeric values. Length must be 23 to match the number of species in the variant.
#'
#' @returns The filename created for .key and .tre files, invisibly.
#' @export
write_FVS_files <- function(treelist, standinfo,
                            years_out=100,
                            calibrate=TRUE,
                            triple=FALSE,
                            add_regen=FALSE,
                            customSDImax=NULL,
                            randomseed=NULL,
                            STDIDENT = 'FVSProjection',
                            outdir,
                            file_prefix = NULL,
                            ...){
  stopifnot('Output directory does not exist' = dir.exists(outdir))
  stand <- set_FVSie_defaults(standinfo)

  # generate file names
  if(is.null(file_prefix)){
    filename <- tempfile(tmpdir = outdir)
  }else{
    filename <- file.path(outdir, file_prefix)
  }

  keyfilename <- paste0(filename, ".key")
  treefilename <- paste0(filename, ".tre")

  write_FVS_TRE(treelist, standinfo, treefilename = treefilename)
  write_FVS_KEY(stand = stand, years_out = years_out, calibrate = calibrate,
                triple = triple, add_regen = add_regen, customSDImax = customSDImax,
                randomseed = randomseed, STDIDENT = STDIDENT, ..., keyfilename = keyfilename)

  invisible(filename)
}

#' Write FVS Keyword File
#'
#' Given a stand list and FVS projection parameters, writes a keyword file.
#' This can then be inspected and passed to run_FVS().
#'
#' @param stand Dataframe. Stand data (for a single stand) for running FVS.
#' @param years_out How many years out should we project? Default 100.
#' @param calibrate Logical. Should self-calibration be used? Default TRUE.
#' @param triple Logical. Should tripling be turned on? Default FALSE.
#' @param add_regen Logical. Should regeneration be modeled? Default FALSE.
#' @param customSDImax Optional. Maximum stand density index. See details.
#' @param randomseed For replicability. Default 2025. To turn off, set to NULL.
#' @param STDIDENT Optional. Stand Identity keyword to pass to FVS.
#' @param ... Optional arguments. See details.
#' @param keyfilename .key file to write to.
#'
#' @keywords internal
#'
#' @details
#' Optional arguments:
#' * `customSDImax`: Dataframe. customSDImax$SP is the species, customSDImax$MaxSDI is that species' SDI.
#' * `TIMEINT`: Named list. TIMEINT$CYCLE_NUM is the cycle whose length is to be changed. 0 changes the length for all cycles. TIMEINT$CYCLE_LEN is the
#' cycle length to change to.
#' * `READCORD`, `READCORR`: Vectors of numeric values. Length must be 23 to match the number of species in the variant.
#'
#' @returns Keyword filename, invisibly.
write_FVS_KEY <- function(stand,
                          years_out,
                          calibrate,
                          triple,
                          add_regen,
                          customSDImax,
                          randomseed,
                          STDIDENT,
                          ...,
                          keyfilename){
  opt_args <- list(...)
  # stand identification
  write("STDIDENT", file = keyfilename, append = TRUE)
  write(STDIDENT,  file = keyfilename, append = TRUE)
  t1 <- sprintf("RANNSEED  %10.0f",randomseed)
  write(t1, file = keyfilename, append = TRUE)

  t1 <- sprintf("STDINFO   %10s%10s%10.1f%10.1f%10.1f%10.0f",
                stand$FOREST, stand$PV_CODE, stand$AGE, stand$ASPECT, stand$SLOPE, stand$ELEVFT)
  write(t1, file = keyfilename, append = TRUE)

  # tree list output file (with no headers = column 3 = -1)
  t1 <- "TREELIST           0         3         0         0         0         0         0"
  write(t1, file = keyfilename, append = TRUE)

  # tree list output file (with headers = column 3 = 0)
  t1 <- "TREEFMT"
  t2 <- "(I4,I4,F8.3,I1,A3,F5.1,F5.1,2F5.1,F5.1,I1,6I2,2I1,I2,2I3,2I1,F3.0)"   # use FIA species codes

  write(t1, file = keyfilename, append = TRUE)
  write(t2, file = keyfilename, append = TRUE)
  write(" ", file = keyfilename, append = TRUE)

  ### sample design
  t1 <- sprintf("DESIGN          -1.0         0         0%10i         0         0       1.0", stand$NUM_PLOTS)
  write(t1, file = keyfilename, append = TRUE)

  ### inventory year
  t1 <- sprintf("INVYEAR   %10i", stand$INV_YEAR)
  write(t1, file = keyfilename, append = TRUE)


  if(is.null(opt_args$TIMEINT)){
    cycle_length <- 10 # default
  }else{
    cycle_length <- opt_args$TIMEINT$CYCLE_LEN
    t1 <- sprintf("TIMEINT   %10i %10i", opt_args$TIMEINT$CYCLE_NUM, opt_args$TIMEINT$CYCLE_LEN)
    write(t1, file = keyfilename, append = TRUE)
  }

  # final year is a reporting year
  t1 <- sprintf("CYCLEAT   %10i", stand$INV_YEAR + years_out)
  write(t1, file = keyfilename, append = TRUE)

  ### bar tripling if necessary
  if (!triple){
    write("NOTRIPLE", file = keyfilename, append = TRUE)
  }

  ### ingrowth(regeneration)
  if (!add_regen){
    write("NOAUTOES", file = keyfilename, append = TRUE)
  } else {
    write("ESTAB", file = keyfilename, append = TRUE)
    if(!is.null(randomseed)){
      t1 <- sprintf("RANNSEED  %10.0f",randomseed)
      write(t1, file = keyfilename, append = TRUE)
    }
    write("NOINGROWTH", file = keyfilename, append = TRUE)
    write("END", file = keyfilename, append = TRUE)
  }

  ### SDI maximum
  if(!is.null(customSDImax))
  {
    j <- sprintf("SDIMAX  %10s%10i", customSDImax$SP, customSDImax$MaxSDI)
    cat(j, sep = "\n", file = keyfilename, append = TRUE)
  }

  ## calibration

  write(t1, file = keyfilename, append = TRUE)
  if (!calibrate){
    write("NOCALIB", file = keyfilename, append = TRUE)
    write("NOHTDREG", file = keyfilename, append = TRUE)
  }else{
    # GROWTH keyword:
    #> field 1: measurement method, diam
    #> field 2: length of diameter measurement period
    #> field 3: measurement method, ht
    #> field 4: length of height growth measurement period
    #> field 5: length of mortality observation period
    t1 <- sprintf("GROWTH    %10.0f%10.0f%10.0f%10.0f%10.0f",
                  stand$DG_TRANS, stand$DG_MEASURE,
                  stand$HTG_TRANS, stand$HTG_MEASURE,
                  stand$MORT_MEASURE)
    write(t1, file = keyfilename, append = TRUE)

    # Get calibration statistics in DB
    write('', file = keyfilename, append = TRUE)

    t1 <- sprintf('DATABASE  ')
    write(t1, file = keyfilename, append = TRUE)

    t1 <- sprintf('DSNOut     ')
    write(t1, file = keyfilename, append = TRUE)
    t1 <- paste0(stringr::str_pad('FVSOut.db', width = 10, side = 'right'),
                 sprintf('%10s%10s', '', ''))
    write(t1, file = keyfilename, append = TRUE)

    t1 <- sprintf('CALBSTDB  ')
    write(t1, file = keyfilename, append = TRUE)
    t1 <- sprintf('INVSTATS   ')
    write(t1, file = keyfilename, append = TRUE)

    t1 <- sprintf('END       ')
    write(t1, file = keyfilename, append = TRUE)
  }

  if(!is.null(opt_args$READCORD)){
    stopifnot('READCORD must have length 23' = length(opt_args$READCORD) == 23)
    t1 <- sprintf('READCORD  ')
    write(t1, file = keyfilename, append = TRUE)
    # 23 species --> 3 lines of 8 entries
    cat(opt_args$READCORD, sep = '', fill = 80, file = keyfilename, append = TRUE)
    write(' ', file = keyfilename, append = TRUE)
  }

  if(!is.null(opt_args$READCORR)){
    stopifnot('READCORR must have length 23' = length(opt_args$READCORR) == 23)
    t1 <- sprintf('READCORR  ')
    write(t1, file = keyfilename, append = TRUE)
    # 23 species --> 3 lines of 8 entries
    cat(opt_args$READCORR, sep = '', fill = 80, file = keyfilename, append = TRUE)
    write(' ', file = keyfilename, append = TRUE)
  }

  write('', file = keyfilename, append = TRUE)

  # number of cycles
  cycles <- ceiling(years_out / cycle_length)

  t1 <- sprintf("NUMCYCLE  %10i", cycles)
  write(t1, file = keyfilename, append = TRUE)


  write("PROCESS", file = keyfilename, append = TRUE)
  write("STOP", file = keyfilename, append = TRUE)
  invisible(keyfilename)
}

#' Writes tree list to .TRE file.
#'
#' @param treelist Tree list to write.
#' @param standinfo Stand table associated with tree list.
#' @param treefilename Destination for .TRE file.
#'
#' @keywords internal
#'
#' @returns The tree file name, invisibly.

write_FVS_TRE <- function(treelist, standinfo, treefilename)
{
  std <- set_FVSie_defaults(standinfo)
  tl <- clean_FIA_tree_list(treelist, std)

  # replace missing values with empties
  fvs_formats <- data.frame(tree_var = c("PLOT_ID","fvs.TREE_ID","TREE_COUNT","HISTORY","SPECIES",
                                       "DIAMETER","DG","HT","HTTOPK","HTG","CRcode",
                                       "DAMAGE1","SEVERITY1","DAMAGE2","SEVERITY2","DAMAGE3","SEVERITY3",
                                       "TVAL","CUT","SLOPE","ASPECT","PV_CODE","TOPO","SPREP","AGE"),
                            format = c("%4.0f","%4.0f","%8.3f","%1.0f","%3.0f",
                                     "%5.1f","%5.1f","%5.1f","%5.1f","%5.1f","%1.0f",
                                     "%2.0f","%2.0f","%2.0f","%2.0f","%2.0f","%2.0f",
                                     "%1.0f","%1.0f","%2.0f","%3.0f","%3.0f","%1.0f","%1.0f","%3.0f"))
  fvs_formats$txt_format <- with(fvs_formats,paste(substring(format,1,2),"s",sep = ""))


  for (var in 1:nrow(fvs_formats)){
    if(fvs_formats$tree_var[var] == 'TREE_COUNT'){
      dec <- 4-nchar(tl$TREE_COUNT) # get number of 0s to put after decimal
      tl$TREE_COUNT <- ifelse(is.na(tl$TREE_COUNT),
                              paste(rep(' ', substring(fvs_formats$format[var],2,2)), collapse = ''),
                              sprintf(paste0('%8.', dec, 'f'), tl$TREE_COUNT))
    }else{
      tl[,fvs_formats$tree_var[var]] <- ifelse(is.na(tl[,fvs_formats$tree_var[var]]),
                                               paste(rep(" ",substring(fvs_formats$format[var],2,2)),collapse=""),
                                               sprintf(fvs_formats$format[var],tl[,fvs_formats$tree_var[var]]))
    }
  }
  tl$SPECIES <- ifelse(as.numeric(tl$SPECIES)<100,
                       paste("0",as.numeric(tl$SPECIES),sep=""),tl$SPECIES)

  # write text file
  flat_format <- sprintf(paste(fvs_formats$txt_format,collapse = ""),
                         tl$PLOT_ID,tl$fvs.TREE_ID,tl$TREE_COUNT,tl$HISTORY,tl$SPECIES,
                         tl$DIAMETER,tl$DG,tl$HT,tl$HTTOPK,tl$HTG,tl$CRcode,
                         tl$DAMAGE1,tl$SEVERITY1,tl$DAMAGE2,tl$SEVERITY2,tl$DAMAGE3,tl$SEVERITY3,
                         tl$TVAL,tl$CUT,
                         tl$SLOPE,tl$ASPECT,tl$PV_CODE,tl$TOPO,tl$SPREP,tl$AGE)
  cat(flat_format, file=treefilename, sep="\n")
  invisible(treefilename)
}

#' Clean an FIA tree list to prepare for writing a .TRE file
#'
#' @param treelist Tree list, from FIA_TREEINIT_PLOT table (FVS_TreeInit from get_FIA())
#' @param standinfo Stand information associated with the tree list (FVS_StandInit from get_FIA())
#'
#' @keywords internal
#'
#' @returns A dataframe containing all tree information necessary to be input into FVS.
#'

clean_FIA_tree_list <- function(treelist, standinfo){
  # copy over information from the stand list
  out <- dplyr::select(standinfo,
                            .data$SLOPE, .data$ASPECT, .data$PV_CODE, .data$TOPO,
                            .data$STAND_CN) |>
    dplyr::right_join(treelist, by = 'STAND_CN') |>
    dplyr::mutate(SPREP = 0,
                  TVAL = 0,
                  CUT = 0,
                  PV_CODE = as.numeric(.data$PV_CODE),
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
  as.data.frame(out)
}

#' Replace NA values in stand tables with FVS-IE defaults for writing a stand list.
#'
#' @param stand Stand to set defaults for.
#'
#' @keywords internal
#'
#' @returns A dataframe.
#'
set_FVSie_defaults <- function(stand){
  out <- stand |>
    dplyr::mutate(ASPECT = ifelse(is.na(.data$ASPECT),
                                  yes = 0,
                                  no = .data$ASPECT),
                  SLOPE = ifelse(is.na(.data$SLOPE),
                                 yes = 5,
                                 no = .data$SLOPE),
                  # need elevation in 100s of feet for FVS, FIA gives in ft
                  ELEVFT = ifelse(is.na(.data$ELEVFT),
                                  yes = 38,
                                  no = .data$ELEVFT/100),
                  FOREST = ifelse(is.na(.data$FOREST),
                                  yes = 18,
                                  no = .data$FOREST)
    )
  out
}
