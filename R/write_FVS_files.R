#' Write keyword files and treelist files.
#'
#' This function, wrapped in [run_FVS()], writes and saves .key and .tre files
#' that FVS-IE uses to initialize and carry out a simulation. Use outside of
#' `run_FVS()` when diagnostics on these files are needed.
#'
#' @param tree_list Tree list to be run through FVS, e.g., FVS_TreeInit output
#'   from get_FIA()
#' @param stand_info Stand data to be run through FVS, e.g., FVS_StandInit
#'   output from get_FIA()
#' @param proj_len How many years ahead should we project?
#' @param calibrate Logical. Should self-calibration be used? Default TRUE.
#' @param triple Logical. Should tripling be turned on? Default FALSE.
#' @param add_regen Logical. Should regeneration be modeled? Default FALSE.
#' @param custom_SDI_max Optional. Maximum stand density index.
#' @param random_seed For replicability. Default 2025. To turn off, set to NULL.
#' @param out_dir Directory to write files to.
#' @param file_prefix Optional. If provided, KEY and TRE files with temp names
#'   will have this prefix.
#' @param STDIDENT Optional. Stand Identity keyword to pass to FVS.
#' @param ... Additional optional arguments passed to [write_FVS_KEY()]. See
#'   details.
#'
#' @details Optional arguments:
#' * `custom_SDI_max`: Dataframe. `custom_SDI_max$SP` is the species, `
#'   `Custom_SDI_max$MaxSDI` is that species' SDI.
#' * `TIMEINT`: Named list. `TIMEINT$CYCLE_NUM` is the cycle whose length is to
#'   be changed. 0 changes the length for all cycles. `TIMEINT$CYCLE_LEN` is the
#'   cycle length to change to.
#' * `READCORD`, `READCORR`: Vectors of numeric values. Length must be 23 to
#'   match the number of species in the variant.
#' * `CYCLEAT`: Additional reporting year.
#'
#' @returns The filename created for .key and .tre files, invisibly.
#' @export
write_FVS_files <- function(tree_list, stand_info, proj_len = 100,
                            calibrate = TRUE, triple = FALSE,
                            add_regen = FALSE, custom_SDI_max = NULL,
                            random_seed = NULL, STDIDENT = 'FVSProjection',
                            out_dir, file_prefix = NULL, ...){
  stopifnot('Output directory does not exist' = dir.exists(out_dir))
  stand <- set_FVSie_defaults(stand_info)

  # generate file names
  if(is.null(file_prefix)){
    filename <- tempfile(tmpdir = out_dir)
  }else{
    filename <- file.path(out_dir, file_prefix)
  }

  keyfile_name <- paste0(filename, ".key")
  treefile_name <- paste0(filename, ".tre")

  write_FVS_TRE(tree_list, stand_info, treefile_name = treefile_name)
  write_FVS_KEY(stand = stand, proj_len = proj_len, calibrate = calibrate,
                triple = triple, add_regen = add_regen, custom_SDI_max = custom_SDI_max,
                random_seed = random_seed, STDIDENT = STDIDENT, ..., keyfile_name = keyfile_name)

  invisible(filename)
}

#' Write FVS Keyword File
#'
#' Given a stand list and FVS projection parameters, writes a keyword file.
#' This can then be inspected and passed to run_FVS().
#'
#' @param stand Dataframe. Stand data (for a single stand) for running FVS.
#' @param proj_len How long, in years, should the projection be? Default 100.
#' @param calibrate Logical. Should self-calibration be used? Default TRUE.
#' @param triple Logical. Should tripling be turned on? Default FALSE.
#' @param add_regen Logical. Should regeneration be modeled? Default FALSE.
#' @param custom_SDI_max Optional. Maximum stand density index. See details.
#' @param random_seed For replicability. Default 2025. To turn off, set to NULL.
#' @param STDIDENT Optional. Stand Identity keyword to pass to FVS.
#' @param ... Optional arguments. See details.
#' @param keyfile_name .key file to write to.
#'
#' @keywords internal
#'
#' @details
#' Optional arguments:
#' * `custom_SDI_max`: Dataframe. custom_SDI_max$SP is the species,
#'  `custom_SDI_max$MaxSDI` is that species' SDI.
#' * `TIMEINT`: Named list. TIMEINT$CYCLE_NUM is the cycle whose length is to
#'   be changed. 0 changes the length for all cycles. TIMEINT$CYCLE_LEN is the
#'   cycle length to change to.
#' * `READCORD`, `READCORR`: Vectors of numeric values. Length must be 23 to
#'   match the number of species in the variant.
#'  * `CYCLEAT`: Additional reporting year.
#'
#' @returns Keyword filename, invisibly.
write_FVS_KEY <- function(stand,
                          proj_len,
                          calibrate,
                          triple,
                          add_regen,
                          custom_SDI_max,
                          random_seed,
                          STDIDENT,
                          ...,
                          keyfile_name){
  opt_args <- list(...)
  # stand identification
  write("STDIDENT", file = keyfile_name, append = TRUE)
  write(STDIDENT,  file = keyfile_name, append = TRUE)
  t1 <- sprintf("RANNSEED  %10.0f",random_seed)
  write(t1, file = keyfile_name, append = TRUE)

  t1 <- sprintf("STDINFO   %10s%10s%10.1f%10.1f%10.1f%10.0f",
                stand$FOREST, stand$PV_CODE, stand$AGE, stand$ASPECT, stand$SLOPE, stand$ELEVFT)
  write(t1, file = keyfile_name, append = TRUE)

  # tree list output file (with no headers = column 3 = -1)
  t1 <- "TREELIST           0         3         0         0         0         0         0"
  write(t1, file = keyfile_name, append = TRUE)

  # tree list output file (with headers = column 3 = 0)
  t1 <- "TREEFMT"
  t2 <- "(I4,I4,F8.3,I1,A3,F5.1,F5.1,2F5.1,F5.1,I1,6I2,2I1,I2,2I3,2I1,F3.0)"   # use FIA species codes

  write(t1, file = keyfile_name, append = TRUE)
  write(t2, file = keyfile_name, append = TRUE)
  write(" ", file = keyfile_name, append = TRUE)

  ### sample design
  t1 <- sprintf("DESIGN          -1.0         0         0%10i         0         0       1.0", stand$NUM_PLOTS)
  write(t1, file = keyfile_name, append = TRUE)

  ### inventory year
  t1 <- sprintf("INVYEAR   %10i", stand$INV_YEAR)
  write(t1, file = keyfile_name, append = TRUE)


  if(is.null(opt_args$TIMEINT)){
    cycle_length <- 10 # default
  }else{
    cycle_length <- opt_args$TIMEINT$CYCLE_LEN
    t1 <- sprintf("TIMEINT   %10i %10i", opt_args$TIMEINT$CYCLE_NUM, opt_args$TIMEINT$CYCLE_LEN)
    write(t1, file = keyfile_name, append = TRUE)
  }


  ### bar tripling if necessary
  if (!triple){
    write("NOTRIPLE", file = keyfile_name, append = TRUE)
  }

  ### ingrowth(regeneration)
  if (!add_regen){
    write("NOAUTOES", file = keyfile_name, append = TRUE)
  } else {
    write("ESTAB", file = keyfile_name, append = TRUE)
    if(!is.null(random_seed)){
      t1 <- sprintf("RANNSEED  %10.0f",random_seed)
      write(t1, file = keyfile_name, append = TRUE)
    }
    write("NOINGROWTH", file = keyfile_name, append = TRUE)
    write("END", file = keyfile_name, append = TRUE)
  }

  ### SDI maximum
  if(!is.null(custom_SDI_max))
  {
    j <- sprintf("SDIMAX  %10s%10i", custom_SDI_max$SP, custom_SDI_max$MaxSDI)
    cat(j, sep = "\n", file = keyfile_name, append = TRUE)
  }

  ## calibration

  if (!calibrate){
    write("NOCALIB", file = keyfile_name, append = TRUE)
    write("NOHTDREG", file = keyfile_name, append = TRUE)
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
    write(t1, file = keyfile_name, append = TRUE)

    # Get calibration statistics in DB
    write('', file = keyfile_name, append = TRUE)

    t1 <- sprintf('DATABASE  ')
    write(t1, file = keyfile_name, append = TRUE)

    t1 <- sprintf('DSNOut     ')
    write(t1, file = keyfile_name, append = TRUE)
    t1 <- paste0(stringr::str_pad('FVSOut.db', width = 10, side = 'right'),
                 sprintf('%10s%10s', '', ''))
    write(t1, file = keyfile_name, append = TRUE)

    t1 <- sprintf('CALBSTDB  ')
    write(t1, file = keyfile_name, append = TRUE)
    t1 <- sprintf('INVSTATS   ')
    write(t1, file = keyfile_name, append = TRUE)

    t1 <- sprintf('END       ')
    write(t1, file = keyfile_name, append = TRUE)
  }

  if(!is.null(opt_args$READCORD)){
    stopifnot('READCORD must have length 23' = length(opt_args$READCORD) == 23)
    t1 <- sprintf('READCORD  ')
    write(t1, file = keyfile_name, append = TRUE)
    # 23 species --> 3 lines of 8 entries
    cat(opt_args$READCORD, sep = '', fill = 80, file = keyfile_name, append = TRUE)
    write(' ', file = keyfile_name, append = TRUE)
  }

  if(!is.null(opt_args$READCORR)){
    stopifnot('READCORR must have length 23' = length(opt_args$READCORR) == 23)
    t1 <- sprintf('READCORR  ')
    write(t1, file = keyfile_name, append = TRUE)
    # 23 species --> 3 lines of 8 entries
    cat(opt_args$READCORR, sep = '', fill = 80, file = keyfile_name, append = TRUE)
    write(' ', file = keyfile_name, append = TRUE)
  }

  write('', file = keyfile_name, append = TRUE)

  # number of cycles
  cycles <- ifelse(is.null(opt_args$CYCLEAT),
                   ceiling(proj_len/cycle_length),
                   ceiling(max(opt_args$CYCLEAT-stand$INV_YEAR, proj_len)/cycle_length))

  t1 <- sprintf("NUMCYCLE  %10i", cycles)
  write(t1, file = keyfile_name, append = TRUE)

  if(!is.null(opt_args$CYCLEAT)){
    for(i in seq_along(opt_args$CYCLEAT)){
      t1 <- sprintf("CYCLEAT %10i", opt_args$CYCLEAT)
      write(t1, file = keyfile_name, append = TRUE)
    }
  }


  write("PROCESS", file = keyfile_name, append = TRUE)
  write("STOP", file = keyfile_name, append = TRUE)
  invisible(keyfile_name)
}

#' Writes tree list to .TRE file.
#'
#' @param tree_list Tree list to write.
#' @param stand_info Stand table associated with tree list.
#' @param treefile_name Destination for .TRE file.
#'
#' @keywords internal
#'
#' @returns The tree file name, invisibly.

write_FVS_TRE <- function(tree_list, stand_info, treefile_name)
{
  std <- set_FVSie_defaults(stand_info)
  tl <- clean_FIA_tree_list(tree_list, std)

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
  cat(flat_format, file=treefile_name, sep="\n")
  invisible(treefile_name)
}

#' Clean an FIA tree list to prepare for writing a .TRE file
#'
#' @param tree_list Tree list, from FIA_TREEINIT_PLOT table
#' (FVS_TreeInit from get_FIA())
#' @param stand_info Stand information associated with the tree list
#' (FVS_StandInit from get_FIA())
#'
#' @keywords internal
#'
#' @returns A dataframe containing all tree information necessary to be input
#' into FVS.
#'

clean_FIA_tree_list <- function(tree_list, stand_info){
  stopifnot('STAND_CN' %in% colnames(tree_list))
  stand_cols <- c('SLOPE', 'ASPECT', 'TOPO')
  # if any of these are missing from tree list, get them from stand table
  if(any(!(stand_cols %in% colnames(tree_list)))){
    needed_cols <- stand_cols[!stand_cols %in% colnames(tree_list)]
    tree_list <- tree_list |>
      dplyr::left_join(stand_info[c(needed_cols, 'STAND_CN')],
                       by = 'STAND_CN')
  }
  out <- tree_list |>
    dplyr::select(-PV_CODE) |>
    dplyr::left_join(stand_info[c('PV_CODE', 'STAND_CN')], by = 'STAND_CN') |>
    dplyr::mutate(SPREP = 0,
                  TVAL = 0,
                  CUT = 0,
                  # FVS tree init PV is always NA, so need to get from stand info
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
  out$fvs.TREE_ID <- 1:nrow(tree_list)
  as.data.frame(out)
}

#' Replace NA values in stand tables with FVS-IE defaults for writing a stand list.
#'
#' @param stand_info Stand to set defaults for.
#'
#' @keywords internal
#'
#' @returns A dataframe.
#'
set_FVSie_defaults <- function(stand_info){
  stand_info |>
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
                                  no = .data$FOREST))
}
