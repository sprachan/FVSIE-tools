ex_stand <- readRDS('../fvs-code/fia_calib/data/fvs_ready/FVS_StandInit_IE.rds')[1,]
ex_trees <-  readRDS('../fvs-code/fia_calib/data/fvs_ready/FVS_TreeInit_IE.rds') |>
  dplyr::filter(STAND_CN %in% ex_stand$STAND_CN)

usethis::use_data(ex_stand, ex_trees, internal = FALSE, overwrite = TRUE)
