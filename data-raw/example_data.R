ex_stand <- readRDS('../fvs-code/fia_calib/data/fvs_ready/FVS_StandInit_IE.rds')[1,]
ex_trees <-  readRDS('../fvs-code/fia_calib/data/fvs_ready/FVS_TreeInit_IE.rds') |>
  dplyr::filter(STAND_CN %in% ex_stand$STAND_CN) |>
  dplyr::mutate(SPECIES = as.integer(SPECIES))

usethis::use_data(ex_stand, ex_trees, internal = FALSE, overwrite = TRUE)

ex_stands <- readRDS('../fvs-code/fia_calib/data/fvs_ready/FVS_StandInit_IE.rds')[1:3,]
ex_trees <-  readRDS('../fvs-code/fia_calib/data/fvs_ready/FVS_TreeInit_IE.rds') |>
  dplyr::filter(STAND_CN %in% ex_stands$STAND_CN) |>
  dplyr::mutate(SPECIES = as.integer(SPECIES))

conn <- DBI::dbConnect(RSQLite::SQLite(), 'inst/extdata/ex_data.db')
DBI::dbWriteTable(conn, 'FVS_StandInit', ex_stands, overwrite = TRUE)
DBI::dbWriteTable(conn, 'FVS_TreeInit', ex_trees, overwrite = TRUE)

DBI::dbDisconnect(conn)
