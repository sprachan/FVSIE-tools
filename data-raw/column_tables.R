# Code to prepare dataset of necessary columns for pre-FVS-run data checks
## Stand List Columns
fvs_stand_cols <- data.frame(col = c('STAND_ID', 'STANDPLOT_CN',
                                     'STANDPLOT_ID', 'PLOT_ID',
                                     'VARIANT', 'INV_YEAR',
                                     'GROUPS', 'ADD_FILES', 'FVS_KEYWORDS',
                                     'LATITUDE', 'LONGITUDE',
                                     'REGION', 'FOREST',
                                     'HABITAT_TYPE', 'PV_REF_CODE',
                                     'AGE',
                                     'ASPECT', 'SLOPE', 'ELEVATION', 'ELEV_FT',
                                     'BASAL_AREA_FACTOR', 'INV_PLOT_SIZE',
                                     'BRK_DBH', 'NUM_PLOTS', 'NONSTK_PLOTS',
                                     'SAM_WT', 'STK_PCNT',
                                     'DG_TRANS', 'DG_MEASURE',
                                     'HTG_TRANS', 'HTG_MEASURE',
                                     'MORT_MEASURE',
                                     'MAX_BA', 'MAX_SDI', 'SITE_SPECIES', 'SITE_INDEX',
                                     'MODEL_TYPE', 'PHYSIO_REGION', 'FOREST_TYPE',
                                     'STATE', 'COUNTY',
                                     'FUEL_MODEL', 'FUEL_0_25', 'FUEL_0_25',
                                     'FUEL_0_25_1', 'FUEL_0_1','FUEL_1_3',
                                     'FUEL_3_6', 'FUEL_6_12', 'FUEL_12_20',
                                     'FUEL_GT_12', 'FUEL_20_35', 'FUEL_35_50',
                                     'FUEL_GT_50', 'FUEL_LITTER', 'FUEL_DUFF',
                                     'PHOTO_REF', 'PHOTO_CODE',
                                     'STAND_ORIGIN_CODE'),
                             default = c(9999, NA,
                                         NA, NA,
                                         'IE', 9999,
                                         NA, NA, NA,
                                         46, 116,
                                         1, 18,
                                         260, NA,
                                         NA,
                                         0, 5, 38, 3800,
                                         40, 300,
                                         5, 0, 1,
                                         9999, 100,
                                         0, 10,
                                         0, 5,
                                         5,
                                         NA, NA, NA, NA,
                                         NA, NA, NA,
                                         22, 9999,
                                         NA, NA, NA,
                                         NA, NA, NA,
                                         NA, NA, NA,
                                         NA, NA, NA,
                                         NA, NA, NA,
                                         NA, NA,
                                         NA),
                             has_default = TRUE,
                             alt_col = '')
fvs_stand_cols$has_default[fvs_stand_cols$default == 9999] <- FALSE
fvs_stand_cols$alt_col[fvs_stand_cols$col == 'HABITAT_TYPE'] <- 'PV_CODE'
fvs_stand_cols$alt_col[fvs_stand_cols$col == 'STAND_ID'] <- 'STAND_CN'

## Tree List Columns
fvs_tree_cols <- data.frame(col = c('STAND_ID', 'STANDPLOT_CN',
                                    'STANDPLOT_ID', 'PLOT_ID',
                                    'TREE_ID', 'TREE_COUNT', 'HISTORY', 'SPECIES',
                                    'DBH', 'DG', 'HT', 'HTG', 'HTTOPK',
                                    'CR_RATIO',
                                    'DAMAGE1', 'SEVERITY1',
                                    'DAMAGE2', 'SEVERITY2',
                                    'DAMAGE3', 'SEVERITY3',
                                    'TREE_VALUE', 'PRESCRIPTION',
                                    'AGE',
                                    'SLOPE', 'ASPECT', 'HABITAT_TYPE',
                                    'TOPO_CODE', 'SITE_PREP',
                                    'CULL', 'DECAY_CODE', 'WOODLAND_STEMS'),
                            default = c(9999, NA,
                                        NA, 9999,
                                        9999, 1, 9999, 23,
                                        9999, NA, 9999, NA, NA,
                                        NA,
                                        NA, NA,
                                        NA, NA,
                                        NA, NA,
                                        NA, NA,
                                        NA,
                                        5, 0, 260,
                                        NA, NA,
                                        NA, NA, NA),
                            has_default = TRUE,
                            alt_col = '')

fvs_tree_cols$has_default[fvs_tree_cols$default == 9999] <- FALSE
fvs_tree_cols$alt_col[fvs_tree_cols$col == 'STAND_ID'] <- 'STAND_CN'
fvs_tree_cols$alt_col[fvs_tree_cols$col == 'DBH'] <- 'DIAMETER'
fvs_tree_cols$alt_col[fvs_tree_cols$col == 'HT'] <- 'HEIGHT'
fvs_tree_cols$alt_col[fvs_tree_cols$col == 'WOODLAND_STEMS'] <- 'WDLND_STEMS'
fvs_tree_cols$alt_col[fvs_tree_cols$col == 'HABITAT_TYPE'] <- 'PV_CODE'

## Save data to R/sysdata.rda for internal use
usethis::use_data(fvs_stand_cols, fvs_tree_cols, overwrite = TRUE,
                  internal = TRUE)
