test_that("error if database tables not found", {
  expect_error(get_FIA_state(system.file('extdata', 'ex_data.db', package = 'rFVSIEtools'),
               fetch_cond(system.file('extdeata', 'dummy_fia.db', package = 'rFVSIEtools'))))
})

test_that("outputs list of dataframes", {
  expect_type(get_FIA_state(system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools'),
                            fetch_cond(system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools'))),
              'list')
  expect_s3_class(get_FIA_state(system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools'),
                            fetch_cond(system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools')))[[1]],
              'data.frame')
  expect_s3_class(get_FIA_state(system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools'),
                                fetch_cond(system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools')))[[2]],
                  'data.frame')
})

test_that("returns NULL if no matching plots found", {
  expect_null(get_FIA_state(system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools'),
                            fetch_cond(system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools'), "STATECD == 17"))[[1]])
  expect_null(get_FIA_state(system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools'),
                            fetch_cond(system.file('extdata', 'dummy_fia.db', package = 'rFVSIEtools'), "STATECD == 17"))[[2]])
})
