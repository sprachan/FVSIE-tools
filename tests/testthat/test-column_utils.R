test_that("name checking works", {
  expect_true(check_name('PLOT ID', 'PLOT_ID'))
  expect_true(check_name('PLOT.ID', 'PLOT_ID'))
  expect_false(check_name('STANDPLOT.ID', 'STAND_ID'))
  expect_false(check_name('STANDPLOT.ID', 'PLOT_ID'))
  expect_true(check_name('DECAY_CODE', 'DECAYCD'))
  expect_true(check_name('DECAY.class', 'DECAY_CD'))
})
