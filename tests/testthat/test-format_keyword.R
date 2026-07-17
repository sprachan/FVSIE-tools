test_that("errors when fields and dtypes don't match", {
 expect_error(format_keyword('SDICALC', 0, dtypes = c('f', 'i', 'i')))
 expect_error(format_keyword('SDICALC', 0, 1, 2, dtypes = c('f')))
})

test_that("string fixing works",{
  expect_equal(format_keyword('foo bar'), 'FOOBAR    ')
})

test_that("errors when inputs are too long", {
  expect_error(format_keyword('TOOLONGKWDS'))
  expect_error(format_keyword('OKKWD', 'toolongfieldentry'))
})
