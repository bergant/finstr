context("Create statements")

test_that("Read statements from XBRL data", {
  data(xbrl_data_aapl2014)
  test_statement <- xbrl_get_statements(xbrl_data_aapl2014)
  expect_more_than(length(test_statement), 2)
  expect_is(test_statement, "statements")
  expect_is(test_statement[[1]], "statement")
  expect_output(test_statement[[1]], "Financial")
})


context("Expose")

test_that("Expose", {
  data(xbrl_data_aapl2014)
  test_statement <- xbrl_get_statements(xbrl_data_aapl2014)
  expect_more_than(ncol(expose(test_statement[[1]], "nothing")), 2)
})