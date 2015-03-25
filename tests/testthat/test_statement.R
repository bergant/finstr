context("Create statements")

test_that("Read statements from XBRL data", {
  data(xbrl_data_aapl2014)
  test_statement <- xbrl_get_statements(xbrl_data_aapl2014)
  expect_more_than(length(test_statement), 2)
  expect_is(test_statement, "statements")
  expect_is(test_statement[[1]], "statement")
  expect_output(test_statement[[1]], "Financial")
})

context("Merging")

test_that("Merge", {
  data(xbrl_data_aapl2013)
  data(xbrl_data_aapl2014)
  st1 <- xbrl_get_statements(xbrl_data_aapl2013)
  st2 <- xbrl_get_statements(xbrl_data_aapl2014)
  st_all <- merge(st1, st2)
  expect_true( nrow(st_all[[1]]) > nrow(st1[[1]]), "merge" )
  nrow(st1[[1]]) 
  nrow(st2[[1]])
})  

context("Expose")

test_that("Expose", {
  data(xbrl_data_aapl2014)
  test_statement <- xbrl_get_statements(xbrl_data_aapl2014)
  expect_more_than(ncol(expose(test_statement[[1]], "nothing")), 2)
})