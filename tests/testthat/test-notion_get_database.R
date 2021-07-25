library(httr)
library(Rnotion)
library(jsonlite)

test_that("notion_get_database works", {
  expect_error(notion_get_database(query = "Totally Wrong!"))
  actual <- notion_get_database()
  expect_equal(httr::http_status(actual) ,httr::http_status(200))
  expect_true(httr::has_content(actual))
  expect_length(httr::content(actual)$results, 2)
})
