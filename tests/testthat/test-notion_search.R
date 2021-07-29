library(httr)
library(Rnotion)
library(jsonlite)

test_that("notion_search works", {
  expect_error(notion_search(query = "Totally Wrong!"))
  actual <- notion_get_user()
  expect_equal(httr::http_status(actual) ,httr::http_status(200))
  expect_true(httr::has_content(actual))
  expect_length(httr::content(actual)$results, 2)
})
