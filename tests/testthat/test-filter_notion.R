library(Rnotion)
library(jsonlite)

# Test sort_notion() function
test_that("filter object works in a 'search API'", {
  # Define test case
  test_case1 <- '{
    "filter":{
      "property":"object",
      "value":"database"
    }
  }' |> fromJSON(simplifyVector = F)

  # Check for search API
  expect_equal(
    filter_notion(
      property = "object",
      value = "database"
    ) |> a_wrap("filter"),
    test_case1
  )
})
