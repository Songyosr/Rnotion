library(Rnotion)
library(jsonlite)

# Test sort_notion() function
test_that("Sort object works in a 'search API'", {
  # Define test case
  test_case1 <- '{
    "sort":{
      "direction":"ascending",
      "timestamp":"last_edited_time"
    }
  }' |> fromJSON(simplifyVector = F)

  # Check for search API
  expect_equal(
    sort_notion(
      direction = "ascending",
      timestamp = "last_edited_time"
    ) |> a_wrap(),
    test_case1
  )
  expect_equal(
    sort_notion(
      property = NULL,
      direction = "ascending",
      timestamp = "last_edited_time"
    ) |> a_wrap(),
    test_case1
  )
})

test_that("Sort object works in a 'datavase query API'", {
  test_case2 <- '{
  "sorts": [
	    {
	      "property": "Last ordered",
	      "direction": "ascending",
	      "timestamp": "created_time"
	    },
      {
	      "property": "Ingredients",
	      "direction": "descending",
	      "timestamp": "last_edited_time"
	    }

	  ]
	}' |> fromJSON(simplifyVector = F)

  # Coercing form
  expect_equal(
    c(
      sort_notion(
        property = "Last ordered", direction = "ascending",
        timestamp = "created_time"
      ),
      sort_notion(
        property = "Ingredients", direction = "descending",
        timestamp = "last_edited_time"
      )
    ) |> a_wrap(.names = "sorts"),
    test_case2
  )

  # 2-site vector form
  expect_equal(
    sort_notion(
      property = c("Last ordered", "Ingredients"),
      direction = c("ascending", "descending"),
      timestamp = c("created_time", "last_edited_time")
    ) |>
    a_wrap(.names = "sorts"),
    test_case2
  )
})
