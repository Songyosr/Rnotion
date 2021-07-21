notion_search <- function(query = NULL, sort = NULL, filter = NULL,
                          start_cursor = NULL, page_size = NULL, ...) {
  # Check query terms
  ## scalar
  query <- query %|null|% assertive.types::assert_is_a_string(query)
  start_cursor <- start_cursor %|null|% assertive.types::assert_is_a_string(start_cursor)
  page_size <- page_size %|null|% assertive.types::assert_is_an_integer(page_size)

  ## object
  sort <- sort %|null|% validate_checkpoint(sort)
  filter <- filter %|null|% validate_checkpoint(filter)

  # Coerce argument into a list
  args <- tibble::lst(
    query, sort, filter,
    start_cursor, page_size, ...
  ) |> rlist::list.clean()

  #print(args)

  httr::POST("https://api.notion.com/v1/search",
    notion_header(),
    body = jsonlite::toJSON(args,auto_unbox = TRUE),
    encode = "raw"
  )
}






