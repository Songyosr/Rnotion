#' Notion GET Database API
#'
#' @description Wrapper function for 'GET Database' API of Notion.so
#'
#' @param database_id a length 1 character vector represented an identifier for a database.
#' When not supplied, list all databases
#'
#' @param start_cursor a length 1 character vector. If supplied, this endpoint
#' will return a page of results starting after the cursor provided.
#' If not supplied, this endpoint will return the first page of results.
#'
#' @param page_size a length 1 integer vector. The number of items from the full
#' list desired in the response. Maximum: 100
#'
#' @param api_rate_limited The rate limit for incoming requests is an average of
#' 3 requests per second. Some bursts beyond the average rate are allowed.
#'
#' @details please check: \url{https://developers.notion.com/reference/get-database} and
#' \url{https://developers.notion.com/reference/get-databases}
#'
#' @examples
#' notion_get_database()
#'
#' @export
notion_get_database <- function(database_id = NULL,
                            start_cursor = NULL,
                            page_size = NULL,
                            api_rate_limited = 1/3){
  url <-  "https://api.notion.com/v1/databases"
  if(!is.null(database_id)) url <- paste(url, database_id, sep = "/")
  else url <- httr::modify_url(url, query = tibble::lst(start_cursor, page_size))

  #limit
  Sys.sleep(api_rate_limited)

  #Result
  result <- httr::POST(url, notion_header())
  httr::stop_for_status(result)
}

