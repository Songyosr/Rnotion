#' Notion GET Database API
#'
#' @description Wrapper function for 'GET User' API of Notion.so
#'
#' @param user_id a length 1 character vector represented Identifier for a Notion user.
#' When not supplied, retrieve all users
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
#' @details please check: \url{https://developers.notion.com/reference/user}
#'
#' @examples
#' notion_get_user()
#'
#' @export
notion_get_user <- function(user_id = NULL,
                         start_cursor = NULL,
                         page_size = NULL,
                         api_rate_limited = 1/3){
  url <-  "https://api.notion.com/v1/users"
  if(!is.null(user_id)) url <- paste(url, user_id, sep = "/")
  else url <- httr::modify_url(url, query = tibble::lst(start_cursor, page_size))

  #limit
  Sys.sleep(api_rate_limited)

  #Result
  result <- httr::GET(url, notion_header())
  httr::stop_for_status(result)
}

