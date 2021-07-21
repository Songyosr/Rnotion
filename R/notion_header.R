#' Function to generate a request header for notion.so
#'
#' @param version Notion version used in a request
#'
#' @details The Notion API is versioned and named for the date the version
#' is released. The current latest version is 2021-05-13.
#' please check: \url{https://developers.notion.com/reference/versioning}
#'
#' @examples
#' notion_header()
#'
#' @seealso "set_notin_token"
#' @export

notion_header <- \(version = "2021-05-13"){
  key <- get_notion_token()
  httr::add_headers(
    "Authorization" = paste("Bearer", key),
    "Notion-Version" = version,
    "Content-Type" = "application/json"
  )
}
