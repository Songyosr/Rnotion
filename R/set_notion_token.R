#' Set the Notion token key
#'
#' @description Function to generate a request header for notion.so
#'
#' @param token a notion
#'
#' @details Review the official instruction in order to get your token
#' \url{https://developers.notion.com/docs/getting-started}
#'
#' @examples
#' set_notion_token()
#' set_notion_token(token = "AVBBPDSAFASKDDASD") # False token
#' @export
#'
set_notion_token <- function(token = NULL){
  key <- token %||% readline(prompt = "Enter your Notion API Key: ")
  text <- paste0("notion_api_key=", key, "\n")
  env <- Sys.getenv("notion_api_key")
  if (!file.exists(file.path(normalizePath("~/"), ".Renviron"))){
    file.create(file.path(normalizePath("~/"), ".Renviron"), showWarnings = TRUE)
  }
  if (!identical(env, "")) {
    renv <- readLines(file.path(normalizePath("~/"), ".Renviron"))
    loc <- grep("notion_api_key", renv)
    renv[loc] <- text
    Sys.setenv(notion_api_key = key)
    writeLines(renv, file.path(normalizePath("~/"), ".Renviron"))
  }
  else {
    Sys.setenv(notion_api_key = key)
    cat(text, file = file.path(normalizePath("~/"), ".Renviron"),
        append = TRUE)
  }
}


get_notion_token <- function(){
  KEY = Sys.getenv("notion_api_key")
  if (KEY == ""){
    stop("Please set your notion token with the set_notion_token function")
  }
  KEY
}


