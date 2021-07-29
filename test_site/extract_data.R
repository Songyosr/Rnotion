# PULL initial
# pull_face <- function(jsondata) {
#   data <- httr::content(jsondata)
#   tibble::tibble(result = data[["results"]]) |>
#     tidyr::hoist(result, object = "object",
#           id = "id", parent = list("parent", "type"),
#           properties = list("properties"))
# }
jx_remove <- function() {
  FALSE
}

#' @export
pull_content <- function(response, content = "results",...){
  if(!inherits(response, "response")) return(response)
  #data <- httr::content(response)[[content]]
  tibble::tibble(jsondata = httr::content(response)[[content]])
}

#' @export
pull_things <- function(.data, ... ,
                        .remove = jx_remove()) {
  .data <- pull_content(.data)
  args <- list(...)
  new_args <- setdiff(names(args), names(.data))
  #print(new_args)
  if(length(new_args) != length(args)) {
    warning("The following columns [",
            paste(setdiff(names(args), new_args), collapse = ", "),
            "] are existed and thier hoisting function will be ignored")

  }
  args <- args[new_args]
  args[[".data"]] <- .data
  args[[".col"]] <- "jsondata"
  args[[".remove"]] <- .remove
  do.call(tidyr::hoist, args)
  #tidyr::hoist(.data, jsondata, ... , .remove = .remove)
}

pull_object <- function(.data, remove = jx_remove()) {
  .data <- pull_content(.data, remove = remove)
  tidyr::hoist(.data, jsondata, object = "object",
               .remove = remove)
}

pull_id <- function(.data, remove = jx_remove()) {
  .data <- pull_content(.data, remove = remove)
  tidyr::hoist(.data, jsondata, id = "id",
               .remove = remove)
}




#' @export
pull_surface <- function(.data, remove = jx_remove()) {
  .data <- pull_content(.data)
  .data |>
    pull_object() |>
    pull_name()
  #tibble::tibble(data = data[["results"]]) |>
  # tidyr::hoist(.data, data, object = "object",
  #              id = "id",
  #              #parent = list("parent", "type"),
  #              .remove = remove)
}




pull_parent <- function(.data, remove = jx_remove()) {
  .data <- pull_content(.data, remove = remove)
  #tibble::tibble(data = data[["results"]]) |>
  tidyr::hoist(.data, jsondata,
               parent = list("parent", "type"),
               parent_id = list("parent", "page_id"),
               .remove = remove)
}

# DATABASE REQUEST
pull_name <- function(.data, object_col = "object", remove = jx_remove()) {

  # Check and transform data
  .data <- pull_content(.data, remove = remove)
  if(!"object" %in% names(.data)) .data <- pull_surface(.data,remove = remove)

  # Add name
  rbind(
    filter(.data, !!sym(object_col) == "user") |> pull_name_user(remove = remove),
    filter(.data, !!sym(object_col) == "database") |> pull_name_database(remove = remove),
    filter(.data, !!sym(object_col) == "page") |> pull_name_page(remove = remove)
  )
}


# Sub-function for name pulling
pull_name_user <- function(.data, remove) {
  tidyr::hoist(.data, jsondata,
               name = "name",
               .remove = remove)
}

pull_name_database <- function(.data, remove) {
  tidyr::hoist(.data, jsondata,
               title = list("title",1, "plain_text"),
               .remove = remove)
}

pull_name_page <- function(.data, remove) {
  tidyr::hoist(.data, jsondata,
               title = list("properties",1, "title", 1,"plain_text"),
               .remove = remove)
}
