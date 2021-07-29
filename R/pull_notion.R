#' @export
pull_content <- function(response, content = "results", ...) {

  # Data check
  if (!inherits(response, "response")) {
    return(response)
  }
  data <- httr::content(response)

  if(!content %in% names(data)) return(tibble(jsondata = list(data)))
  #.data[["has_more"]] <- .data[["has_more"]] %||% FALSE

  # warning for more pages
  if (data[["has_more"]]) {
    info(
      "There are more pages to be queried. Supply start_cursor = '",
      data[["next_cursor"]],"' in the API function for the next part\n"
    )
  }

  tibble::tibble(jsondata = data[[content]])
}

#' @export
pull_things <- function(.data, ...,
                        .remove = FALSE) {
  .data <- pull_content(.data)
  args <- list(...)
  new_args <- setdiff(names(args), names(.data))
  # print(new_args)
  if (length(new_args) != length(args)) {
    warning(
      "The following columns [",
      paste(setdiff(names(args), new_args), collapse = ", "),
      "] are already existed. The new pulling will be ignored"
    )
  }
  args <- args[new_args]
  args[[".data"]] <- .data
  args[[".col"]] <- "jsondata"
  args[[".remove"]] <- .remove

  do.call(tidyr::hoist, args)
  # tidyr::hoist(.data, jsondata, ... , .remove = .remove)
}


# 1step pull --------------------------------------------------------------

#' @export
pull_object <- function(.data, ...) {
  pull_things(.data, object = "object", ...)
}

#' @export
pull_id <- function(.data, ...) {
  pull_things(.data, id = "id", ...)
}

#' @export
pull_parent <- function(.data, ...) {
  pull_things(.data,
    parent = list("parent", "type"),
    parent_id = list("parent", 2), ...
  )
}

#' @export
pull_properties <- function(.data, ...) {
  pull_things(.data, properties = list("properties"), ...)
}


# conditional pull --------------------------------------------------------

# Pull_name
#' @export
pull_name <- function(.data, object_col = "object", ...) {

  # Check and transform data
  .data <- pull_object(.data)
  #str(.data, max.level = 2) |>

  rbind(
    dplyr::filter(.data, !!rlang::sym(object_col) == "user") |> pull_name_user(...),
    dplyr::filter(.data, !!rlang::sym(object_col) == "database") |> pull_name_database(...),
    dplyr::filter(.data, !!rlang::sym(object_col) == "page") |> pull_name_page(...)
  )
}

# sub function
pull_name_user <- function(.data, ...) {
  pull_things(.data, name = "name", ...)
}

pull_name_database <- function(.data, ...) {
  pull_things(.data,
    title = list("title", 1, "plain_text"),
    ...
  )
}

pull_name_page <- function(.data, ...) {

  if(nrow(.data) ==0) return(NULL)

  pos <- .data[["jsondata"]][[1]][["properties"]]  |>
    purrr::map_chr(c("id")) |>
    (\(x){which(x == "title")})()
  print(str(.data[["jsondata"]], max.level = 2))

  pull_things(.data,
    title = list("properties", pos,"title", 1, "plain_text"),
    ...
  )
}
