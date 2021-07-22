#' #' Internal vctrs methods
#' #'
#' #' @import vctrs
#' #' @keywords internal
#' #' @name notion-vctrs
#' NULL


methods::setOldClass(c("notion_sort", "vctrs_vctr"))

new_sort_notion <- function(property = character(),
                            direction = character(),
                            timestamp = character()) {
  vec_assert(property, ptype = character())
  vec_assert(direction, ptype = character())
  vec_assert(timestamp, ptype = character())
  # print(property)

  new_rcrd(list(
    property = property,
    direction = direction,
    timestamp = timestamp
  ), class = "notion_sort")
}

#' Notion Sort object generator
#'
#' @description A helper function for creating 'Sort objects' used
#' by Notion API in Search and Query.
#'
#' @param property A character vector represented the name of the
#' property to sort against. Should be omitted, when used inside
#' \code{\link{notion_search}} function.
#'
#' @param direction A character vector represented the direction to sort.
#' Either \code{"ascending"} and \code{"descending"}.
#'
#' @param timestamp A character vector represented The name of the timestamp
#' to sort against. Either \code{"created_time"} and \code{"last_edited_time"}.
#'
#' @details please check:
#' \url{https://developers.notion.com/reference/post-search} and
#' \url{https://developers.notion.com/reference/post-database-query}
#'
#' @examples
#'
#' # Size of one
#' sort_1 <- sort_notion(property = "Ingredients", direction = "descending", timestamp = "last_edited_time")
#' sort_1
#'
#' # Size of N (all element should have an equal length or 1)
#' sort_n <- sort_notion(
#'   property = LETTERS[1:4],
#'   direction = "ascending",
#'   timestamp = rep(c("last_edited_time", "created_time"), 2)
#' )
#'
#' # Coercing sort criteria with 'c()'
#' c(sort_1, sort_n)
#'
#' # Omit properties when use inside notion search
#' notion_search(
#'   sort = sort_notion(direction = "ascending", timestamp = "last_edited_time"),
#'   page_size = 4
#' )
#' @export
sort_notion <- function(property = NA,
                        direction = character(),
                        timestamp = character()) {

  # Casting Character
  c(property, direction, timestamp) %<-%
    vec_cast_common(property, direction, timestamp, .to = character())

  # Check for range 0
  c(property, direction, timestamp) %<-%
    vec_recycle_common(property, direction, timestamp)

  # cat("3. " ,property, "|" , direction, "|" , timestamp, "\n")

  new_sort_notion(property, direction, timestamp)
}


# Sort Validators
#' @export
validate_notion_sort <- function(x, fromSearchAPI = FALSE,
                                 severity = "warning",
                                 prepare_toJSON = FALSE) {
  if (tmp <- !assertive.types::is_inherited_from(x, "notion_sort")) {
    warning(
      attr(tmp, "cause"),
      "\n  [i] Please use 'notion_sort()' function to create sort regimes to ensure the correctness"
    )
  }

  property <- field(x, "property")
  direction <- field(x, "direction")
  timestamp <- field(x, "timestamp")

  # For 'Search API'
  if (fromSearchAPI) {
    assertive.properties::assert_is_scalar(x)
    assertive.base::assert_are_identical(timestamp, "last_edited_time")
  } else {
    assertive.base::assert_all_are_not_na(property)
    assertive.base::assert_all_are_true(
      timestamp %in% c("created_time", "last_edited_time")
    )
  }

  assertive.base::assert_all_are_true(
    direction %in% c("ascending", "descending")
  )
  #if(prepare_toJSON) return(vec_data(x))
  #else return(x)
  x
}

#' @export
format.notion_sort <- function(x, ...) {
  property <- field(x, "property")
  direction <- field(x, "direction")
  timestamp <- field(x, "timestamp")

  arrow <- ifelse(direction == "ascending", "", emojifont::emoji("arrow_down_small"))
  times <- ifelse(timestamp == "created_time", "*", "")

  out <- paste0(property, arrow, times)
  # out[timestamp == "created_time"] <- crayon::italic(out[timestamp == "created_time"])
  out[is.na(direction) | is.na(timestamp)] <- NA
  out
}

#' @export
vec_ptype_abbr.notion_sort <- function(x, ...) "n.sort"

#' @export
vec_ptype_full.notion_sort <- function(x, ...) "Notion_sort"
