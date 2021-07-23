#' #' Internal vctrs methods
#' #'
#' #' @import vctrs
#' #' @keywords internal
#' #' @name notion-vctrs
#' NULL

methods::setOldClass(c("notion_filter", "vctrs_vctr"))

# Import References
filter_ref <- read.csv("data/property_filter_reference.csv")

# Create basic property ---------------------------------------------------
# text_filter <- function(equals = NULL,  does_not_equal = NULL,
#                         contains  = NULL,
#                         does_not_contain = NULL,
#                         starts_with = NULL,
#                         ends_with = NULL,
#                         is_empty = NULL,
#                         is_not_empty = NULL,
# )

validate_a_filter_condition <- \(x, un_RNO) {#..., type){

  property <- field(x, "property")
  type <- field(x, "type")
  condition <- field(x, "condition")

  assertive.properties::assert_is_scalar(condition)
  filter_ref <- filter_ref[filter_ref$group == type, "Property"]
  inside_list <- names(condition)
  check <-  names(condition) %in% filter_ref
  if(!check) {
    stop("Property '",  names(condition[!check]),
         "' does not exists filter type['",type,"']",
         "\n [i] Please review: \nhttps://developers.notion.com/reference/post-database-query#post-database-query-filter")
  }
  #print(str(condition))

  if(un_RNO) {
    result <- list(property = property, type = type, condition[[1]])
    names(result)[3] <- inside_list
    return(result)
  }
  x
}

validate_filter_conditions <- \(x, un_RNO = FALSE) {

  if (tmp <- !assertive.types::is_inherited_from(x, "notion_filter")) {
    warning(
      attr(tmp, "cause"),
      "\n  [i] Please use 'notion_filter()' function to create sort regimes to ensure the correctness"
    )
  }

  #type <- field(x, "type")
  #condition <- field(x, "condition")

#  str(type)
#  str(condition)

  # lapply(seq_along(x), \(i){
  #   validate_a_filter_condition(condition[i], type = type[i])
  # })
  y <- lapply(x, validate_a_filter_condition, un_RNO = un_RNO)
  str(y)

  #return(x)
}
# Core function -----------------------------------------------------------
new_filter_notion <- function(property = character(),
                              type = character(),
                              condition = list()) {
  vec_assert(property, ptype = character())
  vec_assert(type, ptype = character())
  #assertive.properties::assert_all_are_same_length(property, type, list(...))
  #cat(length(list(...)), "Wow")
  # condition = list(...)
  vec_assert(condition, ptype = list())
  # print(property)

  new_rcrd(list(
    property = property,
    type = type,
    condition = condition
  ), class = "notion_filter")
}

#' #' Notion Sort object generator
#' #'
#' #' @description A helper function for creating 'Sort objects' used
#' #' by Notion API in Search and Query.
#' #'
#' #' @param property A character vector represented the name of the
#' #' property to sort against. Should be omitted, when used inside
#' #' \code{\link{notion_search}} function.
#' #'
#' #' @param direction A character vector represented the direction to sort.
#' #' Either \code{"ascending"} and \code{"descending"}.
#' #'
#' #' @param timestamp A character vector represented The name of the timestamp
#' #' to sort against. Either \code{"created_time"} and \code{"last_edited_time"}.
#' #'
#' #' @details please check:
#' #' \url{https://developers.notion.com/reference/post-search} and
#' #' \url{https://developers.notion.com/reference/post-database-query}
#' #'
#' #' @examples
#' #'
#' #' # Size of one
#' #' sort_1 <- sort_notion(property = "Ingredients", direction = "descending", timestamp = "last_edited_time")
#' #' sort_1
#' #'
#' #' # Size of N (all element should have an equal length or 1)
#' #' sort_n <- sort_notion(
#' #'   property = LETTERS[1:4],
#' #'   direction = "ascending",
#' #'   timestamp = rep(c("last_edited_time", "created_time"), 2)
#' #' )
#' #'
#' #' # Coercing sort criteria with 'c()'
#' #' c(sort_1, sort_n)
#' #'
#' #' # Omit properties when use inside notion search
#' #' notion_search(
#' #'   sort = sort_notion(direction = "ascending", timestamp = "last_edited_time"),
#' #'   page_size = 4
#' #' )
#'
#' @export
filter_notion <- function(property = character(),
                          type = character(),
                          ...,
                          condition = list()) {

  tmp_condition <- list(...)
  # Casting Character
  c(property, type) %<-%
    vec_cast_common(property, type, .to = character())

  if(length(condition) == 0 &
     length(property) != 0 &
     length(type) != 0) {
    condition <- tmp_condition
  }
  else if(!is.list(condition) & purrr::vec_depth(condition) == 1) {
    condition <- list(condition)
  } else if(!is.list(condition)) condition <- as.list(condition)

  #print(list(A = property, B = type, C = tmp_condition, D = condition))

  # Check for r
  c(property, type, condition) %<-%
    vec_recycle_common(property, type, condition)

  # Check
  #lapply(seq_along)

  # cat("3. " ,property, "|" , direction, "|" , timestamp, "\n")
  new_filter_notion(property, type, condition)
}

#' @export
format.notion_filter <- function(x, ...) {
  property <- field(x, "property")
  condition <- field(x, "condition")
  #
  out <- paste0("[",property,"] ",
                 names(condition),": ",
                 unlist(condition, use.names =  FALSE))
  out[is.na(property)] <- NA
  out
}
#'
#' @export
vec_ptype_abbr.notion_filter <- function(x, ...) "n.filter"
#'
#' @export
vec_ptype_full.notion_filter <- function(x, ...) "Notion_filter"



#' OR and And
OR <- function(...){
  or <- list(...)
  tmp <- tibble::lst(or)
  return(tmp)
}

AND <- function(...){
  and <- list(...)
  tmp <- tibble::lst(and)
  return(tmp)
}
