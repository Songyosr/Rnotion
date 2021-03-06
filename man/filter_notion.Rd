% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_notion.R
\name{filter_notion}
\alias{filter_notion}
\title{Notion Filter object generator}
\usage{
filter_notion(
  property = character(),
  type = character(),
  ...,
  condition = list(),
  .CallfromSearch = false
)
}
\arguments{
\item{property}{A character vector represented the name of the
property to filter on. Should only be 'object', when used inside
\code{\link{notion_search}} function.}

\item{type}{A character vector represented the direction to sort.
Either \code{"ascending"} and \code{"descending"}.}

\item{timestamp}{A character vector represented The name of the timestamp
to sort against. Either \code{"created_time"} and \code{"last_edited_time"}.}
}
\description{
A helper function for creating 'Filter objects' used
by Notion API in Search and Query.
}
\details{
please check:
\url{https://developers.notion.com/reference/post-search} and
\url{https://developers.notion.com/reference/post-database-query}
}
\examples{

# Size of one
sort_1 <- sort_notion(property = "Ingredients", direction = "descending", timestamp = "last_edited_time")
sort_1

# Size of N (all element should have an equal length or 1)
sort_n <- sort_notion(
  property = LETTERS[1:4],
  direction = "ascending",
  timestamp = rep(c("last_edited_time", "created_time"), 2)
)

# Coercing sort criteria with 'c()'
c(sort_1, sort_n)

# Omit properties when use inside notion search
notion_search(
  sort = sort_notion(direction = "ascending", timestamp = "last_edited_time"),
  page_size = 4
)

}
