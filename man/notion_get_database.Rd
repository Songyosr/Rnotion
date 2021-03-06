% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notion_get_database.R
\name{notion_get_database}
\alias{notion_get_database}
\title{Notion GET Database API}
\usage{
notion_get_database(
  database_id = NULL,
  start_cursor = NULL,
  page_size = NULL,
  api_rate_limited = 1/3
)
}
\arguments{
\item{database_id}{a length 1 character vector represented an identifier for a database.
When not supplied, list all databases}

\item{start_cursor}{a length 1 character vector. If supplied, this endpoint
will return a page of results starting after the cursor provided.
If not supplied, this endpoint will return the first page of results.}

\item{page_size}{a length 1 integer vector. The number of items from the full
list desired in the response. Maximum: 100}

\item{api_rate_limited}{The rate limit for incoming requests is an average of
3 requests per second. Some bursts beyond the average rate are allowed.}
}
\description{
Wrapper function for 'GET Database' API of Notion.so
}
\details{
please check: \url{https://developers.notion.com/reference/get-database} and
\url{https://developers.notion.com/reference/get-databases}
}
\examples{
notion_get_database()

}
