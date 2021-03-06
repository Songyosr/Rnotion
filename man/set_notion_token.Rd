% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_notion_token.R
\name{set_notion_token}
\alias{set_notion_token}
\title{Set the Notion token key}
\usage{
set_notion_token(token = NULL)
}
\arguments{
\item{token}{a notion}
}
\description{
Function to generate a request header for notion.so
}
\details{
Review the official instruction in order to get your token
\url{https://developers.notion.com/docs/getting-started}
}
\examples{
set_notion_token()
set_notion_token(token = "AVBBPDSAFASKDDASD") # False token
}
