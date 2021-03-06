% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/json_design.R
\name{json_design}
\alias{json_design}
\title{Convert design to JSON}
\usage{
json_design(design, filename = NULL, digits = 8, pretty = FALSE, ...)
}
\arguments{
\item{design}{a design list including within, between, n, mu, sd, r, dv, id}

\item{filename}{option name of file to save the json to}

\item{digits}{number of digits to save}

\item{pretty}{whether to print condensed or readable}

\item{...}{other options to send to jsonlite::toJSON}
}
\value{
a JSON string
}
\description{
Convert a design list to JSON notation for archiving (e.g. in scienceverse)
}
\examples{
des <- check_design(2,2)
json_design(des)
json_design(des, pretty = TRUE)
}
