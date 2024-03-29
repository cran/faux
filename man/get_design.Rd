% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_design.R
\name{get_design}
\alias{get_design}
\title{Get design}
\usage{
get_design(data)
}
\arguments{
\item{data}{The data table to check}
}
\value{
list with class design
}
\description{
Get the design specification from a data table created in faux. This can be used to create more simulated data with the same design.
}
\examples{
data <- sim_design(2, 2, plot = FALSE)
design <- get_design(data)
data2 <- sim_design(design, plot = FALSE)
}
