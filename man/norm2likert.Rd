% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distribution_convertors.R
\name{norm2likert}
\alias{norm2likert}
\title{Convert normal to likert}
\usage{
norm2likert(x, prob, labels = names(prob), mu = mean(x), sd = stats::sd(x))
}
\arguments{
\item{x}{the normally distributed vector}

\item{prob}{a vector of probabilities or counts; if named, the output is a factor}

\item{labels}{a vector of values, defaults to names(prob) or 1:length(prob), if numeric, the output is numeric}

\item{mu}{the mean of x (calculated from x if not given)}

\item{sd}{the SD of x (calculated from x if not given)}
}
\value{
a vector with the specified distribution
}
\description{
Convert normal to likert
}
\examples{

x <- rnorm(10000)
y <- norm2likert(x, c(.1, .2, .35, .2, .1, .05))
g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
ggExtra::ggMarginal(g, type = "histogram")

y <- norm2likert(x, c(40, 30, 20, 10))
g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
ggExtra::ggMarginal(g, type = "histogram")

y <- norm2likert(x, c(lower = .5, upper = .5))
g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
ggExtra::ggMarginal(g, type = "histogram")
}
