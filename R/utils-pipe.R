#' Pipe operator
#'
#' See \code{dplyr::\link[dplyr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL

#' Piped OR
#'
#' LHS if not \code{NULL}, otherwise RHS
#'
#' @param l LHS.
#' @param r RHS.
#' @return LHS if not \code{NULL}, otherwise RHS.
#' @name OR
#'
#' @keywords internal
#'
`%||%` <- function(l, r) {
  if (is.null(l)) r else l
}


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


