#' Add a dependent variable 
#' 
#' Add a dependent variable to a mixed model simulation using a formula and specification of fixed and random effects parameters.
#' 
#' Fixed effects are specified as a named list for each effect in the equation. For example, for the equation `y ~ a * b + (1 | id)`, the fixed effects might be specified as such: `list(a = 5, b = 10, "a:b" = 0)`.
#' 
#' Random effects are also specified as a named list of standard deviations for the random intercept and slopes, plus optional correlations. For example, for the equation `y ~ a * b + (b | id)`, the random effects might be specified as such: `list(id = list(intercept = 10, b = 5, .cors = 0.4))`. 
#'
#' @param .data the data frame
#' @param formula The formula for your model
#' @param intercept The (grand) intercept value
#' @param error The SD of the error term
#' @param fixef A list of fixed effects (see Details)
#' @param ranef A list of random effects parameters (see Details)
#'
#' @return a data frame with new DV column
#' @keywords internal
#'
#' @examples
#' add_random(id = 1000) |> 
#'   add_between(a = c("A1", "A2")) |>
#'   add_within(b = c("B1", "B2")) |>
#'   add_dv(y ~ a*b + (b | id),
#'          intercept = 100,
#'          error = 10,
#'          fixef = list(a = 5, b = 10, "a:b" = 0),
#'          ranef = list(id = list(intercept = 10, b = 5, .cors = 0.4))
#'   )
# add_dv <- function(.data, formula = y ~ 1, 
#                    intercept = 0, 
#                    error = 1,
#                    fixef = list(), 
#                    ranef = list()) {
  # if (is.character(formula)) formula <- stats::as.formula(formula)
  # dv <- all.vars(formula[[2]])
  # .data[dv] <- 0
  # m <- lm(formula, .data)
  # 
  # conames <- names(m$coefficients)
  # coefs <- c(intercept)
  # m$coefficients <- setNames(coefs, conames)
  # err <- rnorm(nrow(.data), 0, error)
  # .data[dv] <- predict(m) + err
#   
#   return(.data)
# }
