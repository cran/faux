#' Get Coefficients from Data
#' 
#' You need model coefficients to simulate multilevel data, and can get them from data simulated from parameters using sim_design() or rmulti().
#' 
#' @param data A dataset in long format
#' @param formula A formula (can be extracted from datasets created by sim_design)
#' @param fun the model function (one of "lm", "glm", "lmer",  or "glmer")
#' @param ... Further arguments to the model function
#'
#' @return a list of the model coefficients
#' @export
#'
#' @examples
#' # simulate some data
#' data <- sim_design(within = 2, between = 2, 
#'                    mu = c(1, 0, 1, 1), 
#'                    long = TRUE, empirical = TRUE)
#' 
#' # get coefs for the full factorial model
#' get_coefs(data)
#' 
#' # a reduced model
#' get_coefs(data, y ~ B1 + W1)
#' 
#' # specify a different model function
#' data$y <- norm2binom(data$y)
#' get_coefs(data, fun = "glm", family = binomial)
get_coefs <- function(data, formula = NULL, 
                      fun = c("lm", "glm", "lmer", "glmer"), ...) {
  fun <- match.arg(fun)
  
  if (is.null(formula)) {
    design <- get_design(data)
    factors <- c(names(design$within), names(design$between))
    ranef <- ""
    if (length(design$within) > 0) {
      ranef <- sprintf(" + (1 | %s)",
                       #names(design$within) %>% paste(collapse = " * "),
                       names(design$id))
      
      if (fun == "lm") {
        fun <- "lmer"
      } else if (fun == "glm") {
        fun <- "glmer"
      }
    }
    txt_formula <- sprintf("%s ~ %s%s", 
                           names(design$dv),
                           paste(factors, collapse = " * "),
                           ranef)
    formula <- stats::as.formula(txt_formula)
  }
  
  func <- switch(fun, 
                 lm = stats::lm, 
                 glm = stats::glm, 
                 lmer = lme4::lmer, 
                 glmer = lme4::glmer)
  
  # switch data wide to long
  dv <- as.character(formula[[2]])
  
  if (!dv %in% names(data)) {
    tryCatch(data <- wide2long(data),
             error = function(e) {
               stop("The dv (", dv, ") is not in the dataset")
             }
    )
  }
  
  args <- list(formula = formula, data = data, ...)
  model <- do.call(func, args)
  
  if (fun %in% c("lm", "glm")) {
    estimates <- model$coefficients
  } else {
    estimates <- lme4::fixef(model)
  }
  
  estimates
}


