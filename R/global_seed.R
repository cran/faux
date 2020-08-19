#' #' Get or set seed
#' #'
#' #' @param seed The seed to set; gets global seed if NULL
#' #'
#' #' @return The global seed (if seed argument is null)
#' #'
#' global_seed <- function(seed = NULL) {
#'   if (!is.null(seed)) {
#'     assign(".Random.seed", seed, envir = .GlobalEnv, inherits = FALSE)
#'   } else {
#'     get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
#'   }
#' }
