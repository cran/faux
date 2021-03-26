#' Simulate an existing dataframe
#'
#' Produces a data table with the same distributions and correlations 
#' as an existing data table Only returns numeric columns and simulates all numeric variables from a continuous normal distribution (for now).
#' 
#' See \href{../doc/sim_df.html}{\code{vignette("sim_df", package = "faux")}} for details.
#'
#' @param data the existing tbl
#' @param n the number of samples to return per group
#' @param within a list of the within-subject factor columns (if long format)
#' @param between a list of the between-subject factor columns
#' @param dv the name of the DV (value) column
#' @param id the names of the column(s) for grouping observations
#' @param empirical Should the returned data have these exact parameters? (versus be sampled from a population with these parameters)
#' @param long whether to return the data table in long format
#' @param seed DEPRECATED use set.seed() instead before running this function
#' @param missing simulate missing data?
#' 
#' @return a tbl
#' @examples
#' iris100 <- sim_df(iris, 100)
#' iris_species <- sim_df(iris, 100, between = "Species")
#' @export

sim_df <- function (data, n = 100, within = c(), between = c(), 
                    id = "id", dv = "value",
                    empirical = FALSE, long = FALSE, seed = NULL, 
                    missing = FALSE) {
  if (!is.null(seed)) {
    warning("The seed argument is deprecated. Please set seed using set.seed() instead")
  #   # reinstate system seed after simulation
  #   gs <- global_seed(); on.exit(global_seed(gs))
  #   set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  }
  
  # error checking ------
  if ( !is.numeric(n) || n %% 1 > 0 || n < 3 ) {
    stop("n must be an integer > 2")
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame or matrix")
  }
  
  if (length(within) > 0) {
    # convert long to wide
    data <- long2wide(data = data, within = within, between = between, dv = dv, id = id)
  }
  
  if (is.numeric(between)) between <- names(data)[between]
  
  if (length(between) > 0 & !is.character(between)) {
    stop("between must be a numeric or character vector")
  }
  
  numvars <- setdiff(names(data), c(id, dv, between))
  is_num <- sapply(data[numvars], is.numeric)
  numvars <- numvars[is_num]
  
  grps <- data[between]
  if (length(grps) == 0) grps <- rep(1, nrow(data))
  
  simdat <- by(data, grps, function(x) {
    y <- x[numvars]
    z <- rnorm_multi(
      n = n,
      vars = ncol(y),
      mu = sapply(y, mean, na.rm = TRUE),
      sd = sapply(y, sd, na.rm = TRUE),
      r = cor(y, use = "complete.obs"),
      varnames = names(y),
      empirical = empirical
    )
    
    # simulate missing data pattern
    if (missing) {
      na_cells <- dplyr::mutate_all(y, is.na) %>% 
        sim_joint_dist(n = n)
      
      z <- mapply(function(sim_col, nc_col) {
        sim_col[nc_col] <- NA
        sim_col
      }, z, na_cells, SIMPLIFY = FALSE) %>%
        as.data.frame()
    }
    
    ## add between vars
    for (b in between) z[b] <- unique(x[[b]])
    z[ , c(between, numvars), drop = FALSE]
  }) %>% do.call(rbind, .)
  
  # fix names
  nm <- names(simdat)
  simdat[id] <- make_id(nrow(simdat))
  simdat <- simdat[c(id, nm)]
  rownames(simdat) <- c()
  
  return(simdat)
}
