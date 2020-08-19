#' Get parameters from a data table
#'
#' \code{get_params} Generates a table of the correlations and means of numeric columns in a data frame
#'
#' @param data the existing tbl
#' @param between a vector of column names for between-subject factors
#' @param within a vector of column names for within-subject factors (if data is long)
#' @param dv the column name of the dv (if data is long)
#' @param id the column name(s) of the subject ID (if data is long)
#' @param digits how many digits to round to (default = 2)
#' 
#' @return a tbl of correlations, means and sds
#' @examples
#' get_params(iris, "Species")
#' @export
#' 

get_params <- function(data, between = c(), within = c(), 
                       dv = "y", id = "id", digits = 2) {
  # error checking -----------------
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  } else if (!is.data.frame(data)) {
    stop("data must be a data frame or matrix")
  }
  
  if (is.numeric(between)) between <- names(data)[between]
  
  if (length(between) > 0 & !is.character(between)) {
    stop("between must be a numeric or character vector")
  }
  
  if (length(within) && length(dv) && length(id)) {
    # convert long to wide
    data <- long2wide(data, within, between, dv, id)
    data$id <- NULL
  }
  
  numvars <- setdiff(names(data), c(names(id), names(dv), between))
  is_num <- sapply(data[numvars], is.numeric)
  numvars <- numvars[is_num]
  
  grps <- data[between]
  if (length(grps) == 0) grps <- rep(1, nrow(data))
  
  desc <- by(data, grps, function(x) {
    m <- apply(x[numvars], 2, mean)
    sd <- apply(x[numvars], 2, sd)
    desc <- data.frame(var = names(m),
                       mean = round(m, digits), 
                       n = nrow(x), 
                       sd = round(sd, digits))
    for (b in between) desc[b] <- unique(x[[b]])
    desc[, c(between, "var", "mean", "n", "sd")]
  })
  
  desc <- do.call(rbind, desc)
  ord <- do.call(order, desc[c(between, "var")])
  descriptives <- desc[ord, ]
  
  if (length(numvars) > 1) {
    r <- by(data, grps, function(x) {
      r <- cor(x[numvars]) %>% round(digits) %>% as.data.frame()
      r$var <- row.names(r)
      for (b in between) r[b] <- unique(x[b])
      r
    })
    
    r <- do.call(rbind, r)
    ord <- do.call(order, r[c(between, "var")])
    r <- r[ord, ]
    
    descriptives[c(between, "var")] <- NULL
    stats <- cbind(r, descriptives)
    var_order <- c(between, "n", "var", numvars, "mean", "sd")
  } else {
    stats <- descriptives
    var_order <- c(between, "n", "mean", "sd")
  }
  stats <- stats[, var_order] %>%
    as.data.frame()
  row.names(stats) <- NULL
  
  stats
}

#' @rdname get_params
#' @export
check_sim_stats <- get_params
