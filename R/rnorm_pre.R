#' Make a normal vector correlated to an existing vector
#'
#' \code{rnorm_pre} Produces a random normally distributed vector with the specified correlation to an existing vector
#'
#' @param x the existing vector
#' @param mu desired mean of returned vector
#' @param sd desired SD of returned vector
#' @param r desired correlation between existing and returned vectors
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance 
#' 
#' @return vector
#' @examples
#' v1 <- rnorm(10)
#' v2 <- rnorm_pre(v1, 0, 1, 0.5)
#' cor(v1, v2)
#' @export

rnorm_pre <- function (x, mu=0, sd=1, r=0, empirical = FALSE) {
  # error checking
  if (!is.vector(x)) stop("x must be a vector") 
  if (!is.numeric(x)) stop("x must be numeric")
  if (length(x) < 3) stop("x must have length > 2")
  
  n <- length(x)
  
  if (!empirical) {
    sample_params <- sample_from_pop(n, mu, sd, r)
    mu <- sample_params$mu
    sd <- sample_params$sd
    r <- sample_params$r
  }
  
  # generate correlated vector
  y <- stats::rnorm(n)
  z <- r * scale(x)[,1] + sqrt(1 - r^2) * 
    scale(stats::resid(stats::lm(y ~ x)))[,1]
  yresult <- mu + sd * z
  
  return(yresult)
}

#' Sample Parameters from Population Parameters
#'
#' @param n sample size
#' @param mu population mean
#' @param sd population SD
#' @param r population r
#'
#' @return list of sample parameters (mu, sd, r)
#' @export
#'
#' @examples
#' sample_from_pop(10)
sample_from_pop <- function(n = 100, mu = 0, sd = 1, r = 0) {
  # sample r from distribution depending on n and r
  r_sd <- sqrt(1/n) * (1-r^2)
  sample_r <- stats::rnorm(1, r, r_sd) %>% pmax(-1) %>% pmin(1)
  
  # sample mu from distribution depending on n and sd
  mu_sd <- sd / sqrt(n)
  sample_mu <- stats::rnorm(1, mu, mu_sd)
  
  # sample sd from distribution depending on n and sd
  sd_sd <- sd / sqrt(2*n)
  sample_sd <- stats::rnorm(1, sd, sd_sd)
  
  list(
    mu = sample_mu,
    sd = sample_sd,
    r = sample_r
  )
}
