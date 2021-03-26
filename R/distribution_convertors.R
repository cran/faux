#' Convert uniform to normal
#'
#' Convert a uniform distribution to a normal (gaussian) distribution with specified mu and sd
#' 
#' @param x the uniformly distributed vector
#' @param mu the mean of the normal distribution to return
#' @param sd the SD of the normal distribution to return
#' @param min the minimum possible value of x (calculated from x if not given)
#' @param max the maximum possible value of x (calculated from x if not given)
#'
#' @return a vector with a gaussian distribution
#' @export
#'
#' @examples
#' 
#' x <- runif(10000)
#' y <- unif2norm(x)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
unif2norm <- function(x, mu = 0, sd = 1, min = NULL, max = NULL) {
  # tol prevents min and max values returning as -Inf and Inf
  tol <- 1/length(x)
  if (is.null(min)) {
    min <- min(x, na.rm = TRUE) - tol
    message("min was set to ", min)
  }
  if (is.null(max)) {
    max <- max(x, na.rm = TRUE) + tol
    message("max was set to ", max)
  }
  
  p <- stats::punif(x, min, max)
  stats::qnorm(p, mu, sd)
}

#' Convert binomial to normal
#'
#' Convert a binomial distribution to a normal (gaussian) distribution with specified mu and sd
#' 
#' @param x the binomially distributed vector
#' @param mu the mean of the normal distribution to return
#' @param sd the SD of the normal distribution to return
#' @param size number of trials (set to max value of x if not specified)
#' @param prob the probability of success on each trial (set to mean probability if not specified)
#'
#' @return a vector with a gaussian distribution
#' @export
#'
#' @examples
#' 
#' x <- rbinom(10000, 20, 0.75)
#' y <- binom2norm(x, 0, 1, 20, 0.75)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
binom2norm <- function(x, mu = 0, sd = 1, size = NULL, prob = NULL) {
  if (!all(as.integer(x) == x, na.rm = TRUE)) stop("all values in x must be integers or NA")
  
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  if (is.null(size)) {
    size <- maxx
    message("size was set to ", size)
  }
  if (is.null(prob)) {
    prob <- mean(x, na.rm = TRUE)/size
    message("prob was set to ", prob)
  }
  
  if (size < maxx) stop("size cannot be smaller than the largest value")
  if (size < 2) stop("size cannot be smaller than 2")
  if (minx < 0) stop("the smallest possible value in a binomial distribution is 0")
  if (prob <0 || prob>1) stop("prob must be between 0 and 1")
  
  # replace infinite values (where x_i == size)
  a <- stats::pbinom((size-2):(size-1), size, prob) %>%
    stats::qnorm(mu, sd)
  replace_inf <- a[2] + (a[2]-a[1])
  
  p <- stats::pbinom(x, size, prob)
  x2 <- stats::qnorm(p, mu, sd)
  x2[x2==Inf] <- replace_inf
  
  x2
}

#' Convert gamma to normal
#'
#' @param x the gamma distributed vector
#' @param mu the mean of the normal distribution to convert to
#' @param sd the SD of the normal distribution to convert to
#' @param shape gamma distribution parameter (must be positive)
#' @param scale gamma distribution parameter (must be positive)
#' @param rate	an alternative way to specify the scale
#'
#' @return a vector with a normal distribution
#' @export
#'
#' @examples
#' 
#' x <- rgamma(10000, 2)
#' y <- gamma2norm(x)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
gamma2norm <- function(x, mu = 0, sd = 1, shape = NULL, rate = 1, scale = 1/rate) {
  if (scale != 1) rate <- 1/scale
  if (is.null(shape)) {
    shape = mean(x) * rate
    message("shape was set to ", shape)
  }
  p <- stats::pgamma(x, shape, rate = rate)
  stats::qnorm(p, mu, sd)
}

#' Convert beta to normal
#'
#' @param x the gamma distributed vector
#' @param mu the mean of the normal distribution to convert to
#' @param sd the SD of the normal distribution to convert to
#' @param shape1,shape2 non-negative parameters of the beta distribution
#' @param ... further arguments to pass to pbeta (e.g., ncp)
#'
#' @return a vector with a normal distribution
#' @export
#'
#' @examples
#' 
#' x <- rbeta(10000, 2, 3)
#' y <- beta2norm(x)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
beta2norm <- function(x, mu = 0, sd = 1, shape1 = NULL, shape2 = NULL, ...) {
  xmu <- mean(x, na.rm = TRUE)
  xsd <- stats::sd(x, na.rm = TRUE)
  if (is.null(shape1)) {
    shape1 = ((1 - xmu) / xsd^2 - 1 / xmu) * xmu^2
    message("shape1 was set to ", shape1)
  }
  if (is.null(shape2)) {
    shape2 <- shape1 * (1 / xmu - 1)
    message("shape2 was set to ", shape2)
  }
  
  p <- stats::pbeta(x, shape1, shape2, ...)
  stats::qnorm(p, mu, sd)
}

#' Convert normal to poisson
#'
#' @param x the normally distributed vector
#' @param lambda the mean of the distribution to return
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#'
#' @return a vector with a poisson distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2pois(x, 2)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2pois <- function(x, lambda, mu = mean(x), sd = stats::sd(x)) {
  p <- stats::pnorm(x, mu, sd)
  stats::qpois(p, lambda)
}


#' Convert normal to beta
#'
#' @param x the normally distributed vector
#' @param shape1,shape2 non-negative parameters of the distribution to return
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#' @param ... further arguments to pass to qbeta (e.g., ncp)
#'
#' @return a vector with a beta distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2beta(x, 1, 3)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2beta <- function(x, shape1, shape2, mu = mean(x), sd = stats::sd(x), ...) {
  p <- stats::pnorm(x, mu, sd)
  stats::qbeta(p, shape1, shape2, ...)
}


#' Convert normal to gamma
#'
#' @param x the normally distributed vector
#' @param shape gamma distribution parameter (must be positive)
#' @param scale gamma distribution parameter (must be positive)
#' @param rate	an alternative way to specify the scale
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#'
#' @return a vector with a gamma distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2gamma(x, shape = 2)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2gamma <- function(x, shape, rate = 1, scale = 1/rate, 
                       mu = mean(x), sd = stats::sd(x)) {
  p <- stats::pnorm(x, mu, sd)
  if (rate == 1 && scale != 1) rate <- 1/scale
  stats::qgamma(p, shape, rate = rate)
}


#' Convert normal to binomial
#'
#' @param x the normally distributed vector
#' @param size number of trials (0 or more)
#' @param prob the probability of success on each trial (0 to 1)
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#'
#' @return a vector with a binomial distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2binom(x)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2binom <- function(x, size = 1, prob = 0.5, mu = mean(x), sd = stats::sd(x)) {
  p <- stats::pnorm(x, mu, sd)
  stats::qbinom(p, size, prob)
}


#' Convert normal to uniform
#' 
#' Convert a normal (gaussian) distribution to a uniform distribution with specified minimum and maximum
#'
#' @param x the normally distributed vector
#' @param min the minimum of the uniform distribution to return
#' @param max the maximum of the uniform distribution to return
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#'
#' @return a vector with a uniform distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2unif(x)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2unif <- function(x, min = 0, max = 1, mu = mean(x), sd = stats::sd(x)) {
  p <- stats::pnorm(x, mu, sd)
  stats::qunif(p, min, max)
}

#' Convert normal to truncated normal
#' 
#' Convert a normal (gaussian) distribution to a truncated normal distribution with specified minimum and maximum
#'
#' @param x the normally distributed vector
#' @param min the minimum of the truncated distribution to return
#' @param max the maximum of the truncated distribution to return
#' @param mu the mean of the distribution to return (calculated from x if not given)
#' @param sd the SD of the distribution to return (calculated from x if not given)
#'
#' @return a vector with a uniform distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2trunc(x, 1, 7, 3.5, 2)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
norm2trunc <- function(x, min = -Inf, max = Inf, mu = mean(x), sd = stats::sd(x)) {
  p <- stats::pnorm(x, mean(x), stats::sd(x))
  truncnorm::qtruncnorm(p, a = min, b = max, mean = mu, sd = sd)
}

#' Convert truncated normal to normal
#' 
#' Convert a truncated normal distribution to a normal (gaussian) distribution
#'
#' @param x the truncated normally distributed vector
#' @param min the minimum of the truncated distribution (calculated from x if not given)
#' @param max the maximum of the truncated distribution (calculated from x if not given)
#' @param mu the mean of the distribution to return (calculated from x if not given)
#' @param sd the SD of the distribution to return (calculated from x if not given)
#'
#' @return a vector with a uniform distribution
#' @export
#'
#' @examples
#' 
#' x <- truncnorm::rtruncnorm(10000, 1, 7, 3.5, 2)
#' y <- trunc2norm(x, 1, 7)
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
trunc2norm <- function(x, min = NULL, max = NULL, 
                       mu = mean(x), sd = stats::sd(x)) {
  n <- length(x)
  
  # if not specified, set min and max 
  if (is.null(min)) {
    #min <- mu - (1.5*sd + 0.22*sd*log2(n))
    min <- mu - 3*sd
    message("min was set to ", min, 
            " (min(x) = ", min(x), ")")
  }
  if (is.null(max)) {
    #max <- mu + (1.5*sd + 0.22*sd*log2(n))
    max <- mu + 3*sd
    message("max was set to ", max, 
            " (max(x) = ", max(x), ")")
  }
  
  # make sure min and max encompass data
  if (min > min(x)) {
    min <- min(x) - 0.01*sd
    warning("min was > min(x), so min was set to ", min)
  }
  if (max < max(x)) {
    max <- max(x) + 0.01*sd
    warning("max was < max(x), so max was set to ", max)
  }
  
  p <- truncnorm::ptruncnorm(x, a = min, b = max, mean = mu, sd = sd)
  stats::qnorm(p, mean = mu, sd = sd)
}

#' Convert normal to likert
#'
#' @param x the normally distributed vector
#' @param prob a vector of probabilities or counts; if named, the output is a factor
#' @param mu the mean of x (calculated from x if not given)
#' @param sd the SD of x (calculated from x if not given)
#'
#' @return a vector with the specified distribution
#' @export
#'
#' @examples
#' 
#' x <- rnorm(10000)
#' y <- norm2likert(x, c(.1, .2, .35, .2, .1, .05))
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
#' y <- norm2likert(x, c(40, 30, 20, 10))
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
#' 
#' y <- norm2likert(x, c(lower = .5, upper = .5))
#' g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
#' ggExtra::ggMarginal(g, type = "histogram")
norm2likert <- function(x, prob, mu = mean(x), sd = stats::sd(x)) {
  prob <- prob / sum(prob)
  cprob <- cumsum(prob)
  n <- length(cprob)
  
  p <- stats::pnorm(x, mu, sd)
  x2 <- sapply(p, function(a) n + 1 - sum(a < cprob))
  
  if (!is.null(names(prob))) {
    x2 <- factor(x2, levels = 1:n, labels = names(prob))
  }
  
  x2
}


#' Standardized Alpha to Average R
#'
#' @param std_alpha The standarized alpha 
#' @param n The number of items
#'
#' @return The average inter-item correlation
#' @export
#'
#' @examples
#' std_alpha2average_r(.8, 10)
std_alpha2average_r <- function(std_alpha, n) {
  sumR <- -n / ((std_alpha / (n/(n - 1))) - 1)
  (sumR - n)/(n * (n - 1))
}

#' Average r to Random Intercept SD
#'
#' @param average_r The average inter-item correlation
#' @param sigma Total error variance
#'
#' @return The standard deviation of the random intercept
#' @export
average_r2tau_0 <- function(average_r, sigma) {
  sqrt((average_r * sigma^2) / (1 - average_r))
}
