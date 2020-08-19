context("test-distributions")

# error ----
test_that("error", {
  missing_x <- "argument \"x\" is missing, with no default"
  expect_error(norm2unif(), missing_x)
  expect_error(norm2pois(), missing_x)
  expect_error(norm2beta(), missing_x)
  expect_error(norm2binom(), missing_x)
  expect_error(unif2norm(), missing_x)
  
  expect_error(norm2pois(1), "argument \"lambda\" is missing, with no default")
  expect_error(norm2beta(1), "argument \"shape1\" is missing, with no default")
  expect_error(norm2beta(1, 1), "argument \"shape2\" is missing, with no default")
})

set.seed(1)
reps <- 1
mapreps <- 1:100
tol <- 0.1

# unif2norm ----
test_that("unif2norm", {
  for (i in 1:reps) {
    mu <- runif(1, -100, 100)
    sd <- runif(1, 1, 5)
    
    s <- purrr::map_df(mapreps, ~{
      x <- runif(100)
      y <- unif2norm(x, mu, sd)
      y2 <- rnorm(100, mu, sd)
      
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2))
    }) 
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(mu, sd, mu, sd), tolerance = tol)
  }
})

# norm2unif ----
test_that("norm2unif", {
  for (i in 1:reps) {
    min <- runif(1, -100, 100)
    max <- min + runif(1, 1, 100)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(100)
      y <- norm2unif(x, min, max)
      
      y2 <- runif(100, min, max)
      list(min_1 = min(y), max_1 = max(y),
           min_2 = min(y2), max_2 = max(y2)
      )
    })
    
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(min, max, min, max), tolerance = tol)
  }
})

# norm2pois ----
test_that("norm2pois", {
  for (i in 1:reps) {
    lambda <- sample(1:10, 1)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(100)
      y <- norm2pois(x, lambda)
      
      y2 <- rpois(100, lambda)
      list(m_1 = mean(y), m_2 = mean(y2)
      )
    })
    
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(lambda, lambda), tolerance = tol)
  }
})

# norm2beta ----
test_that("norm2beta", {
  find_shape_params <- function(x) {
    mu <- mean(x)
    variance <- var(x)
    shape1 <- ((1 - mu) / variance - 1 / mu) * mu^2
    shape2 <- shape1 * (1 / mu - 1)
    return(list(shape1 = shape1, shape2 = shape2))
  }
  
  for (i in 1:reps) {
    shape1 <- sample(1:10, 1)
    shape2 <- sample(1:10, 1)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(100)
      y <- norm2beta(x, shape1, shape2)
      
      y2 <- rbeta(100, shape1, shape2)
      list(dist1_shape1 = find_shape_params(y)$shape1, 
           dist1_shape2 = find_shape_params(y)$shape2,
           dist2_shape1 = find_shape_params(y2)$shape1, 
           dist2_shape2 = find_shape_params(y2)$shape2)
    })
    
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(shape1, shape2, shape1, shape2), tolerance = 0.05)
  }
})

# norm2binom ----
test_that("norm2binom", {
  for (i in 1:reps) {
    prob <- runif(1)
    size <- sample(1:10, 1)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(100)
      y <- norm2binom(x, size, prob)
      
      y2 <- rbinom(100, size, prob)
      list(m_1 = mean(y), m_2 = mean(y2)
      )
    })
    
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(prob*size, prob*size), tolerance = tol)
  }
})

# norm2trunc ----
test_that("norm2trunc", {
  for (i in 1:reps) {
    min <- runif(1, -100, 100)
    max <- min + runif(1, 1, 100)
    mu <- (max + min)/2
    sd <- runif(1, 1, 5)
    
    s <- purrr::map_df(mapreps, ~{
      x <- rnorm(100, mu, sd)
      y <- norm2trunc(x, min, max, mu, sd)
      
      y2 <- truncnorm::rtruncnorm(100, min, max, mu, sd)
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2))
    }) 
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(mu, sd, mu, sd), tolerance = tol)
  }
})


# trunc2norm ----
test_that("trunc2norm", {
  # messages
  set.seed(3)
  x <- truncnorm::rtruncnorm(1000)
  
  expect_message(suppressWarnings(trunc2norm(x)), 
                 "mu was set to 0.00639653548187051", fixed = TRUE)
  expect_message(suppressWarnings(trunc2norm(x)), 
                 "sd was set to 0.9980754175952", fixed = TRUE)
  expect_message(suppressWarnings(trunc2norm(x)), 
                 "-2.98782971730373 (min(x) = -3.05632823356306)", fixed = TRUE)
  expect_message(suppressWarnings(trunc2norm(x)), 
                 "max was set to 3.00062278826747 (max(x) = 3.51929906496364)", fixed = TRUE)
  expect_warning(suppressMessages(trunc2norm(x)), 
                 "min was > min(x), so min was set to -3.06630898773901", fixed = TRUE)
  expect_warning(suppressMessages(trunc2norm(x)), 
                 "max was < max(x), so max was set to 3.52927981913959", fixed = TRUE)
  
  set.seed(8675309)
  x <- truncnorm::rtruncnorm(100, mean = 10, sd = 5)
  
  expect_message(trunc2norm(x), "mu was set to 10.2615138815768", fixed = TRUE)
  expect_message(trunc2norm(x), "sd was set to 4.64571854015768", fixed = TRUE)
  expect_message(trunc2norm(x), "min was set to -3.67564173889628 (min(x) = -2.98432899791241)", fixed = TRUE)
  expect_message(trunc2norm(x), "max was set to 24.1986695020498 (max(x) = 20.1469578725237)", fixed = TRUE)
  
  # defaults
  for (i in 1:reps) {
    s <- purrr::map_df(mapreps, ~{
      x <- truncnorm::rtruncnorm(100)
      suppressMessages(suppressWarnings(y <- trunc2norm(x)))
      y2 <- rnorm(100)
      
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2))
    }) 
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(0, 1, 0, 1), tolerance = tol)
  }
  
  # set min, max, mean and sd
  for (i in 1:reps) {
    min <- runif(1, -100, 100)
    max <- min + runif(1, 1, 100)
    mu <- (max + min)/2
    sd <- runif(1, 1, 5)
    
    s <- purrr::map_df(mapreps, ~{
      x <- truncnorm::rtruncnorm(100, min, max, mu, sd)
      suppressWarnings(
        y <- trunc2norm(x, min, max, mu, sd)
      )
      
      y2 <- rnorm(100, mu, sd)
      list(m_1 = mean(y), sd_1 = sd(y),
           m_2 = mean(y2), sd_2 = sd(y2))
    }) 
    summ <- dplyr::summarise_all(s, mean) %>% unlist() %>% unname()
    expect_equal(summ, c(mu, sd, mu, sd), tolerance = tol)
  }
})

# norm2likert ----
test_that("norm2likert", {
  x <- rnorm(1e4)
  
  expect_error(norm2likert(), "argument \"prob\" is missing, with no default")
  expect_error(norm2likert(x), "argument \"prob\" is missing, with no default")
  expect_error(norm2likert(x, 2), "argument \"prob\" must add up to 1")
  
  y <- norm2likert(x, c(.25, .5, .25))
  expect_equal(mean(y == 1), .25, tolerance = tol)
  expect_equal(mean(y == 2), .50, tolerance = tol)
  expect_equal(mean(y == 3), .25, tolerance = tol)
  
  y <- norm2likert(x, c(.1, .2, .3, .4))
  expect_equal(mean(y == 1), .1, tolerance = tol)
  expect_equal(mean(y == 2), .2, tolerance = tol)
  expect_equal(mean(y == 3), .3, tolerance = tol)
  expect_equal(mean(y == 4), .4, tolerance = tol)
  
  y <- norm2likert(x, c(.4, .3, .2, .1))
  expect_equal(mean(y == 1), .4, tolerance = tol)
  expect_equal(mean(y == 2), .3, tolerance = tol)
  expect_equal(mean(y == 3), .2, tolerance = tol)
  expect_equal(mean(y == 4), .1, tolerance = tol)
})
