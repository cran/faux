set.seed(90210)
tol <- 0.1

test_that("error messages", {
  expect_error(rnorm_pre(matrix(1:9, 3, 3)), "x must be a vector")
  expect_error(rnorm_pre(letters), "x must be numeric")
  expect_error(rnorm_pre(rnorm(2)), "x must have length > 2")
})

test_that("correct default parameters", {
  n <- sample(10:100, 1)
  v1 <- rnorm(n)
  v2 <- rnorm_pre(v1, empirical = TRUE)
  
  rho <- cor(v1, v2)
  ysd <- sd(v2)
  ymean <- mean(v2)
  
  expect_equal(rho, 0)
  expect_equal(ymean, 0)
  expect_equal(ysd, 1)
})

test_that("correct specified parameters", {
  n <- sample(10:100, 1)
  v1 <- rnorm(n)
  rho <- runif(1) * sample(c(-1, 1), 1)
  ymean <- rnorm(1, 0, 100)
  ysd <- runif(1, 0.001, 100)
  v2 <- rnorm_pre(v1, ymean, ysd, rho, empirical = TRUE)
  
  testrho <- cor(v1, v2)
  testymean <- mean(v2)
  testysd <- sd(v2)
  
  expect_equal(rho, testrho)
  expect_equal(ymean, testymean)
  expect_equal(ysd, testysd)
})

test_that("empirical = FALSE", {
  n <- 100
  r <- 0
  mu <- 0
  sd <- 1
  simdat <- purrr::map_df(1:100, ~{
    v1 <- rnorm(n)
    v2 <- rnorm_pre(v1, mu, sd, r)
    
    data.frame(
      sd = sd(v2),
      mu = mean(v2),
      r = cor(v1, v2)
    )
  })
  
  expect_equal(mean(simdat$mu), mu, tolerance = tol)
  expect_equal(mean(simdat$sd), sd, tolerance = tol)
  expect_equal(mean(simdat$r), r, tolerance = tol)
  
  expect_equal(sd(simdat$mu), sd / sqrt(n), tolerance = tol)
  expect_equal(sd(simdat$sd), sd / sqrt(2*n), tolerance = tol)
  expect_equal(sd(simdat$r), sqrt(1/n) * (1-r^2), tolerance = tol)
})
