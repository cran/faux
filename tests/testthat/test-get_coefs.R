test_that("errors", {
  data <- sim_design(within = 2, between = 2, 
                     mu = c(1, 0, 1, 1), 
                     r = 0.5,
                     long = TRUE, empirical = TRUE, plot = FALSE) %>%
    add_contrast("W1", "anova", colnames = "W1") %>%
    add_contrast("B1", "anova", colnames = "B1")
  
  coef <- get_coefs(data)
  expect_equal(coef, c("(Intercept)" = 0.75, W1 = -0.5, B1 = 0.5,  "W1:B1" = 1.0))
  
  coef <- get_coefs(data, y ~ W1 * B1)
  expect_equal(coef, c("(Intercept)" = 0.75, W1 = -0.5, B1 = 0.5, "W1:B1" = 1.0))
  
  coef <- get_coefs(data, y ~ B1 * W1)
  expect_equal(coef, c("(Intercept)" = 0.75, B1 = 0.5, W1 = -0.5, "B1:W1" = 1.0))
  
  coef <- get_coefs(data, y ~ W1 + B1)
  expect_equal(coef, c("(Intercept)" = 0.75, W1 = -0.5, B1 = 0.5))
  
  coef <- get_coefs(data, y ~ B1)
  expect_equal(coef, c("(Intercept)" = 0.75, B1 = 0.5))
  
  data$y <- norm2binom(data$y)
  mod <- lme4::glmer(y ~ W1*B1 + (1 | id), data, family = binomial)
  coef <- get_coefs(data, fun = "glm", family = binomial)
  expect_equivalent(coef, lme4::fixef(mod))
})
