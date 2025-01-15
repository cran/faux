# test_that("errors", {
#   dat <- sim_design(2, 2, long = TRUE)
#   expect_error(add_dv(dat, y ~ x), regexp = ": x$")
#   expect_error(add_dv(dat, y ~ age), regexp = ": age$")
#   expect_error(add_dv(dat, y ~ x*age), regexp = ": x, age$")
#   
#   expect_warning(add_dv(dat, W1 ~ B1), "The column W1 will be overwritten")
# })
# 
# test_that("basic", {
#   dat <- add_random(rater = 5)
#   
#   # default
#   dat1 <- add_dv(dat)
#   expect_equal(c("rater", "y"), colnames(dat1))
#   expect_equal(rep(0, 5), dat1$y)
#   
#   # change intercept
#   dat1 <- add_dv(dat, intercept = 10)
#   expect_equal(c("rater", "y"), colnames(dat1))
#   expect_equal(rep(10, 5), dat1$y)
#   
#   # change dv name using formula
#   dat2 <- add_dv(dat, dv ~ 1)
#   expect_equal(c("rater", "dv"), colnames(dat2))
#   expect_equal(rep(0, 5), dat2$dv)
#   
#   # change dv name using text
#   dat3 <- add_dv(dat, "dv ~ 1")
#   expect_equal(c("rater", "dv"), colnames(dat3))
#   expect_equal(rep(0, 5), dat3$dv)
# })
# 
# test_that("categorical IVs", {
#   dat <- add_random(rater = 6) |>
#     add_between("rater", x = c("A", "B"))
#   
#   dat1 <- add_dv(dat, y ~ x, list(x = 1))
#   expect_equal(dat1$y, rep(0:1, 3))
# })
