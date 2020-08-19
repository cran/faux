context("test-messy")

test_that("messy", {
  iris2 <- messy(iris, 1, "Species")
  
  expect_equal(iris2[,1:4], iris[,1:4])
  expect_equal(iris2$Species, rep(NA, 150) %>% factor(levels = levels(iris$Species)))
  
  iris3 <- messy(iris, 0.5, 1:2)
  
  expect_equal(is.na(iris3[[1]]) %>% sum(), 75)
  expect_equal(is.na(iris3[[2]]) %>% sum(), 75)
  expect_equal(is.na(iris3[[3]]) %>% sum(), 0)
  expect_equal(is.na(iris3[[4]]) %>% sum(), 0)
  
  iris4 <- messy(iris, 0.5, "Species", replace = "NOPE")
  
  expect_equal(sum(iris4$Species == "NOPE"), 75)
})
