test_that("contr_code_anova", {
  x <- factor(1:2, labels = LETTERS[1:2])
  x1 <- contr_code_anova(x, base = 1)
  x2 <- contr_code_anova(x, base = 2)
  mat1 <- matrix(c(-0.5, 0.5), dimnames = list(LETTERS[1:2], ".B-A"))
  mat2 <- matrix(c(0.5, -0.5), dimnames = list(LETTERS[1:2], ".A-B"))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- contr_code_anova(x, base = 1)
  x2 <- contr_code_anova(x, base = 2)
  x3 <- contr_code_anova(x, base = 3)
  mat1 <- matrix(c(-1/3,  2/3, -1/3, -1/3, -1/3, 2/3), nrow = 3,
                 dimnames = list(LETTERS[1:3], c(".B-A", ".C-A")))
  mat2 <- matrix(c(2/3, -1/3, -1/3, -1/3, -1/3, 2/3), nrow = 3,
                 dimnames = list(LETTERS[1:3], c(".A-B", ".C-B")))
  mat3 <- matrix(c(2/3, -1/3, -1/3, -1/3, 2/3, -1/3), nrow = 3,
                 dimnames = list(LETTERS[1:3], c(".A-C", ".B-C")))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  expect_equal(contrasts(x3), mat3)
  
  # non-factor vector
  x <- sample(LETTERS[1:3], 100, T)
  x1 <- contr_code_anova(x, base = 1)
  x2 <- contr_code_anova(x, base = 2)
  x3 <- contr_code_anova(x, base = 3)
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  expect_equal(contrasts(x3), mat3)
})

# contr_code_anova, base by name ----
test_that("contr_code_anova, base by name", {
  x <- factor(1:2, labels = LETTERS[1:2])
  xA <- contr_code_anova(x, base = "A")
  xB <- contr_code_anova(x, base = "B")
  mat1 <- matrix(c(-0.5, 0.5), dimnames = list(LETTERS[1:2], ".B-A"))
  mat2 <- matrix(c(0.5, -0.5), dimnames = list(LETTERS[1:2], ".A-B"))
  expect_equal(contrasts(xA), mat1)
  expect_equal(contrasts(xB), mat2)
})

# contr_code_anova, specify levels ----
test_that("contr_code_anova, specify levels", {
  x <- 1:2
  lvls <- c("A", "B")
  x1 <- contr_code_anova(x, levels = lvls, base = 1)
  x2 <- contr_code_anova(x, levels = lvls, base = 2)
  mat1 <- matrix(c(-0.5, 0.5), dimnames = list(LETTERS[1:2], ".B-A"))
  mat2 <- matrix(c(0.5, -0.5), dimnames = list(LETTERS[1:2], ".A-B"))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  expect_true(is.factor(x1))
  expect_true(is.factor(x2))
  expect_equal(levels(x1), lvls)
  expect_equal(levels(x2), lvls)
  
  x <- c("A", "B")
  lvls <- c("B", "A")
  x1 <- contr_code_anova(x, levels = lvls, base = 1)
  x2 <- contr_code_anova(x, levels = lvls, base = 2)
  mat1 <- matrix(c(-0.5, 0.5), dimnames = list(LETTERS[2:1], ".A-B"))
  mat2 <- matrix(c(0.5, -0.5), dimnames = list(LETTERS[2:1], ".B-A"))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  expect_true(is.factor(x1))
  expect_true(is.factor(x2))
  expect_equal(levels(x1), lvls)
  expect_equal(levels(x2), lvls)
})

# contr_code_sum ----
test_that("contr_code_sum", {
  x <- factor(1:2, labels = LETTERS[1:2])
  x0 <- contr_code_sum(x)
  x1 <- contr_code_sum(x, omit = 1)
  x2 <- contr_code_sum(x, omit = 2)
  mat1 <- matrix(c(-1, 1), dimnames = list(LETTERS[1:2], ".B-intercept"))
  mat2 <- matrix(c(1, -1), dimnames = list(LETTERS[1:2], ".A-intercept"))
  expect_equal(contrasts(x0), mat2)
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- contr_code_sum(x, omit = 1)
  x2 <- contr_code_sum(x, omit = 2)
  x3 <- contr_code_sum(x)
  
  expect_equal(contrasts(x1) %>% as.vector(), c(-1, 1, 0, -1, 0, 1))
  expect_equal(contrasts(x2) %>% as.vector(), c(1, -1, 0, 0, -1, 1))
  expect_equal(contrasts(x3) %>% as.vector(), c(1, 0, -1, 0, 1, -1))
  expect_equal(
    contrasts(x1) %>% dimnames() %>% `[[`(2), 
    c(".B-intercept", ".C-intercept")
  )
  expect_equal(
    contrasts(x2) %>% dimnames() %>% `[[`(2), 
    c(".A-intercept", ".C-intercept")
  )
  expect_equal(
    contrasts(x3) %>% dimnames() %>% `[[`(2), 
    c(".A-intercept", ".B-intercept")
  )
  
  # non-factor vector
  x <- sample(LETTERS[1:3], 100, T)
  x1 <- contr_code_sum(x, omit = 1)
  x2 <- contr_code_sum(x, omit = 2)
  x3 <- contr_code_sum(x)
  expect_equal(contrasts(x1) %>% as.vector(), c(-1, 1, 0, -1, 0, 1))
  expect_equal(contrasts(x2) %>% as.vector(), c(1, -1, 0, 0, -1, 1))
  expect_equal(contrasts(x3) %>% as.vector(), c(1, 0, -1, 0, 1, -1))
})


# contr_code_treatment ----
test_that("contr_code_treatment", {
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- contr_code_treatment(x)
  x2 <- contr_code_treatment(x, base = "B")
  x3 <- contr_code_treatment(x, base = 3)
  mat1 <- matrix(c(0,1,0,0,0,1), 3, 
                 dimnames = list(LETTERS[1:3], c(".B-A", ".C-A")))
  mat2 <- matrix(c(1,0,0,0,0,1), 3, 
                 dimnames = list(LETTERS[1:3], c(".A-B", ".C-B")))
  mat3 <- matrix(c(1,0,0,0,1,0), 3, 
                 dimnames = list(LETTERS[1:3], c(".A-C", ".B-C")))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  expect_equal(contrasts(x3), mat3)
  
  # non-factor vector
  x <- sample(LETTERS[1:3], 100, T)
  x1 <- contr_code_treatment(x, base = "A")
  x2 <- contr_code_treatment(x, base = "B")
  x3 <- contr_code_treatment(x, base = 3)
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  expect_equal(contrasts(x3), mat3)
})


# contr_code_helmert ----
test_that("contr_code_helmert", {
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- contr_code_helmert(x)
  x2 <- contr_code_helmert(x, levels = c("C", "B", "A"))
  mat1 <- matrix(c(-.5, .5, 0, -1/3, -1/3, 2/3), 3, 
                 dimnames = list(LETTERS[1:3], c(".B-A", ".C-A.B")))
  mat2 <- matrix(c(-.5, .5, 0, -1/3, -1/3, 2/3), 3, 
                 dimnames = list(LETTERS[3:1], c(".B-C", ".A-C.B")))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
  
  # non-factor vector
  x <- sample(LETTERS[1:3], 100, T)
  x1 <- contr_code_helmert(x, levels = c("A", "B", "C"))
  x2 <- contr_code_helmert(x, levels = c("C", "B", "A"))
  expect_equal(contrasts(x1), mat1)
  expect_equal(contrasts(x2), mat2)
})

# contr_code_poly ----
test_that("contr_code_poly", {
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- contr_code_poly(x)
  lvl2 <- c("C", "B", "A")
  x2 <- contr_code_poly(x, levels = lvl2)
  mat1 <- matrix(c(-.7071, 0, .7071, .408, -.8165, .408), 3, 
                 dimnames = list(LETTERS[1:3], c("^1", "^2")))
  mat2 <- mat1
  dimnames(mat2)[[1]] <- lvl2
  expect_equal(contrasts(x1), mat1, tol = .001)
  expect_equal(contrasts(x2), mat2, tol = .001)
  
  # make sure result is an ordered factor with the right levels
  expect_true(is.ordered(x1))
  expect_true(is.ordered(x2))
  expect_equal(levels(x2), lvl2)
  
  # non-factor vector
  x <- sample(LETTERS[1:3], 100, T)
  x1 <- contr_code_poly(x)
  x2 <- contr_code_poly(x, levels = lvl2)
  expect_equal(contrasts(x1), mat1, tol = .001)
  expect_equal(contrasts(x2), mat2, tol = .001)
  expect_true(is.ordered(x1))
  expect_true(is.ordered(x2))
  expect_equal(levels(x2), lvl2)
})

# contr_code_difference ----
test_that("contr_code_difference", {
  x <- factor(1:3, labels = LETTERS[1:3])
  x1 <- contr_code_difference(x)
  x2 <- contr_code_difference(x, levels = c("C", "B", "A"))
  mat1 <- matrix(c(-2/3, 1/3, 1/3, -1/3, -1/3, 2/3), 3, 
                 dimnames = list(LETTERS[1:3], c(".B-A", ".C-B")))
  mat2 <- mat1
  dimnames(mat2) <- list(LETTERS[3:1], c(".B-C", ".A-B"))
  expect_equal(contrasts(x1), mat1, tol = .001)
  expect_equal(contrasts(x2), mat2, tol = .001)
  
  # non-factor vector
  x <- sample(LETTERS[1:3], 100, T)
  x1 <- contr_code_difference(x)
  x2 <- contr_code_difference(x, levels = c("C", "B", "A"))
  expect_equal(contrasts(x1), mat1, tol = .001)
  expect_equal(contrasts(x2), mat2, tol = .001)
})


# change contrast column names ----
test_that("colnames", {
  x <- factor(1:2, labels = LETTERS[1:2])
  
  xA <- contr_code_anova(x, colnames = "Z")
  expect_equal(contrasts(xA) |> colnames(), "Z")
  
  xS <- contr_code_sum(x, colnames = "Z")
  expect_equal(contrasts(xS) |> colnames(), "Z")
  
  xD <- contr_code_difference(x, colnames = "Z")
  expect_equal(contrasts(xD) |> colnames(), "Z")
  
  xT <- contr_code_treatment(x, colnames = "Z")
  expect_equal(contrasts(xT) |> colnames(), "Z")
  
  xH <- contr_code_helmert(x, colnames = "Z")
  expect_equal(contrasts(xH) |> colnames(), "Z")
  
  xP <- contr_code_poly(x, colnames = "Z")
  expect_equal(contrasts(xP) |> colnames(), "Z")
  
  x3 <- factor(1:3, labels = LETTERS[1:3])
  xP3 <- contr_code_poly(x3, colnames = "Z")
  expect_equal(contrasts(xP3) |> colnames(), c("Z1", "Z2"))
})


# add_contrast ----
test_that("add_contrast", {
  btwn <- list(pet = c("cat", "dog", "ferret")) 
  df <- sim_design(between = btwn, n = 1, plot = FALSE)
  names <- list(anova = c("pet.dog-cat", "pet.ferret-cat"), 
                sum = c("pet.cat-intercept", "pet.dog-intercept"), 
                treatment = c("pet.dog-cat", "pet.ferret-cat"), 
                helmert = c("pet.dog-cat", "pet.ferret-cat.dog"), 
                poly = c("pet^1", "pet^2"), 
                difference = c("pet.dog-cat", "pet.ferret-dog"))
  contrasts <- c("anova", "sum", "treatment", "helmert", "poly", "difference")
  
  expect_error(add_contrast(df, "pet", "nope"))
  expect_error(add_contrast(df, "nope", "treatment"))
  
  for (ctrst in contrasts) {
    df1 <- add_contrast(df, "pet", ctrst)
    df2 <- add_contrast(df1, "pet", ctrst)
    cnames <- c(colnames(df), names[[ctrst]])
    expect_equal(colnames(df1), cnames)
    expect_equal(colnames(df2), cnames)
  }
})


test_that("numeric levels", {
  data <- sim_design(list(time = 1:5), long = TRUE, plot = FALSE)
  
  cont <- add_contrast(data, col = "time", contrast = "poly")
  
  expect_true(is.factor(cont$time))
  expect_equal(levels(cont$time), as.character(1:5))
  
  data <- sim_design(between = list(x = c("b", "a")), plot = FALSE)
  cont <- add_contrast(data, col = "x", contrast = "treatment")
  
  expect_true(is.factor(cont$x))
  expect_equal(levels(cont$x), c("b", "a"))
})

test_that("same name levels", {
  data <- sim_design(between = 2, n = 10, plot = FALSE)
  
  cont <- add_contrast(data, "B1", colnames = "B")
  expect_equal(names(cont), c("id", "B1", "y", "B"))
  contname <- cont$B1 |> contrasts() |> colnames()
  expect_equal(contname, "B")
  
  # if colnames has same name as col, don't add cols
  cont <- add_contrast(data, "B1", colnames = "B1")
  expect_equal(names(cont), c("id", "B1", "y"))
  expect_equal(rep(c(-0.5, 0.5), each = 10), cont$B1)
})


test_that("get_contrast_vals", {
  dat <- sim_design(between = list(group = c("A", "B")), 
                    n = 5, plot = FALSE)
              
  obs <- get_contrast_vals(dat$group)
  exp <- data.frame(B = rep(0:1, each = 5))
  expect_equal(obs, exp) 
  
  dat2 <- sim_design(between = list(group = c("A", "B", "C")), 
                    n = 2, plot = FALSE)
  
  obs <- get_contrast_vals(dat2$group)
  exp <- data.frame(B = c(0,0,1,1,0,0),
                    C = c(0,0,0,0,1,1))
  expect_equal(obs, exp) 
  
  dat3 <- add_contrast(dat2, "group", "anova", add_cols = FALSE)
  
  obs <- get_contrast_vals(dat3$group)
  exp <- data.frame(`.B-A` = c(0,0,1,1,0,0)-(1/3),
                    `.C-A` = c(0,0,0,0,1,1)-(1/3))
  colnames(exp) <- c(".B-A", ".C-A")
  expect_equal(obs, exp) 
})

