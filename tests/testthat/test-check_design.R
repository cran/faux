context("test-check_design")

faux_options(plot = FALSE)

# errors ----
test_that("errors", {
  expect_error(check_design(n = -1), "All n must be >= 0")
  expect_warning(check_design(n = 0), "Some cell Ns are 0. Make sure this is intentional.")
  expect_warning(check_design(n = 10.3), "Some cell Ns are not integers. They have been rounded up to the nearest integer.")
  
  # numeric n
  expect_silent(check_design(between = 2, n = list("A1" = 10, "A2" = 20)))
  expect_silent(check_design(between = 2, n = list("A1" = 10, "A2" = "20")))
  expect_error(
    check_design(between = 2, n = list("A1" = 10, "A2" = "B")),
    "All n must be numbers"
  )
  
  # numeric mu
  expect_silent(check_design(between = 2, mu = list("A1" = 10, "A2" = 20)))
  expect_silent(check_design(between = 2, mu = list("A1" = 10, "A2" = "20")))
  expect_error(
    check_design(between = 2, mu = list("A1" = 10, "A2" = "B")),
    "All mu must be numbers"
  )
  
  # numeric sd
  expect_silent(check_design(between = 2, sd = list("A1" = 10, "A2" = 20)))
  expect_silent(check_design(between = 2, sd = list("A1" = 10, "A2" = "20")))
  expect_error(
    check_design(between = 2, sd = list("A1" = 10, "A2" = "B")),
    "All sd must be numbers", fixed = TRUE
  )
  
  expect_error(check_design(sd = -1), "All sd must be >= 0", fixed = TRUE)
  
  err <- "You have duplicate levels for factor(s): A"
  expect_error(check_design(list(A = c("A1", "A1"))), err, fixed = TRUE)
  
  err <- "You have duplicate levels for factor(s): A, B"
  expect_error(check_design(list(A = c("A1", "A1"), B = c("B1", "B1"))), err, fixed = TRUE)
  
  err <- "You have multiple factors with the same name (A). Please give all factors unique names."
  expect_error(check_design(list(A = c("A1", "A2"), A = c("B1", "B2"))), err, fixed = TRUE)
  expect_error(check_design(list(A = c("A1", "A2")), 
                            list(A = c("B1", "B2"))), 
               err, fixed = TRUE)
})

# no factors
test_that("no factors", {
  design <- check_design()
  expect_equal(design$within, list())
  expect_equal(design$between, list())
  expect_equal(design$dv, list(y = "value"))
})

# 2w ----
test_that("2w", {
  within <- list(time = c("night", "day"))
  between <- list()
  design <- check_design(within, between, n = 10)
  
  cell_n <- list(y = 10)
  cell_mu <- list(y = list(night = 0, day = 0))
  cell_sd <- list(y = list(night = 1, day = 1))
  
  expect_equal(design$within, list(time = list(night = "night", day = "day")))
  expect_equal(design$between, list())
  
  expect_equal(design$n, cell_n)
  expect_equal(design$mu, cell_mu)
  expect_equal(design$sd, cell_sd)
  expect_equal(design$dv, list(y = "value"))
  expect_equal(design$id, list(id = "id"))
  
  expect_true("design" %in% class(design))
})

# 2b ----
test_that("2b", {
  within <- list()
  between <- list(time = c("night", "day"))
  design <- check_design(within, between, n = 10)
  
  cell_n <- list(night = 10, day = 10)
  cell_mu <- list(night = list(y=0), day = list(y=0))
  cell_sd <- list(night = list(y=1), day = list(y=1))
  
  expect_equal(design$within, list())
  expect_equal(design$between, list(time = list(night = "night", day = "day")))
  
  expect_equal(design$n, cell_n)
  expect_equal(design$mu, cell_mu)
  expect_equal(design$sd, cell_sd)
  expect_equal(design$dv, list(y = "value"))
  expect_equal(design$id, list(id = "id"))
})

# 2w*2b ----
test_that("2w*2b", {
  within  <- list(time = c("night", "day"))
  between <- list(pet = c("dog", "cat"))
  design  <- check_design(within, between, n = 10)

  cell_n  <- list(dog = 10, cat = 10)
  cell_mu <- list(dog = list(night = 0, day = 0),
                  cat = list(night = 0, day = 0))
  cell_sd <- list(dog = list(night = 1, day = 1),
                  cat = list(night = 1, day = 1))
  
  expect_equal(design$within, list(time = list(night = "night", day = "day")))
  expect_equal(design$between, list(pet = list(dog = "dog", cat = "cat")))
  
  expect_equal(design$n, cell_n)
  expect_equal(design$mu, cell_mu)
  expect_equal(design$sd, cell_sd)
  expect_equal(design$dv, list(y = "value"))
  expect_equal(design$id, list(id = "id"))
})

# 2w*2w*2b*2b ----
test_that("2w*2w*2b*2b", {
  within <- list(
    time = c(night = "night time", day = "day time"), 
    condition = c(A = "condition A", B = "condition B")
  )
  between <- list(
    pet = c(dog = "has dogs", cat = "has cats"), 
    age = c(old = "older", young = "younger")
  )
    
  design <- check_design(within, between)
  
  cells_w <- c("night_A", "night_B", "day_A", "day_B")
  cells_b <- c("dog_old", "dog_young", "cat_old", "cat_young")
  cell_n <- list(dog_old = 100, dog_young = 100, cat_old = 100, cat_young = 100)
  mu_list <- list(night_A = 0, night_B = 0, day_A = 0, day_B = 0)
  cell_mu <- list(
    dog_old = mu_list,
    dog_young = mu_list,
    cat_old = mu_list,
    cat_young = mu_list
  )
  sd_list <- list(night_A = 1, night_B = 1, day_A = 1, day_B = 1)
  cell_sd <- list(
    dog_old = sd_list,
    dog_young = sd_list,
    cat_old = sd_list,
    cat_young = sd_list
  )
  
  expect_equal(design$n, cell_n)
  expect_equal(design$mu, cell_mu)
  expect_equal(design$sd, cell_sd)
  expect_equal(design$dv, list(y = "value"))
  expect_equal(design$id, list(id = "id"))
})

# design spec ----
test_that("design spec", {
  between <- list(
    "B" = c("B1", "B2")
  )
  within <- list(
    "W" = c("W1", "W2")
  )
  n <- list(
    "B1" = 60,
    "B2" = 40
  )
  mu <- list(
    "B1" = c(10, 20),
    "B2" = c(10, 30)
  )
  sd <- list(
    "B1" = c(3, 4),
    "B2" = c(5, 6)
  )
  r <- list(
    "B1" = .2,
    "B2" = .5
  )
  dv <- list(dv = "DV")
  id <- list(sub_id = "id")
  
  design <- check_design(within, between, n, mu, sd, r, dv, id)
  
  design_elements <- c("within", "between", "dv", "id", "n", "mu", "sd", "r", "params")
  
  expect_equal(names(design), design_elements)
  expect_equal(design$dv, dv)
  expect_equal(design$id, id)
})

# anon factors ----
test_that("anon factors", {
  design <- check_design(c(2, 4), c(2, 2))
  
  w <- list(
    A = list(A1="A1", A2="A2"),
    B = list(B1="B1", B2="B2", B3="B3", B4="B4")
  )
  
  b <- list(
    C = list(C1="C1",C2="C2"),
    D = list(D1="D1", D2="D2")
  )
  
  expect_equal(design$within, w)
  expect_equal(design$between, b)
})

# wierd factor names ----
test_that("wierd factor names", {
  # only replaces undervalues
  within <- list("A" = c("A_1", "A 2"),
                 "B" = c("B~1", "B'2"))
  design <- check_design(within)
  
  expect_equal(design$within$A %>% names(), c("A.1", "A 2"))
  expect_equal(design$within$B %>% names(), c("B~1", "B'2"))
})

# make_id ----
test_that("make_id", {
  expect_equal(make_id(10), c("S01", "S02", "S03", "S04", "S05", 
                              "S06", "S07", "S08", "S09", "S10"))
  
  expect_equal(make_id(10, "SUB"), c("SUB01", "SUB02", "SUB03", "SUB04", "SUB05", 
                                     "SUB06", "SUB07", "SUB08", "SUB09", "SUB10"))
  
  expect_equal(make_id(100)[[1]], "S001")
  expect_equal(make_id(1000)[[1]], "S0001")
  expect_equal(make_id(1000, "pokemon_")[[1]], "pokemon_0001")
  expect_equal(make_id(100, digits = 4)[[1]], "S0001")
  
  # named arguments
  expect_equal(make_id(n = 100, prefix = "A", digits = 4)[[1]], "A0001")
  expect_equal(make_id(digits = 4, prefix = "A", n = 100)[[1]], "A0001")
  
  # vector
  expect_equal(make_id(2:4), c("S2", "S3", "S4"))
  expect_equal(make_id(100:200)[[1]], "S100")
})

# params table ----
test_that("params table", {
  des <- check_design()
  params <- data.frame(y = "value", n = 100, mu = 0, sd = 1)
  expect_equal(des$params, params)
  
  within <- list(
    time = c("morning" = "am", "night" = "pm"),
    condition = c("A" = "cond 1", "B" = "cond 2", "C" = "cond 3")
  )
  between <- list(
    pet = c("dog" = "Dogs", "cat" = "Cats"),
    x = c("X1" = "First", "X2" = "Second"))
  
  n <- list(
    dog_X1 = 100,
    dog_X2 = 200,
    cat_X1 = 300,
    cat_X2 = 400
  )
  
  r <- list(
    dog_X1 = seq(.1, by = .025, length.out = 15),
    dog_X2 = seq(.2, by = .025, length.out = 15),
    cat_X1 = seq(.3, by = .025, length.out = 15),
    cat_X2 = seq(.4, by = .025, length.out = 15)
  )
  
  des <- check_design(within, between, n = n, mu = 1:24, 
               sd = 1:24, r = r, id = c(id = "ID"))
  
  nm <- c("pet", "x", "time", "condition", "morning_A", 
          "morning_B", "morning_C", "night_A", "night_B",
          "night_C", "n", "mu", "sd")
  
  expect_true(des$params %>% nrow() == 24)
  expect_true(all(des$params %>% names() == nm))
  
  expected <- c(
    "Design",
    "",
    "* [DV] y: value  ",
    "* [ID] id: ID  ",
    "",
    "Within-subject variables:",
    "",
    "* time: ",
    "  * morning: am",
    "  * night: pm",
    "* condition: ",
    "  * A: cond 1",
    "  * B: cond 2",
    "  * C: cond 3",
    "",
    "Between-subject variables:",
    "",
    "* pet: ",
    "  * dog: Dogs",
    "  * cat: Cats",
    "* x: ",
    "  * X1: First",
    "  * X2: Second"
  )
  
  op <- capture.output(des)
  expect_equal(op[1:length(expected)], expected)
})

faux_options(plot = TRUE)