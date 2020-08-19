context("test-plot_design")

user_opts <- faux_options("sep", "verbose", "plot", "connection")
on.exit(faux_options(user_opts))

faux_options(plot = FALSE)

# error ----
test_that("error", {
  df <- sim_design()
  attributes(df)$design <- NULL
  
  err <- "The data table must have a design attribute"
  expect_error(plot_design(df), err)
  expect_error(plot_design(iris), err)
  
  err <- "x must be a design list or a data frame"
  expect_error(plot_design(1), err)
  expect_error(plot_design("A"), err)
  expect_error(plot_design(matrix(1:100, 10)), err)
})

# wide2long ----
test_that("wide2long", {
  set.seed(1)
  df <- sim_design()
  p1 <- plot_design(df)
  expect_equal(class(p1), c("gg", "ggplot"))
  
  set.seed(1)
  df <- sim_design(long = TRUE)
  p2 <- plot_design(df)
  expect_equal(class(p2), c("gg", "ggplot"))
  expect_equal(p1, p2)
})

# order ----
test_that("order", {
  des <- check_design(c(2,2,2,2,2,2))
  
  p <- plot_design(des, "F", "E", "D", "C", "B", "A")
  
  expect_equal(p$labels$x, "E")
  expect_equal(p$labels$fill, "F")
  expect_equal(p$labels$colour, "F")
  expect_equal(p$facet$params$rows %>% names(), c("D", "C"))
  expect_equal(p$facet$params$cols %>% names(), c("B", "A"))
})

# from design ----
test_that("from design", {
  s0 <- check_design() %>% plot_design()
  s1 <- check_design(2) %>% plot_design()
  s2 <- check_design(c(2,2)) %>% plot_design()
  s3 <- check_design(c(2,2,2)) %>% plot_design()
  s4 <- check_design(c(2,2,2,2)) %>% plot_design()
  s5 <- check_design(c(2,2,2,2,2)) %>% plot_design()
  s6 <- check_design(c(2,2,2,2,2,2)) %>% plot_design()
  
  expect_equal(s0$labels$x, "value")
  expect_equal(s0$labels$y, "value")
  expect_equal(s0$labels$fill, "fill")
  expect_equal(s0$labels$colour, "colour")
  expect_equal(s0$facet$params, list())
  
  expect_equal(s1$labels$x, "A")
  expect_equal(s1$labels$y, "value")
  expect_equal(s1$labels$fill, "A")
  expect_equal(s1$labels$colour, "A")
  expect_equal(s1$facet$params, list())
  
  expect_equal(s6$labels$x, "B")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "A")
  expect_equal(s6$labels$colour, "A")
  expect_equal(s2$facet$params, list())
  
  expect_equal(s6$labels$x, "B")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "A")
  expect_equal(s6$labels$colour, "A")
  expect_equal(s3$facet$params$rows %>% names(), c("C"))
  expect_equal(s3$facet$params$cols %>% names(), character(0))
  
  expect_equal(s6$labels$x, "B")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "A")
  expect_equal(s6$labels$colour, "A")
  expect_equal(s4$facet$params$rows %>% names(), c("C"))
  expect_equal(s4$facet$params$cols %>% names(), c("D"))
  
  expect_equal(s6$labels$x, "B")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "A")
  expect_equal(s6$labels$colour, "A")
  expect_equal(s5$facet$params$rows %>% names(), c("C"))
  expect_equal(s5$facet$params$cols %>% names(), c("D", "E"))
  
  expect_equal(s6$labels$x, "B")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "A")
  expect_equal(s6$labels$colour, "A")
  expect_equal(s6$facet$params$rows %>% names(), c("C", "D"))
  expect_equal(s6$facet$params$cols %>% names(), c("E", "F"))
})

# from data ----
test_that("from data", {
  s0 <- sim_design() %>% plot_design()
  s1 <- sim_design(2) %>% plot_design()
  s2 <- sim_design(c(2,2)) %>% plot_design()
  s3 <- sim_design(c(2,2,2)) %>% plot_design()
  s4 <- sim_design(c(2,2,2,2)) %>% plot_design()
  s5 <- sim_design(c(2,2,2,2,2)) %>% plot_design()
  s6 <- sim_design(c(2,2,2,2,2,2)) %>% plot_design()
  
  expect_equal(s0$labels$x, "value")
  expect_equal(s0$labels$y, "value")
  expect_equal(s0$labels$fill, "fill")
  expect_equal(s0$labels$colour, "colour")
  expect_equal(s0$facet$params, list())
  
  expect_equal(s1$labels$x, "A")
  expect_equal(s1$labels$y, "value")
  expect_equal(s1$labels$fill, "A")
  expect_equal(s1$labels$colour, "A")
  expect_equal(s1$facet$params, list())
  
  expect_equal(s6$labels$x, "B")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "A")
  expect_equal(s6$labels$colour, "A")
  expect_equal(s2$facet$params, list())
  
  expect_equal(s6$labels$x, "B")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "A")
  expect_equal(s6$labels$colour, "A")
  expect_equal(s3$facet$params$rows %>% names(), c("C"))
  expect_equal(s3$facet$params$cols %>% names(), character(0))
  
  expect_equal(s6$labels$x, "B")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "A")
  expect_equal(s6$labels$colour, "A")
  expect_equal(s4$facet$params$rows %>% names(), c("C"))
  expect_equal(s4$facet$params$cols %>% names(), c("D"))
  
  expect_equal(s6$labels$x, "B")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "A")
  expect_equal(s6$labels$colour, "A")
  expect_equal(s5$facet$params$rows %>% names(), c("C"))
  expect_equal(s5$facet$params$cols %>% names(), c("D", "E"))
  
  expect_equal(s6$labels$x, "B")
  expect_equal(s6$labels$y, "value")
  expect_equal(s6$labels$fill, "A")
  expect_equal(s6$labels$colour, "A")
  expect_equal(s6$facet$params$rows %>% names(), c("C", "D"))
  expect_equal(s6$facet$params$cols %>% names(), c("E", "F"))
})

# 2w ----
test_that("2w", {
  within <- list(time = c(day = "Tested during the day", 
                          night = "Tested at night"))
  between <- list()
  mu <- c(1,2)
  dv = "rt"
  id = "sub_id"
  d <- sim_design(within, between, mu = mu, dv = dv, id = id, long = TRUE)
  p <- plot_design(d)
  expect_equal(class(p), c("gg", "ggplot"))
  expect_equal(p$labels$x, "time")
  expect_equal(p$labels$y, "rt")
  expect_equal(p$labels$fill, "time")
  expect_equal(p$labels$colour, "time")
})

# 2w*2b ----
test_that("2w*2b", {
  within <- list(time = c("day", "night"))
  between <- list(pet = c("dog", "cat"))
  mu <- list(dog = 1:2, cat = 3:4)
  p <- check_design(within, between, mu = mu, plot = 0) %>% 
    plot_design()
  expect_equal(class(p), c("gg", "ggplot"))
  
  # axis labels
  within <- list(time = c(day = "Tested during the day", 
                          night = "Tested at night"))
  between <- list(pet = c(dog = "Has a dog", cat = "Has a cat"))
  mu <- list(
    dog = c(1,2),
    cat = c(3,4)
  )
  d <- sim_design(within, between, mu = mu, long = TRUE)
  p <- plot_design(d)
  expect_equal(class(p), c("gg", "ggplot"))
  expect_equal(p$labels$x, "pet")
  expect_equal(p$labels$y, "value")
  expect_equal(p$labels$fill, "time")
  expect_equal(p$labels$colour, "time")
})

# 2w*2w*2b ----
test_that("2w*2w*2b", {
  within <- list(time = c("day", "night"), condition = c("A", "B"))
  between <- list(pet = c("dog", "cat"))
  mu <- list(dog = 1:4, cat = 2:5)
  p <- check_design(within, between, mu = mu) %>%
    plot_design()
  expect_equal(class(p), c("gg", "ggplot"))
  expect_equal(p$labels$x, "condition")
  expect_equal(p$labels$y, "value")
  expect_equal(p$labels$fill, "time")
  expect_equal(p$labels$colour, "time")
})

# 2w*2w*2b*2b ----
test_that("2w*2w*2b*2b", {
  within <- list(pet = c("ferret", "dog", "cat"), condition = c("A", "B"))
  between <- list(time = c("night", "day"), age = c("old", "young"))
  mu <- list(night_old = 1:6, 
             day_old = 2:7,
             night_young = 3:8, 
             day_young = 4:9)
  design <- check_design(within, between, mu = mu)
  p <- plot_design(design)
  expect_equal(class(p), c("gg", "ggplot"))
  expect_equal(names(p$facet$params$rows), "time")
  expect_equal(names(p$facet$params$cols), "age")
  expect_equal(p$labels$x, "condition")
  expect_equal(p$labels$y, "value")
  expect_equal(p$labels$fill, "pet")
  expect_equal(p$labels$colour, "pet")
})

# geoms ----
test_that("geoms", {
  dat <- sim_design(c(2,2,2,2), n = 25, sd = 5, 
                    mu = 1:16)
  
  default <- plot_design(dat)
  manual <- plot_design(dat, geoms = c("violin", "box"))
  expect_equal(default, manual)
  
  v  <- plot_design(dat, geoms = "violin")
  b  <- plot_design(dat, geoms = "box")
  sd <- plot_design(dat, geoms = "pointrangeSD")
  se <- plot_design(dat, geoms = "pointrangeSE")
  j  <- plot_design(dat, geoms = "jitter")
  
  v_class  <- v$layers[[1]]$geom  %>% class()
  b_class  <- b$layers[[1]]$geom  %>% class()
  sd_class <- sd$layers[[1]]$geom %>% class()
  se_class <- se$layers[[1]]$geom %>% class()
  j_class  <- j$layers[[1]]$geom  %>% class()
  
  expect_equal(v_class[[1]], "GeomViolin")
  expect_equal(b_class[[1]], "GeomBoxplot")
  expect_equal(sd_class[[1]], "GeomPointrange")
  expect_equal(se_class[[1]], "GeomPointrange")
  expect_equal(j_class[[1]], "GeomPoint")
  
  v_sd <- plot_design(dat, geoms = c("violin", "pointrangeSD"))
  v_sd1 <- v_sd$layers[[1]]$geom  %>% class()
  v_sd2 <- v_sd$layers[[2]]$geom  %>% class()
  expect_equal(v_sd1[1], "GeomViolin")
  expect_equal(v_sd2[1], "GeomPointrange")
  
  # only does one of pointrange - should default to SD
  se_sd <- plot_design(dat, geoms = c("pointrangeSE", "pointrangeSD"))
  sd_se <- plot_design(dat, geoms = c("pointrangeSD", "pointrangeSE"))
  expect_equal(length(se_sd$layers), 1)
  expect_equal(length(sd_se$layers), 1)
  
  # getrid of plot_env$geoms to compare
  se_sd$plot_env$geoms <- NULL
  sd_se$plot_env$geoms <- NULL
  sd$plot_env$geoms <- NULL
  expect_equal(se_sd, sd_se)
  expect_equal(sd, sd_se)
})

# S3 functions ----
test_that("S3 functions", {
  des <- check_design()
  dat <- sim_design()
  p_des <- plot(des)$layers[[1]]$geom %>% class()
  p_dat <- plot(dat)$layers[[1]]$geom %>% class()
  
  expect_equal(p_des[1], "GeomPointrange")
  expect_equal(p_dat[1], "GeomViolin")
  
  p_des <- plot(des, geoms=c("jitter", "violin"))$layers[[1]]$geom %>% class()
  p_dat <- plot(dat, geoms=c("jitter", "violin"))$layers[[1]]$geom %>% class()
  
  expect_equal(p_des[1], "GeomPoint")
  expect_equal(p_dat[1], "GeomPoint")
})

# reps ----
test_that("reps", {
  
  data <- sim_design(2, 2, rep = 3)
  p <- plot_design(data)
  
  expect_equal(p$facet$params, list())
})

faux_options(plot = TRUE)