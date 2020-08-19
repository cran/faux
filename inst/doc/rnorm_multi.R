## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 8,
  fig.height = 5,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
ggplot2::theme_set(ggplot2::theme_bw())
set.seed(8675309)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(faux)

## ----multirnorm-example-------------------------------------------------------

dat <- rnorm_multi(n = 100, 
                  mu = c(0, 20, 20),
                  sd = c(1, 5, 5),
                  r = c(0.5, 0.5, 0.25), 
                  varnames = c("A", "B", "C"),
                  empirical = FALSE)


## -----------------------------------------------------------------------------
bvn <- rnorm_multi(100, 5, 0, 1, .3, varnames = letters[1:5])

## ----vvmatrix-----------------------------------------------------------------
cmat <- cor(iris[,1:4])
bvn <- rnorm_multi(100, 4, 0, 1, cmat, 
                  varnames = colnames(cmat))

## -----------------------------------------------------------------------------
cmat <- c(1, .3, .5,
          .3, 1, 0,
          .5, 0, 1)
bvn <- rnorm_multi(100, 3, 0, 1, cmat, 
                  varnames = c("first", "second", "third"))

## -----------------------------------------------------------------------------
rho1_2 <- .3
rho1_3 <- .5
rho1_4 <- .5
rho2_3 <- .2
rho2_4 <- 0
rho3_4 <- -.3
cmat <- c(rho1_2, rho1_3, rho1_4, rho2_3, rho2_4, rho3_4)
bvn <- rnorm_multi(100, 4, 0, 1, cmat, 
                  varnames = letters[1:4])

## -----------------------------------------------------------------------------
bvn <- rnorm_multi(100, 5, 0, 1, .3, 
                  varnames = letters[1:5], 
                  empirical = T)

## -----------------------------------------------------------------------------
sl <- iris$Sepal.Length

sl.5.v1 <- rnorm_pre(sl, mu = 10, sd = 2, r = 0.5)
sl.5.v2 <- rnorm_pre(sl, mu = 10, sd = 2, r = 0.5)

## ---- echo = FALSE, results='asis'--------------------------------------------
data.frame(sl, sl.5.v1, sl.5.v2) %>%
  get_params() %>% knitr::kable(digits = 3, caption = "rnorm_pre")

## -----------------------------------------------------------------------------
sl.5.v1 <- rnorm_pre(sl, mu = 10, sd = 2, r = 0.5, empirical = TRUE)
sl.5.v2 <- rnorm_pre(sl, mu = 10, sd = 2, r = 0.5, empirical = TRUE)

## ---- echo = FALSE, results='asis'--------------------------------------------
data.frame(sl, sl.5.v1, sl.5.v2) %>%
  get_params() %>% knitr::kable(digits = 3, caption = "rnorm_pre with empirical = TRUE")

