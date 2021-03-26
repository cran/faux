## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
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

## ----plot-cars-orig, fig.cap="Original cars dataset"--------------------------
cars %>%
  ggplot(aes(speed, dist)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x")

## ----plot-cars-sim, fig.cap="Simulated cars dataset"--------------------------
sim_df(cars, 500) %>%
  ggplot(aes(speed, dist)) + 
    geom_point() +
    geom_smooth(method = "lm", formula = "y~x")

## ----plot-mtcars-orig, fig.cap="Original mtcars dataset"----------------------
mtcars %>%
  mutate(transmission = factor(am, labels = c("automatic", "manual"))) %>%
  ggplot(aes(hp, wt, color = transmission)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x")

## ----plot-iris-sim, fig.cap="Simulated iris dataset"--------------------------
sim_df(mtcars, 50 , between = "am") %>%
  mutate(transmission = factor(am, labels = c("automatic", "manual"))) %>%
  ggplot(aes(hp, wt, color = transmission)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x")

## -----------------------------------------------------------------------------
exact_mtcars <- sim_df(mtcars, 50, between = "am", empirical = TRUE)

## ----plot-iris-sim-round, fig.cap="Simulated iris dataset (rounded)"----------
sim_df(mtcars, 50, between = "am") %>%
  mutate(hp = round(hp),
         transmission = factor(am, labels = c("automatic", "manual"))) %>%
  ggplot(aes(hp, wt, color = transmission)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x")

## -----------------------------------------------------------------------------
data <- sim_design(2, 2, n = 10, plot = FALSE)
data$A1[1:3] <- NA
data$A2[1:6] <- NA
data

## -----------------------------------------------------------------------------
simdat <- sim_df(data, between = "B", n = 1000, 
                 missing = TRUE)

## ---- echo = FALSE, results = 'asis'------------------------------------------
simdat %>%
  mutate(A1 = ifelse(is.na(A1), "NA", "not NA"),
         A2 = ifelse(is.na(A2), "NA", "not NA")) %>%
  count(B, A1, A2) %>%
  group_by(B) %>%
  mutate(n = round(n/sum(n), 2)) %>%
  knitr::kable()

