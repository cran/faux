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

