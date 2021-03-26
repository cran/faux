## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
ggplot2::theme_set(ggplot2::theme_bw())
set.seed(8675309)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(dplyr)
library(tidyr)
library(lme4)
library(broom.mixed)
library(faux)

## -----------------------------------------------------------------------------
dat_cc <- sim_mixed_cc(
  sub_n = 100,   # subject sample size
  item_n = 50,  # item sample size
  grand_i = 10,  # overall mean of the score
  sub_sd = 1,   # SD of subject random intercepts
  item_sd = 2,  # SD of item random intercepts
  error_sd = 3  # SD of residual error
)

## ---- results='asis'----------------------------------------------------------
lme4::lmer(y ~ 1 + (1 | sub_id) + (1 | item_id), data = dat_cc) %>%
  broom.mixed::tidy() %>%
  knitr::kable(digits = 3)

## ---- results='asis'----------------------------------------------------------
dat_cc <- sim_mixed_cc(100, 50, 0, 1.5, 3, 10)

lme4::lmer(y ~ 1 + (1 | sub_id) + (1 | item_id), data = dat_cc) %>%
  broom.mixed::tidy() %>%
  knitr::kable(digits = 3)

## ---- eval = FALSE------------------------------------------------------------
#  
#  new_dat <- sim_mixed_df(fr4,
#                          sub_n = 100,
#                          item_n = 50,
#                          dv = "rating",
#                          sub_id = "rater_id",
#                          item_id = "face_id")
#  

