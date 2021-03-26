## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 5,
  fig.height = 5,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
ggplot2::theme_set(ggplot2::theme_bw())
set.seed(8675309)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(faux)
library(ggplot2)
library(ggExtra)
library(truncnorm)

## -----------------------------------------------------------------------------
mu = 3.5
sd = 2.5
min = 1
max = 7

x_n <- rnorm(1000, mu, sd)
y_t <- norm2trunc(x_n, min, max)

x_t <- rtruncnorm(1000, min, max, mu, sd)
y_n <- trunc2norm(x_t, min , max)

## ----trunc-plot, echo = FALSE, fig.width = 10---------------------------------
grp <- x_n <=7 & x_n >=1
n2t <- ggplot() + geom_point(aes(x_n, y_t, color = grp), show.legend = FALSE) +
  labs(x = "Normal", y = "Truncated Normal", title = "Normal to Truncated Normal") +
  scale_color_manual(values = c("red", "black"))
gm_n2t <- ggMarginal(n2t, type = "histogram")

grp <- y_n <=7 & y_n >=1
t2n <- ggplot() + geom_point(aes(x_t, y_n, color = grp), show.legend = FALSE) +
  labs(y = "Normal", x = "Truncated Normal", title = "Truncated Normal to Normal") +
  scale_color_manual(values = c("red", "black"))
gm_t2n <- ggMarginal(t2n, type = "histogram")

cowplot::plot_grid(gm_n2t, gm_t2n)

## -----------------------------------------------------------------------------
mu = 0
sd = 1
prob = c('very low' = 50, 
         'low' = 80, 
         'medium' = 100, 
         'high' = 40, 
         'very high' = 20)


x_n <- rnorm(1000, mu, sd)
y_l <- norm2likert(x_n, prob)

## ----likert-plot, echo = FALSE, fig.height = 3--------------------------------
n2l <- ggplot() + geom_point(aes(x_n, as.numeric(y_l))) +
  labs(x = "Normal", y = "Likert", title = "Normal to Likert") +
  scale_y_continuous(labels = names(prob))
ggMarginal(n2l, type = "histogram")

## -----------------------------------------------------------------------------
mu = 0
sd = 1
min = 0
max = 100

x_n <- rnorm(1e3, mu, sd)
y_u <- norm2unif(x_n, min, max)

x_u <- runif(1e3, min, max)
y_n <- unif2norm(x_u, mu, sd)

## ----unif-plot, echo = FALSE, fig.width = 10----------------------------------
n2u <- ggplot() + geom_point(aes(x_n, y_u)) +
  labs(x = "Normal", y = "Uniform", title = "Normal to Uniform")
gm_n2u <- ggMarginal(n2u, type = "histogram")

u2n <- ggplot() + geom_point(aes(x_u, y_n)) +
  labs(y = "Normal", x = "Uniform", title = "Uniform to Normal")
gm_u2n <- ggMarginal(u2n, type = "histogram")

cowplot::plot_grid(gm_n2u, gm_u2n)

## -----------------------------------------------------------------------------
# cheating a little to make sure no values near min and max
x <- runif(1e4, 0.1, 0.9) 
y_guess <- unif2norm(x, mu = 100, sd = 10)
y_spec <- unif2norm(x, mu = 100, sd = 10, min = 0, max = 1)

## ---- echo = FALSE, fig.width = 10--------------------------------------------
g_guess <- ggplot() + geom_point(aes(x, y_guess)) +
  labs(y = "Normal", x = "Uniform",
       title = "Range Guessed from x") +
  xlim(0, 1) + ylim(60, 140)
g_spec <- ggplot() + geom_point(aes(x, y_spec)) +
  labs(y = "Normal", x = "Uniform",
       title = "Range Specified with min/max") +
  xlim(0, 1) + ylim(60, 140)
gm_guess <- ggMarginal(g_guess, type = "histogram")
gm_spec <- ggMarginal(g_spec, type = "histogram")

cowplot::plot_grid(gm_guess, gm_spec)

## -----------------------------------------------------------------------------
mu = 0
sd = 1
size = 20
prob = 0.75

x_n <- rnorm(1e4, mu, sd)
y_b <- norm2binom(x_n, size, prob)

x_b <- rbinom(1e4, size, prob)
y_n <- binom2norm(x_b, mu, sd, size)

## ----binom-plot, echo = FALSE, fig.width = 10---------------------------------
n2b <- ggplot() + geom_point(aes(x_n, y_b)) +
  labs(x = "Normal", y = "Binomial", title = "Normal to Binomial")
gm_n2b <- ggMarginal(n2b, type = "histogram")

b2n <- ggplot() + geom_point(aes(x_b, y_n)) +
  labs(y = "Normal", x = "Binomial", title = "Binomial to Normal")
gm_b2n <- ggMarginal(b2n, type = "histogram")

cowplot::plot_grid(gm_n2b, gm_b2n)

## -----------------------------------------------------------------------------
mu = 0
sd = 1
shape1 = 2
shape2 = 5

x_n <- rnorm(1e4, mu, sd)
y_b <- norm2beta(x_n, shape1, shape2)

x_b <- rbeta(1e4, shape1, shape2)
y_n <- beta2norm(x_b, mu, sd)

## ----beta-plot, echo = FALSE, fig.width = 10----------------------------------
n2b <- ggplot() + geom_point(aes(x_n, y_b)) +
  labs(x = "Normal", y = "Beta", title = "Normal to Beta")
gm_n2b <- ggMarginal(n2b, type = "histogram")

b2n <- ggplot() + geom_point(aes(x_b, y_n)) +
  labs(y = "Normal", x = "Beta", title = "Beta to Normal")
gm_b2n <- ggMarginal(b2n, type = "histogram")

cowplot::plot_grid(gm_n2b, gm_b2n)

## -----------------------------------------------------------------------------
mu = 0
sd = 1
shape = 2
rate = 1

x_n <- rnorm(1e4, mu, sd)
y_g <- norm2gamma(x_n, shape, rate)

x_g <- rgamma(1e4, shape, rate)
y_n <- gamma2norm(x_g, mu, sd)

## ----gamma-plot, echo = FALSE, fig.width = 10---------------------------------
n2g <- ggplot() + geom_point(aes(x_n, y_g)) +
  labs(x = "Normal", y = "Gamma", title = "Normal to Gamma")
gm_n2g <- ggMarginal(n2g, type = "histogram")

g2n <- ggplot() + geom_point(aes(x_g, y_n)) +
  labs(y = "Normal", x = "Gamma", title = "Gamma to Normal")
gm_g2n <- ggMarginal(g2n, type = "histogram")

cowplot::plot_grid(gm_n2g, gm_g2n)

