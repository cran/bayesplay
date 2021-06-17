## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bayesplay)

## -----------------------------------------------------------------------------
data_mod <- likelihood(family = "normal", mean = 5.5, sd = 32.35)

## -----------------------------------------------------------------------------
h0_mod <- prior(family = "point", point = 0)

## -----------------------------------------------------------------------------
h1_mod <- prior(family = "normal", mean = 0, sd = 13.3, range = c(0, Inf))

## -----------------------------------------------------------------------------
m1 <- integral(data_mod * h1_mod)
m0 <- integral(data_mod * h0_mod)

## -----------------------------------------------------------------------------
bf <- m1 / m0
bf

## -----------------------------------------------------------------------------
data_mod <- likelihood(family = "normal", mean = 5, sd = 10)

## -----------------------------------------------------------------------------
h1_mod <- prior(family = "uniform", min = 0, max = 20)

## -----------------------------------------------------------------------------
h0_mod <- prior(family = "point", point = 0)

## -----------------------------------------------------------------------------
bf <- integral(data_mod * h1_mod) / integral(data_mod * h0_mod)
bf

## -----------------------------------------------------------------------------
d <- 2.03 / sqrt(80) # convert t to d
data_model <- likelihood(family = "noncentral_d", d, 79)
h0_mod <- prior(family = "point", point = 0)
h1_mod <- prior(family = "cauchy",  scale = 1)

bf <- integral(data_model * h0_mod) / integral(data_model * h1_mod)
bf

## -----------------------------------------------------------------------------
d <- 2.03 / sqrt(80) # convert t to d
data_model <- likelihood(family = "noncentral_d", d, 79)
h0_mod <- prior(family = "point", point = 0)
h1_mod <- prior(family = "normal", mean = 0, sd = 1)

bf <- integral(data_model * h0_mod) / integral(data_model * h1_mod)
bf


## -----------------------------------------------------------------------------
data_model <- likelihood(family = "noncentral_t", 2.03, 79)
h0_mod <- prior(family = "point", point = 0)
h1_mod <- prior(family = "cauchy", location = 0, scale = 1 * sqrt(80))

bf <- integral(data_model * h0_mod) / integral(data_model * h1_mod)
bf

