## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = TRUE
)

## ----setup--------------------------------------------------------------------
library(bayesplay)

## -----------------------------------------------------------------------------
norm_mod <- likelihood(family = "normal", mean = 5.5, sd = 32.35)
norm_mod

## -----------------------------------------------------------------------------
plot(norm_mod)

## -----------------------------------------------------------------------------
t_mod <- likelihood(family = "student_t", mean = 10, sd = 5, df = 15)
t_mod

## -----------------------------------------------------------------------------
plot(t_mod)

## -----------------------------------------------------------------------------
point_prior <- prior(family = "point", point = 0)
plot(point_prior)

## -----------------------------------------------------------------------------
uniform_prior <- prior(family = "uniform", min = 10, max = 20)
plot(uniform_prior) 

## -----------------------------------------------------------------------------
normal_prior <- prior(family = "normal", mean = 10, sd = 10)
plot(normal_prior)

## -----------------------------------------------------------------------------
half_normal_prior <- prior(family = "normal", mean = 10, sd = 10, range = c(10, Inf))
plot(half_normal_prior)

## -----------------------------------------------------------------------------
library(ggplot2)

## -----------------------------------------------------------------------------
plot(t_mod) + labs(x = "data", y = "likelihood", title = "t likelihood")

## -----------------------------------------------------------------------------
plot(uniform_prior) + xlim(-100,100)

## -----------------------------------------------------------------------------
plot(norm_mod) +
	labs(title  = "normal likelihood") + 
	theme_linedraw()

## -----------------------------------------------------------------------------

data_model <- likelihood("noncentral_d", d = 0, n = 20)

d_model1 <- extract_predictions(data_model * prior("cauchy", 0, .707))
d_model2 <- extract_predictions(data_model * prior("point", 0))
visual_compare(d_model1, d_model2)

## -----------------------------------------------------------------------------
plot(extract_posterior(data_model * prior("cauchy", 0, .707)), add_prior = TRUE)

## -----------------------------------------------------------------------------
visual_compare(d_model1, d_model2) +
    scale_colour_manual(values = c("green", "blue"),
                        labels = c("d_model1", "d_model2"), name = "Model")

## -----------------------------------------------------------------------------
plot(extract_posterior(data_model * prior("cauchy", 0, .707)), add_prior = TRUE) +
    scale_colour_manual(values = c("green", "blue"),
                        labels = c("posterior", "prior"), name = NULL)

## -----------------------------------------------------------------------------
visual_compare(d_model1, d_model2) +
    scale_linetype_manual(values = c(1, 2),
                        labels = c("d_model1", "d_model2"), name = "Model")

## -----------------------------------------------------------------------------
plot(extract_posterior(data_model * prior("cauchy", 0, .707)), add_prior = TRUE) +
    scale_linetype_manual(values = c(1, 2),
                        labels = c("posterior", "prior"), name = NULL)

## -----------------------------------------------------------------------------
visual_compare(d_model1, d_model2) +
    scale_linetype_manual(values = c(1, 2),
                        labels = c("d_model1", "d_model2"), name = NULL) +
    scale_colour_manual(values = c("grey", "black"),
                        labels = c("d_model1", "d_model2"), name = NULL)

## -----------------------------------------------------------------------------
plot(extract_posterior(data_model * prior("cauchy", 0, .707)), add_prior = TRUE) +
    scale_linetype_manual(values = c(1, 2),
                        labels = c("posterior", "prior"), name = NULL) + 
    scale_colour_manual(values = c("black", "black"),
                        labels = c("posterior", "prior"), name = NULL)

