## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bayesplay)
library(ggplot2)

## -----------------------------------------------------------------------------
# first the likelihood
l <- likelihood(family = "binomial", successes = 2, trials = 10)
l

## -----------------------------------------------------------------------------
# then the prior
p <- prior(family = "beta", alpha = 2.5, beta = 3.5)
p

## -----------------------------------------------------------------------------
prod <- l * p
prod

## -----------------------------------------------------------------------------
plot(l) + labs(title = "binomial likelihood", subtitle = "2 successes out of 10 trials")

## -----------------------------------------------------------------------------
plot(p) + labs(title = "beta prior", subtitle = "alpha = 2.5, beta = 3.5") 

## -----------------------------------------------------------------------------
plot(prod) + labs(title = "product of the likelihood and prior", subtitle = "for a binomial likelihood and beta prior")

## -----------------------------------------------------------------------------
integral(prod)

## -----------------------------------------------------------------------------
posterior1 <- extract_posterior(prod)
posterior1

## -----------------------------------------------------------------------------
plot(posterior1) + labs(title = "posterior distribution", subtitle = "for a binomial likelihood and beta prior")

## -----------------------------------------------------------------------------
posterior2 <- extract_posterior(likelihood("normal", 10, 14) * prior("uniform", -30, 30))
plot(posterior2) + labs(title = "posterior distribution", subtitle = "normal likelihood and uniform prior")

## -----------------------------------------------------------------------------
posterior3 <- extract_posterior(likelihood("noncentral_d", .8, 25) * prior("cauchy", 0, .707))
plot(posterior3) + labs(title = "posterior distribution", subtitle = "noncentral 'd' likelihood and cauchy prior")

## -----------------------------------------------------------------------------
plot(posterior1, add_prior = TRUE) + labs(title = "prior and posterior distribution", subtitle = "for a binomial likelihood and beta prior")

## -----------------------------------------------------------------------------
plot(posterior2, add_prior = TRUE) + labs(title = "prior and posterior distribution", subtitle = "normal likelihood and uniform prior")

## -----------------------------------------------------------------------------
plot(posterior3, add_prior = TRUE) + labs(title = "prior posterior distribution", subtitle = "noncentral 'd' likelihood and cauchy prior")

## -----------------------------------------------------------------------------
data_model <- likelihood("binomial", successes = 2, trials = 10)
prior_alt <- prior("beta", 2.5, 3.5)
prior_null <- prior("point", 0.5)
bf <- integral(data_model * prior_alt) / integral(data_model * prior_null)
summary(bf)

## -----------------------------------------------------------------------------
bf1 <- sd_ratio(posterior1, point = 0.5)
summary(bf1)

## -----------------------------------------------------------------------------
identical(bf, bf1)

## -----------------------------------------------------------------------------
bf2 <- sd_ratio(posterior2, point = 0)
summary(bf2)

## -----------------------------------------------------------------------------
bf3 <- sd_ratio(posterior3, point = 0)
summary(bf3)

## -----------------------------------------------------------------------------
data_model <- likelihood("binomial", successes = 5, trials = 10)

## -----------------------------------------------------------------------------
hypothesis1 <- prior("point", .2)
hypothesis2 <- prior("point", .8)

## -----------------------------------------------------------------------------
# prior for model 1
plot(hypothesis1)

## -----------------------------------------------------------------------------
# prior for model 2
plot(hypothesis2)

## -----------------------------------------------------------------------------
model1 <- data_model * hypothesis1 
model2 <- data_model * hypothesis2

## -----------------------------------------------------------------------------
# predictions of model 1
model1_predictions  <- extract_predictions(model1)
plot(model1_predictions)

## -----------------------------------------------------------------------------
# predictions of model 2
model2_predictions  <- extract_predictions(model2)
plot(model2_predictions)

## -----------------------------------------------------------------------------
visual_compare(model1, model2)

## -----------------------------------------------------------------------------
visual_compare(model1, model2, ratio = TRUE)


## -----------------------------------------------------------------------------
data_model2 <- likelihood("noncentral_d", d = 0, n = 20)

## -----------------------------------------------------------------------------
d_model1 <- extract_predictions(data_model2 * prior("cauchy", 0, .707))
d_model2 <- extract_predictions(data_model2 * prior("point", 0))

## -----------------------------------------------------------------------------
plot(d_model1)

## -----------------------------------------------------------------------------
plot(d_model2)

## -----------------------------------------------------------------------------
visual_compare(d_model1, d_model2)

## -----------------------------------------------------------------------------
visual_compare(d_model1, d_model2, ratio = TRUE)

