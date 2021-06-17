## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bayesplay)

## -----------------------------------------------------------------------------
t <- 2.03
n <- 80
data_model <- likelihood("noncentral_t", t = t, df = n - 1)
plot(data_model)

## -----------------------------------------------------------------------------
plot(prior("cauchy", location = 0, scale = 1))

## -----------------------------------------------------------------------------
plot(prior("cauchy", location = 0, scale = .707))

## -----------------------------------------------------------------------------
alt_prior <- prior("cauchy", location = 0, scale = 1 * sqrt(n)) 
plot(alt_prior)

## -----------------------------------------------------------------------------
null_prior <- prior("point", point = 0) 
plot(null_prior)

## -----------------------------------------------------------------------------
bf_onesample_1 <- integral(data_model * alt_prior) / integral(data_model * null_prior)
summary(bf_onesample_1)

## -----------------------------------------------------------------------------
d <- t / sqrt(n)
data_model2 <- likelihood("noncentral_d", d = d, n = n) 
plot(data_model2)

## -----------------------------------------------------------------------------
alt_prior2 <- prior("cauchy", location = 0, scale = 1)
plot(alt_prior2)

## -----------------------------------------------------------------------------
bf_onesample_2 <- integral(data_model2 * alt_prior2) / integral(data_model2 * null_prior)
summary(bf_onesample_2)

## -----------------------------------------------------------------------------
set.seed(2125519)
group1 <- 25 + scale(rnorm(n = 15)) * 15
group2 <- 35 + scale(rnorm(n = 16)) * 16

## -----------------------------------------------------------------------------
t_result <- t.test(x = group1, y = group2, paired = FALSE, var.equal = TRUE)
t_result

## -----------------------------------------------------------------------------
t <- t_result$statistic
t

## -----------------------------------------------------------------------------
df <- t_result$parameter
df 

## -----------------------------------------------------------------------------
data_model3 <- likelihood("noncentral_t", t = t, df = df)

## -----------------------------------------------------------------------------
n1 <- length(group1)
n2 <- length(group2)

## -----------------------------------------------------------------------------
alt_prior3 <- prior("cauchy", location = 0, scale = 1 * sqrt((n1 * n2) / (n1 + n2)))
plot(alt_prior3)

## -----------------------------------------------------------------------------
bf_independent_1 <- integral(data_model3 * alt_prior3) / integral(data_model3 * null_prior) 
summary(bf_independent_1)

## -----------------------------------------------------------------------------
m1 <- mean(group1)
m2 <- mean(group2)
s1 <- sd(group1)
s2 <- sd(group2)
md_diff <- m1 - m2
sd_pooled <- sqrt((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))
d <- md_diff / sd_pooled
d

## -----------------------------------------------------------------------------
data_model4 <- likelihood("noncentral_d2", d = d, n1 = n1, n2 = n2)
data_model4

## -----------------------------------------------------------------------------
plot(data_model4)

## -----------------------------------------------------------------------------
alt_prior4 <- prior("cauchy", location = 0, scale = 1)
plot(alt_prior4)

## -----------------------------------------------------------------------------
bf_independent_2 <- integral(data_model4 * alt_prior4) / integral(data_model4 * null_prior)
summary(bf_independent_2)

