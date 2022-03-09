
[![Build
Status](https://app.travis-ci.com/AnthonyChristidis/PSGD.svg?branch=master)](https://app.travis-ci.com/AnthonyChristidis/PSGD)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/PSGD)](https://cran.r-project.org/package=PSGD)
[![Downloads](http://cranlogs.r-pkg.org/badges/PSGD)](https://cran.r-project.org/package=PSGD)

# PSGD

This package provides functions to generate ensembles of generalized
linear models using a projected subset gradient descent algorithm.

------------------------------------------------------------------------

### Installation

You can install the **stable** version on [R
CRAN](https://cran.r-project.org/package=PSGD).

``` r
install.packages("PSGD", dependencies = TRUE)
```

You can install the **development** version from
[GitHub](https://github.com/AnthonyChristidis/PSGD).

``` r
library(devtools)
devtools::install_github("AnthonyChristidis/PSGD")
```

### Usage

``` r
# Required Libraries
library(mvnfast)

# Setting the parameters
p <- 100
n <- 40
n.test <- 2000
sparsity <- 0.2
rho <- 0.5
SNR <- 3
set.seed(0)

# Generating the coefficient
p.active <- floor(p*sparsity)
a <- 4*log(n)/sqrt(n)
neg.prob <- 0.2
nonzero.betas <- (-1)^(rbinom(p.active, 1, neg.prob))*(a + abs(rnorm(p.active)))

# Correlation structure
Sigma <- matrix(0, p, p)
Sigma[1:p.active, 1:p.active] <- rho
diag(Sigma) <- 1
true.beta <- c(nonzero.betas, rep(0 , p - p.active))

# Computing the noise parameter for target SNR
sigma.epsilon <- as.numeric(sqrt((t(true.beta) %*% Sigma %*% true.beta)/SNR))

# Simulate some data
set.seed(1)
x.train <- mvnfast::rmvn(n, mu=rep(0,p), sigma=Sigma)
y.train <- 1 + x.train %*% true.beta + rnorm(n=n, mean=0, sd=sigma.epsilon)
x.test <- mvnfast::rmvn(n.test, mu=rep(0,p), sigma=Sigma)
y.test <- 1 + x.test %*% true.beta + rnorm(n.test, sd=sigma.epsilon)

# CV PSGD Ensemble
output <- cv.PSGD(x = x.train, y = y.train, n_models = 5,
                  model_type = c("Linear", "Logistic")[1], include_intercept = TRUE, 
                  split = c(2, 3), size = c(10, 15), 
                  max_iter = 20,
                  cycling_iter = 0,
                  n_folds = 5,
                  n_threads = 1)
psgd.coef <- coef(output, group_index = 1:object$n_models)
psgd.predictions <- predict(output, newx = x.test, group_index = 1:object$n_models)
mean((y.test - psgd.predictions)^2)/sigma.epsilon^2
```

### License

This package is free and open source software, licensed under GPL (&gt;=
2).
