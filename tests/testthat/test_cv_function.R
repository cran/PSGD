# --------------------------------------------------
# Test Script - Output from cv.SplitGLM Function
# --------------------------------------------------

# Required libraries
library(mvnfast)
library(PSGD)

# Context of test script
context("Verify output of cross-validation function.")

# There should be an error if we want to compute the IF TS, and no returns are provided
test_that("Error in the cross-validation function.", {

  # Setting the parameters
  p <- 100
  n <- 40
  n.test <- 1000
  sparsity <- 0.2
  rho <- 0.5
  SNR <- 3

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
  
  # # CV PSGD Ensemble
  # output <- cv.PSGD(x = x.train, y = y.train, n_models = 5,
  #                   model_type = c("Linear", "Logistic")[1], include_intercept = TRUE, 
  #                   split = c(2, 3), size = c(20, 30), 
  #                   max_iter = 20,
  #                   cycling_iter = 0,
  #                   n_folds = 5,
  #                   n_threads = 1)
  # psgd.coef <- coef(output, group_index = 1:object$n_models)
  # psgd.predictions <- predict(output, newx = x.test, group_index = 1:object$n_models)
  # mean((y.test - psgd.predictions)^2)/sigma.epsilon^2
  
  expect_vector(numeric(ncol(x.train)+1))

})




