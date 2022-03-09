#' 
#' @useDynLib PSGD
#' @importFrom Rcpp sourceCpp
#' 
#' @importFrom stats coef predict
#'
#' @title Projected Subset Gradient Descent
#' 
#' @description \code{PSGD} performs a projected subset gradient descent algorithm.
#' 
#' @param x Design matrix.
#' @param y Response vector.
#' @param n_models Number of models into which the variables are split.
#' @param model_type Model type. Must be one of "Linear or Logistic". Default is "Linear".
#' @param include_intercept TRUE or FALSE parameter for the inclusion of an intercept term. Default is TRUE.
#' @param split Number of models that may share a variable.
#' @param size Number of variables that a model may have.
#' @param max_iter Maximum number of iterations in PSGD algorithm.
#' @param cycling_iter Number of random cycling permutations.
#' 
#' @return An object of class PSGD
#' 
#' @export
#' 
#' @author Anthony-Alexander Christidis, \email{anthony.christidis@stat.ubc.ca}
#' 
#' @seealso \code{\link{coef.PSGD}}, \code{\link{predict.PSGD}}
#' 
#' @examples 
#' # Required Libraries
#' library(mvnfast)
#' 
#' # Setting the parameters
#' p <- 100
#' n <- 40
#' n.test <- 1000
#' sparsity <- 0.2
#' rho <- 0.5
#' SNR <- 3
#' 
#' # Generating the coefficient
#' p.active <- floor(p*sparsity)
#' a <- 4*log(n)/sqrt(n)
#' neg.prob <- 0.2
#' nonzero.betas <- (-1)^(rbinom(p.active, 1, neg.prob))*(a + abs(rnorm(p.active)))
#' 
#' # Correlation structure
#' Sigma <- matrix(0, p, p)
#' Sigma[1:p.active, 1:p.active] <- rho
#' diag(Sigma) <- 1
#' true.beta <- c(nonzero.betas, rep(0 , p - p.active))
#' 
#' # Computing the noise parameter for target SNR
#' sigma.epsilon <- as.numeric(sqrt((t(true.beta) %*% Sigma %*% true.beta)/SNR))
#' 
#' # Simulate some data
#' set.seed(1)
#' x.train <- mvnfast::rmvn(n, mu=rep(0,p), sigma=Sigma)
#' y.train <- 1 + x.train %*% true.beta + rnorm(n=n, mean=0, sd=sigma.epsilon)
#' x.test <- mvnfast::rmvn(n.test, mu=rep(0,p), sigma=Sigma)
#' y.test <- 1 + x.test %*% true.beta + rnorm(n.test, sd=sigma.epsilon)
#' 
#' # PSGD Ensemble
#' output <- PSGD(x = x.train, y = y.train, n_models = 5,
#'                model_type = c("Linear", "Logistic")[1], include_intercept = TRUE, 
#'                split = 3, size = 10, 
#'                max_iter = 20,
#'                cycling_iter = 0)
#' psgd.coef <- coef(output, group_index = 1:output$n_models)
#' psgd.predictions <- predict(output, newx = x.test, group_index = 1:output$n_models)
#' mean((y.test - psgd.predictions)^2)/sigma.epsilon^2
#' 
PSGD <- function(x, y, n_models,
                 model_type = c("Linear", "Logistic")[1], include_intercept = TRUE, 
                 split, size, 
                 max_iter = 1e2,
                 cycling_iter = 5){
  
  # Check function input
  Data_Check(x, y, n_models,
             model_type, include_intercept, 
             split, size, 
             max_iter,
             cycling_iter)
  
  # Shuffle the data
  n <- nrow(x)
  p <- ncol(x)
  random.permutation <- sample(1:n, n)
  x.permutation <- x[random.permutation, ]
  y.permutation <- y[random.permutation]

  # CPP input formatting
  if(model_type=="Linear")
    model_type.cpp <- 1 else if(model_type=="Logistic")
      model_type.cpp <- 2 
  include_intercept.cpp <- sum(include_intercept)
  
  # Invoking the CPP code for the algorithm
  output <- Main_PSGD(x = x.permutation, y = y.permutation, n_models = n_models,
                      model_type = model_type.cpp, include_intercept = include_intercept.cpp, 
                      split = split, size = size, 
                      max_iter = max_iter,
                      cycling_iter = cycling_iter)
  
  # Create the object of class "PSGD"
  output$n_models <- n_models
  output$model_type <- model_type
  class(output) <- append("PSGD", class(output))
  
  # Returning the output from the PSGD algorithm
  return(output)
}



