#'
#' @title Cross-Validation - Projected Subset Gradient Descent
#' 
#' @description \code{cv.PSGD} performs the CV procedure for a projected subset gradient descent algorithm.
#' 
#' @param x Design matrix.
#' @param y Response vector.
#' @param n_models Number of models into which the variables are split.
#' @param model_type Model type. Must be one of "Linear or Logistic". Default is "Linear".
#' @param include_intercept TRUE or FALSE parameter for the inclusion of an intercept term. Default is TRUE.
#' @param split_grid Grid for number of models that may share a variable.
#' @param size_grid Grid for number of variables that a model may have.
#' @param max_iter Maximum number of iterations in PSGD algorithm.
#' @param cycling_iter Number of random cycling permutations.
#' @param n_folds Number of cross-validation folds. Default is 5
#' @param n_threads Number of threads. Default is 1.
#' 
#' @return An object of class cv.PSGD
#' 
#' @export
#' 
#' @author Anthony-Alexander Christidis, \email{anthony.christidis@stat.ubc.ca}
#' 
#' @seealso \code{\link{coef.cv.PSGD}}, \code{\link{predict.cv.PSGD}}
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
#' # CV PSGD Ensemble
#' output <- cv.PSGD(x = x.train, y = y.train, n_models = 5,
#'                   model_type = c("Linear", "Logistic")[1], include_intercept = TRUE, 
#'                   split_grid = c(2, 3), size_grid = c(10, 15), 
#'                   max_iter = 20,
#'                   cycling_iter = 0,
#'                   n_folds = 5,
#'                   n_threads = 1)
#' psgd.coef <- coef(output, group_index = 1:output$n_models)
#' psgd.predictions <- predict(output, newx = x.test, group_index = 1:output$n_models)
#' mean((y.test - psgd.predictions)^2)/sigma.epsilon^2
#' 
cv.PSGD <- function(x, y, n_models,
                    model_type = c("Linear", "Logistic")[1], include_intercept = TRUE, 
                    split_grid, size_grid, 
                    max_iter = 1e2,
                    cycling_iter = 5,
                    n_folds = 5,
                    n_threads = 1){
  
  # Check function input
  Data_Check_CV(x, y, n_models,
                model_type, include_intercept, 
                split_grid, size_grid, 
                max_iter,
                cycling_iter,
                n_folds,
                n_threads)
  
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
  output <- Main_CV_PSGD(x = x.permutation, y = y.permutation, n_models = n_models,
                         model_type = model_type.cpp, include_intercept = include_intercept.cpp, 
                         split_grid = split_grid, size_grid = size_grid, 
                         max_iter = max_iter,
                         cycling_iter = cycling_iter,
                         n_folds = n_folds,
                         n_threads = n_threads)
  
  # Create the object of class "PSGD"
  output$n_models <- n_models
  output$model_type <- model_type
  class(output) <- append("cv.PSGD", class(output))
  
  # Returning the output from the PSGD algorithm
  return(output)
}



