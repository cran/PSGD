#' 
#' @title Predictions for PSGD Object
#' 
#' @description \code{predict.PSGD} returns the predictions for a PSGD object.
#' 
#' @method predict PSGD
#' 
#' @param object An object of class PSGD
#' @param newx New data for predictions.
#' @param group_index Groups included in the ensemble. Default setting includes all the groups.
#' @param ... Additional arguments for compatibility.
#' 
#' @return The predictions for the PSGD object.
#' 
#' @export
#' 
#' @author Anthony-Alexander Christidis, \email{anthony.christidis@stat.ubc.ca}
#' 
#' @seealso \code{\link{PSGD}}
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
predict.PSGD <- function(object, newx, group_index = NULL, ...){
  
  ensemble.coef <- coef(object, group_index = group_index)
  if(object$model_type=="Linear")
    output <- ensemble.coef[1] + as.numeric(newx %*% ensemble.coef[-1]) else if(object$model_type=="Logistic")
      output <- round(exp(ensemble.coef[1] + as.numeric(newx %*% ensemble.coef[-1]))/(1 + exp(ensemble.coef[1] + as.numeric(newx %*% ensemble.coef[-1]))),0)
  return(output)
}
#' 
#' @title Predictions for cv.PSGD Object
#' 
#' @description \code{predict.cv.PSGD} returns the predictions for a cv.PSGD object.
#' 
#' @method predict cv.PSGD
#' 
#' @param object An object of class cv.PSGD
#' @param newx New data for predictions.
#' @param group_index Groups included in the ensemble. Default setting includes all the groups.
#' @param ... Additional arguments for compatibility.
#' 
#' @return The predictions for the cv.PSGD object.
#' 
#' @export
#' 
#' @author Anthony-Alexander Christidis, \email{anthony.christidis@stat.ubc.ca}
#' 
#' @seealso \code{\link{cv.PSGD}}
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
predict.cv.PSGD <- function(object, newx, group_index = group_index, ...){
  
  ensemble.coef <- coef(object, group_index = group_index)
  if(object$model_type=="Linear")
    output <- ensemble.coef[1] + as.numeric(newx %*% ensemble.coef[-1]) else if(object$model_type=="Logistic")
      output <- round(exp(ensemble.coef[1] + as.numeric(newx %*% ensemble.coef[-1]))/(1 + exp(ensemble.coef[1] + as.numeric(newx %*% ensemble.coef[-1]))),0)
    return(output)
}

