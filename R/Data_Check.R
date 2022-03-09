# --------------------------------------
# Checking Input Data for PSGD Function
# --------------------------------------
Data_Check <- function(x, y, n_models,
                       model_type, include_intercept, 
                       split, size, 
                       max_iter,
                       cycling_iter){
  
  # Checking the input for the design matrix (x) and the response vector (y)
  if (all(!inherits(x, "matrix"), !inherits(x, "data.frame"))) {
    stop("x should belong to one of the following classes: matrix, data.frame")
  } else if (all(!inherits(y, "matrix"), all(!inherits(y, "numeric")))) {
    stop("y should belong to one of the following classes: matrix, numeric")
  } else if (any(anyNA(x), any(is.nan(x)), any(is.infinite(x)))) {
    stop("x should not have missing, infinite or nan values")
  } else if (any(anyNA(y), any(is.nan(y)), any(is.infinite(y)))) {
    stop("y should not have missing, infinite or nan values")
  } else {
    if(inherits(y, "matrix")) {
      if (ncol(y)>1){
        stop("y should be a vector")
      }
      # Force to vector if input was a matrix
      y <- as.numeric(y)
    }
    len_y <- length(y)
    if (len_y != nrow(x)) {
      stop("y and x should have the same number of rows")
    }
  }
  
  # Checking the input for the number of models
  if(!is.null(n_models)){
    if (!inherits(n_models, "numeric")) {
      stop("n_models should be numeric")
    } else if (any(!n_models == floor(n_models), n_models <= 1)) {
      stop("n_models should be an integer, greater than one")
    }
  }
  
  # Check shrinkage parameter
  if(!(include_intercept %in% c(TRUE, FALSE)))
    stop("include_intercept should be TRUE or FALSE.")
  
  # Checking split parameter
  if(!inherits(split, "numeric")) {
    stop("split should be numeric.")
  } else if(any(!split == floor(split), split <=0, split > n_models)) {
    stop("split should be a positive integer no larger than n_models.")
  }
  
  # Checking size parameter
  if(!inherits(size, "numeric")) {
    stop("size should be numeric.")
  } else if(any(!size == floor(size), size <=0, size >= min(nrow(x), ncol(x)))) {
    stop("size should be a positive integer less than the sample size and the number of predictors.")
  }
  
  # Check maximum number of iterations
  if(!inherits(max_iter, "numeric")) {
    stop("max_iter should be numeric.")
  } else if(any(!max_iter == floor(max_iter), max_iter <= 0)) {
    stop("max_iter should be a positive integer.")
  }
    
  # Check cycling iterations
  if(!inherits(cycling_iter, "numeric")) {
    stop("cycling_iter should be numeric.")
  } else if(any(!cycling_iter == floor(cycling_iter), cycling_iter < 0)) {
    stop("cycling_iter should be a positive integer.")
  }
}

# -----------------------------------------
# Checking Input Data for cv.PSGD Function
# -----------------------------------------
Data_Check_CV <- function(x, y, n_models,
                          model_type, include_intercept, 
                          split_grid, size_grid, 
                          max_iter,
                          cycling_iter,
                          n_folds,
                          n_threads){
  
  # Checking the input for the design matrix (x) and the response vector (y)
  if (all(!inherits(x, "matrix"), !inherits(x, "data.frame"))) {
    stop("x should belong to one of the following classes: matrix, data.frame")
  } else if (all(!inherits(y, "matrix"), all(!inherits(y, "numeric")))) {
    stop("y should belong to one of the following classes: matrix, numeric")
  } else if (any(anyNA(x), any(is.nan(x)), any(is.infinite(x)))) {
    stop("x should not have missing, infinite or nan values")
  } else if (any(anyNA(y), any(is.nan(y)), any(is.infinite(y)))) {
    stop("y should not have missing, infinite or nan values")
  } else {
    if(inherits(y, "matrix")) {
      if (ncol(y)>1){
        stop("y should be a vector")
      }
      # Force to vector if input was a matrix
      y <- as.numeric(y)
    }
    len_y <- length(y)
    if (len_y != nrow(x)) {
      stop("y and x should have the same number of rows")
    }
  }
  
  # Checking the input for the number of models
  if(!is.null(n_models)){
    if (!inherits(n_models, "numeric")) {
      stop("n_models should be numeric")
    } else if (any(!n_models == floor(n_models), n_models <= 1)) {
      stop("n_models should be an integer, greater than one")
    }
  }
  
  # Check shrinkage parameter
  if(!(include_intercept %in% c(TRUE, FALSE)))
    stop("include_intercept should be TRUE or FALSE.")
  
  # Checking split_grid parameter
  if(!inherits(split_grid, "numeric")) {
    stop("split_grid should be numeric.")
  } else if(any(!split_grid == floor(split_grid), split_grid <=0, split_grid > n_models)) {
    stop("split_grid should be a positive integer no larger than n_models.")
  }
  
  # Checking size_grid parameter
  if(!inherits(size_grid, "numeric")) {
    stop("size_grid should be numeric.")
  } else if(any(!size_grid == floor(size_grid), size_grid <=0, size_grid >= min(nrow(x), ncol(x)))) {
    stop("size_grid should be a positive integer less than the sample size_grid and the number of predictors.")
  }
  
  # Check maximum number of iterations
  if(!inherits(max_iter, "numeric")) {
    stop("max_iter should be numeric.")
  } else if(any(!max_iter == floor(max_iter), max_iter <= 0)) {
    stop("max_iter should be a positive integer.")
  }
  
  # Check cycling iterations
  if(!inherits(cycling_iter, "numeric")) {
    stop("cycling_iter should be numeric.")
  } else if(any(!cycling_iter == floor(cycling_iter), cycling_iter < 0)) {
    stop("cycling_iter should be a positive integer.")
  }
  
  # Check input for number of folds
  if(!inherits(n_folds, "numeric")) {
    stop("n_folds should be numeric")
  } else if(any(!n_folds == floor(n_folds), n_folds <= 0)) {
    stop("n_folds should be a positive integer")
  }
  
  # Check input for number of threads
  if(!inherits(n_threads, "numeric")) {
    stop("n_threads should be numeric")
  } else if(any(!n_threads == floor(n_threads), n_threads <= 0)) {
    stop("n_threads should be a positive integer")
  }
}
