# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

Main_CV_PSGD <- function(x, y, n_models, model_type, include_intercept, split_grid, size_grid, max_iter, cycling_iter, n_folds, n_threads) {
    .Call('_PSGD_Main_CV_PSGD', PACKAGE = 'PSGD', x, y, n_models, model_type, include_intercept, split_grid, size_grid, max_iter, cycling_iter, n_folds, n_threads)
}

Main_PSGD <- function(x, y, n_models, model_type, include_intercept, split, size, max_iter, cycling_iter) {
    .Call('_PSGD_Main_PSGD', PACKAGE = 'PSGD', x, y, n_models, model_type, include_intercept, split, size, max_iter, cycling_iter)
}

Stepwise_Split <- function(x, y, n_models, max_variables_per_model, model_criterion, stop_criterion, stop_parameter) {
    .Call('_PSGD_Stepwise_Split', PACKAGE = 'PSGD', x, y, n_models, max_variables_per_model, model_criterion, stop_criterion, stop_parameter)
}

