#' MCMC for Bayesian Linear Regression
#'
#' This function is implemented in C++ and exposed to R via Rcpp. It performs MCMC sampling 
#' for Bayesian linear regression using a Metropolis-Hastings algorithm.
#'
#' @param y A numeric vector of responses.
#' @param X A numeric matrix of predictors (n x p).
#' @param beta_init Initial values for the regression coefficients.
#' @param sigma2 Known error variance.
#' @param tau2 Prior variance for the coefficients.
#' @param n_iter Number of MCMC iterations.
#' @param proposal_sd Standard deviation of the proposal distribution for Metropolis steps.
#' 
#' @return A matrix of size n_iter x p containing posterior samples of the coefficients.
#' @export
mcmc_linreg <- function(y, X, beta_init, sigma2, tau2, n_iter, proposal_sd) {
  # 注意，这里不要真正写实现，而是使用自动生成的接口
  # 实际实现是在 C++ 中，通过 RcppExports.R 中的 .Call('_SA24204126_mcmc_linreg', ...)
  .Call('_SA24204126_mcmc_linreg', PACKAGE = 'SA24204126', y, X, beta_init, sigma2, tau2, n_iter, proposal_sd)
}
