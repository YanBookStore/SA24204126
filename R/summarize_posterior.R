#' Summarize Posterior Samples with Internal MCMC Sampling
#'
#' This function performs MCMC sampling for a linear regression model
#' and summarizes the posterior samples by computing the mean and credible intervals.
#' @importFrom stats quantile
#' @param y Response vector.
#' @param X Design matrix (with intercept column if needed).
#' @param beta_init Initial values for the regression coefficients.
#' @param sigma2 Known variance of the residuals.
#' @param tau2 Prior variance for the regression coefficients.
#' @param n_iter Number of MCMC iterations.
#' @param proposal_sd Proposal standard deviation for MCMC.
#' @param cred_mass The mass of the credible interval, default is 0.95.
#'
#' @return A list of length p (number of coefficients), each containing:
#' \describe{
#'   \item{mean}{Posterior mean of the coefficient.}
#'   \item{CI}{A vector of length 2 giving the lower and upper credible interval bounds.}
#' }
#' @export
summarize_posterior <- function(y, X, beta_init, sigma2, tau2, n_iter, proposal_sd, cred_mass=0.95) {
  # Perform MCMC sampling
  beta_samples <- mcmc_linreg(y, X, beta_init, sigma2, tau2, n_iter, proposal_sd)
  
  # Summarize posterior samples
  p <- ncol(beta_samples)
  summary_list <- vector("list", p)
  for (j in 1:p) {
    mean_val <- mean(beta_samples[,j])
    ci <- quantile(beta_samples[,j], probs = c((1-cred_mass)/2, 1-(1-cred_mass)/2))
    summary_list[[j]] <- list(mean=mean_val, CI=ci)
  }
  summary_list
}
