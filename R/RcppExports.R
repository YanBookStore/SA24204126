# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

mcmc_linreg <- function(y, X, beta_init, sigma2, tau2, n_iter, proposal_sd) {
    .Call('_SA24204126_mcmc_linreg', PACKAGE = 'SA24204126', y, X, beta_init, sigma2, tau2, n_iter, proposal_sd)
}

