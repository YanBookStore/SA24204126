#include <Rcpp.h>
using namespace Rcpp;

 //' @title MCMC for Bayesian Linear Regression
 //' @description This function implements a Metropolis-Hastings MCMC sampler for a Bayesian linear regression model.
 //' @param y A numeric vector of responses.
 //' @param X A numeric matrix of predictors (n x p).
 //' @param beta_init Initial values for the regression coefficients.
 //' @param sigma2 Known error variance.
 //' @param tau2 Prior variance for the coefficients.
 //' @param n_iter Number of MCMC iterations.
 //' @param proposal_sd Standard deviation of the proposal distribution for Metropolis steps.
 //' @return A matrix of size n_iter x p containing posterior samples of the coefficients.
 //' @export
 // [[Rcpp::export]]
 NumericMatrix mcmc_linreg(const NumericVector &y,
                           const NumericMatrix &X,
                           const NumericVector &beta_init,
                           double sigma2,
                           double tau2,
                           int n_iter,
                           double proposal_sd) {
   
   int n = y.size();
   int p = X.ncol();
   
   NumericMatrix beta_samples(n_iter, p);
   NumericVector beta_current = clone(beta_init);
   
   auto log_posterior = [&](const NumericVector &beta) {
     NumericVector y_hat(n);
     for (int i = 0; i < n; i++) {
       double tmp = 0.0;
       for (int j = 0; j < p; j++) {
         tmp += X(i,j)*beta[j];
       }
       y_hat[i] = tmp;
     }
     
     double rss = 0.0;
     for (int i = 0; i < n; i++) {
       double diff = y[i] - y_hat[i];
       rss += diff*diff;
     }
     double logLik = -0.5*n*std::log(2*M_PI*sigma2) - 0.5*(rss/sigma2);
     
     double beta_norm2 = 0.0;
     for (int j = 0; j < p; j++) {
       beta_norm2 += beta[j]*beta[j];
     }
     double logPrior = -0.5*p*std::log(2*M_PI*tau2) - 0.5*(beta_norm2/tau2);
     
     return logLik + logPrior;
   };
   
   double log_post_current = log_posterior(beta_current);
   
   for (int iter = 0; iter < n_iter; iter++) {
     NumericVector beta_proposal(p);
     for (int j = 0; j < p; j++) {
       beta_proposal[j] = R::rnorm(beta_current[j], proposal_sd);
     }
     
     double log_post_proposal = log_posterior(beta_proposal);
     double log_accept_ratio = log_post_proposal - log_post_current;
     
     if (std::log(R::runif(0,1)) < log_accept_ratio) {
       beta_current = beta_proposal;
       log_post_current = log_post_proposal;
     }
     
     for (int j = 0; j < p; j++) {
       beta_samples(iter, j) = beta_current[j];
     }
   }
   
   return beta_samples;
 }
 