#' Simulate linear regression data
#'
#' This function generates simulated data from a linear regression model with specified parameters.
#' @importFrom stats rnorm
#' @param n Sample size.
#' @param beta A numeric vector of regression coefficients.
#' @param sigma The standard deviation of the error terms.
#'
#' @return A list containing:
#' \describe{
#'   \item{X}{Design matrix (n x p).}
#'   \item{y}{Response vector (length n).}
#'   \item{true_beta}{True coefficient vector.}
#'   \item{sigma}{Standard deviation of the noise.}
#' }
#' @examples
#' data_sim <- simulate_data(n=100, beta=c(1,2), sigma=1)
#' @export
simulate_data <- function(n=100, beta=c(1,2), sigma=1) {
  p <- length(beta)
  X <- cbind(1, matrix(rnorm((p-1)*n), n, p-1))
  y <- X %*% beta + rnorm(n, 0, sigma)
  list(X = X, y = c(y), true_beta = beta, sigma = sigma)
}
