## -----------------------------------------------------------------------------
# 在构建 Vignette 时的默认设置
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=7, fig.height=5
)

## ----eval=FALSE---------------------------------------------------------------
#  simulate_data <- function(n=100, beta=c(1,2), sigma=1) {
#    p <- length(beta)
#    X <- cbind(1, matrix(rnorm((p-1)*n), n, p-1))
#    y <- X %*% beta + rnorm(n, 0, sigma)
#    list(X = X, y = c(y), true_beta = beta, sigma = sigma)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  NumericMatrix mcmc_linreg(const NumericVector &y,
#                             const NumericMatrix &X,
#                             const NumericVector &beta_init,
#                             double sigma2,
#                             double tau2,
#                             int n_iter,
#                             double proposal_sd) {
#  
#     int n = y.size();
#     int p = X.ncol();
#  
#     NumericMatrix beta_samples(n_iter, p);
#     NumericVector beta_current = clone(beta_init);
#  
#     auto log_posterior = [&](const NumericVector &beta) {
#       NumericVector y_hat(n);
#       for (int i = 0; i < n; i++) {
#         double tmp = 0.0;
#         for (int j = 0; j < p; j++) {
#           tmp += X(i,j)*beta[j];
#         }
#         y_hat[i] = tmp;
#       }
#  
#       double rss = 0.0;
#       for (int i = 0; i < n; i++) {
#         double diff = y[i] - y_hat[i];
#         rss += diff*diff;
#       }
#       double logLik = -0.5*n*std::log(2*M_PI*sigma2) - 0.5*(rss/sigma2);
#  
#       double beta_norm2 = 0.0;
#       for (int j = 0; j < p; j++) {
#         beta_norm2 += beta[j]*beta[j];
#       }
#       double logPrior = -0.5*p*std::log(2*M_PI*tau2) - 0.5*(beta_norm2/tau2);
#  
#       return logLik + logPrior;
#     };
#  
#     double log_post_current = log_posterior(beta_current);
#  
#     for (int iter = 0; iter < n_iter; iter++) {
#       NumericVector beta_proposal(p);
#       for (int j = 0; j < p; j++) {
#         beta_proposal[j] = R::rnorm(beta_current[j], proposal_sd);
#       }
#  
#       double log_post_proposal = log_posterior(beta_proposal);
#       double log_accept_ratio = log_post_proposal - log_post_current;
#  
#       if (std::log(R::runif(0,1)) < log_accept_ratio) {
#         beta_current = beta_proposal;
#         log_post_current = log_post_proposal;
#       }
#  
#       for (int j = 0; j < p; j++) {
#         beta_samples(iter, j) = beta_current[j];
#       }
#     }
#  
#     return beta_samples;
#   }

## ----eval=FALSE---------------------------------------------------------------
#  summarize_posterior <- function(y, X, beta_init, sigma2, tau2, n_iter, proposal_sd, cred_mass=0.95) {
#    # Perform MCMC sampling
#    beta_samples <- mcmc_linreg(y, X, beta_init, sigma2, tau2, n_iter, proposal_sd)
#  
#    # Summarize posterior samples
#    p <- ncol(beta_samples)
#    summary_list <- vector("list", p)
#    for (j in 1:p) {
#      mean_val <- mean(beta_samples[,j])
#      ci <- quantile(beta_samples[,j], probs = c((1-cred_mass)/2, 1-(1-cred_mass)/2))
#      summary_list[[j]] <- list(mean=mean_val, CI=ci)
#    }
#    summary_list
#  }

## -----------------------------------------------------------------------------
library(SA24204126)

## -----------------------------------------------------------------------------
# 示例代码（eval=TRUE以实际执行并展示结果）
if (!requireNamespace("SA24204126", quietly = TRUE)) {
  stop("Package 'SA24204126' is required but is not installed. Please install it first.")
}
library(SA24204126)

# 设置随机种子以便结果可重复
set.seed(123)

# 使用 simulate_data() 生成模拟数据
data_sim <- simulate_data(n=200, beta=c(1,2), sigma=1)
X <- data_sim$X
y <- data_sim$y

# 设置 MCMC 参数
n_iter <- 5000
sigma2 <- data_sim$sigma^2
tau2 <- 10      # 先验方差
beta_init <- rep(0, ncol(X))
proposal_sd <- 0.1

# 使用 mcmc_linreg() 抽取后验样本并 summarize_posterior() 进行结果汇总
res_summary <- summarize_posterior(y, X, beta_init, sigma2, tau2, n_iter, proposal_sd)
res_summary

## -----------------------------------------------------------------------------
# 从res_summary中提取信息
mean_val <- res_summary[[1]]$mean
ci_lower <- res_summary[[1]]$CI[1]
ci_upper <- res_summary[[1]]$CI[2]

# 创建数据框
res_df <- data.frame(
  Parameter = "Beta_1",
  Mean = mean_val,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

# 使用knitr::kable展示表格
knitr::kable(res_df, caption = "后验结果汇总表")

## -----------------------------------------------------------------------------
# 获取后验样本矩阵（n_iter x p）
beta_samples <- mcmc_linreg(y, X, beta_init, sigma2, tau2, n_iter, proposal_sd)

par(mfrow=c(1,2))
hist(beta_samples[,1], main="Posterior of Beta_1", xlab="Value", 
     col="lightblue", border="white", freq=FALSE)
abline(v=1, col="red", lwd=2, lty=2)  # 真值1

hist(beta_samples[,2], main="Posterior of Beta_2", xlab="Value", 
     col="lightblue", border="white", freq=FALSE)
abline(v=2, col="red", lwd=2, lty=2)  # 真值2

