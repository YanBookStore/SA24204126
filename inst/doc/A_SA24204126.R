## ----echo=TRUE, fig.height=4, fig.width=6-------------------------------------
# 生成正态分布数据
set.seed(123)
x <- rnorm(100)
y <- 2 * x + rnorm(100)
plot(x, y, main="Scatter Plot of Normally Distributed Data", xlab="x", ylab="y", col="blue", pch=19)

# 添加图例
legend("topleft", legend = "Data Points", col = "blue", pch = 19)

# 在图中添加数学公式
text(x = 0, y = max(y) - 1, labels = expression(y == beta[0] + beta[1] * x + epsilon), cex = 1.2)


## -----------------------------------------------------------------------------

# 创建数据框
students <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "David", "Eva"),
  Math = c(95, 78, 88, 92, 85),
  English = c(80, 85, 82, 88, 90),
  Physics = c(89, 90, 78, 95, 85),
  Chemistry = c(92, 88, 91, 84, 86),
  History = c(88, 94, 85, 90, 82)
)

# 显示数据框为表格
knitr::kable(students, caption = "学生考试成绩")

# 计算每个科目的均值、方差和标准差
stats <- data.frame(
  Subject = c("Math", "English", "Physics", "Chemistry", "History"),
  Mean = sapply(students[,-1], mean),
  Variance = sapply(students[,-1], var),
  SD = sapply(students[,-1], sd)
)

# 显示描述性统计量表格
knitr::kable(stats, caption = "各科目描述性统计量")

# 计算置信区间
confidence_intervals <- data.frame(
  Subject = stats$Subject,
  Lower = stats$Mean - 1.96 * stats$SD / sqrt(nrow(students)),
  Upper = stats$Mean + 1.96 * stats$SD / sqrt(nrow(students))
)

# 显示置信区间表格
knitr::kable(confidence_intervals, caption = "各科目均值的 95% 置信区间")


## -----------------------------------------------------------------------------
# 生成数据
x <- seq(-10, 10, length.out = 100)
y1 <- dnorm(x, mean = 0, sd = 1)
y2 <- dnorm(x, mean = 1, sd = 2)

# 绘制正态分布曲线
plot(x, y1, type = "l", col = "blue", lwd = 2, main = "Normal Distributions with Different Standard Deviations")
lines(x, y2, col = "red", lwd = 2)
legend("topright", legend = c("mean = 0,SD = 1", "mean = 1,SD = 2"), col = c("blue", "red"), lwd = 2)


## -----------------------------------------------------------------------------
# 设定参数
set.seed(123)  # 设置随机数种子
n <- 10000     # 样本数量
sigma_values <- c(1, 2, 3)  # 不同的sigma值

# 定义Rayleigh分布的随机样本生成函数
rayleigh_sample <- function(sigma, n) {
  u <- runif(n)  # 生成n个均匀分布的随机数
  return(sigma * sqrt(-2 * log(u)))
}

# Rayleigh分布理论密度函数
rayleigh_density <- function(x, sigma) {
  (x / sigma^2) * exp(-x^2 / (2 * sigma^2))
}

# 生成样本并绘制直方图与理论密度函数比较
par(mfrow=c(1,3))  # 设置多图布局

# 设置不同sigma对应的y轴限制
y_limits <- list(c(0, 0.8), c(0, 0.4), c(0, 0.3))

for (i in 1:length(sigma_values)) {
  sigma <- sigma_values[i]
  samples <- rayleigh_sample(sigma, n)  # 生成Rayleigh分布样本
  
  # 绘制直方图
  hist(samples, breaks=50, probability=TRUE, main=paste("sigma =", sigma),
       xlab="样本值", col="lightblue", border="black", ylim=y_limits[[i]])
  
  # 生成x轴上的点用于绘制理论密度函数
  x_vals <- seq(0, max(samples), length=1000)
  
  # 绘制理论密度函数曲线
  lines(x_vals, rayleigh_density(x_vals, sigma), col="red", lwd=2)
  
  # 添加理论众数sigma的垂直线
  abline(v=sigma, col="blue", lwd=2, lty=2)  
  
  # 添加生成样本的实际众数
  actual_mode <- density(samples)$x[which.max(density(samples)$y)]
  abline(v=actual_mode, col="green", lwd=2, lty=3)
  
  # 添加图例
  legend("topright", legend=c("理论密度函数", "理论众数", "样本众数"),
         col=c("red", "blue", "green"), lwd=2, lty=c(1, 2, 3), cex=0.8)
}

## ----fig.width=7, fig.height=5------------------------------------------------

set.seed(123)  # 保证可重复性

# 样本大小
n <- 1000

# 混合分布的生成函数
generate_mixture <- function(p1, n) {
  # 生成混合分布
  r <- rbinom(n, 1, p1)
  # r 为1时从 N(0,1) 生成，否则从 N(3,1) 生成
  samples <- rnorm(n, mean = ifelse(r == 1, 0, 3), sd = 1)
  return(samples)
}

# 设置混合概率 p1 = 0.75
p1 <- 0.75
samples <- generate_mixture(p1, n)

# 基础R绘图 - 绘制直方图并叠加密度图
hist(samples, probability = TRUE, breaks = 30, 
     main = paste("p1 =", p1, "时的样本分布"), xlab = "样本值", col = "lightblue")

# 添加密度曲线
lines(density(samples), col = "red", lwd = 2)
# 设置不同的 p1 值
p_values <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# 绘制不同 p1 值下的直方图
par(mfrow = c(3, 2))  # 设置图形布局

for (p1 in p_values) {
  samples <- generate_mixture(p1, n)
  
  hist(samples, probability = TRUE, breaks = 30, 
       main = paste("p1 =", p1), xlab = "样本值", col = "lightblue")
  
  lines(density(samples), col = "red", lwd = 2)
}
par(mfrow = c(1, 1))  # 恢复单一图形布局



## ----warning=FALSE------------------------------------------------------------
# 加载必要的库
set.seed(123)
lambda <- 2  # 泊松过程的参数
t <- 10      # 时间点 t
alpha <- 3   # 伽马分布参数
beta <- 2    # 伽马分布参数

# 模拟复合泊松-伽马过程
simulate_compound_poisson_gamma <- function(lambda, alpha, beta, t) {
  N_t <- rpois(1, lambda * t)  # 泊松过程模拟 N(t)
  Y <- rgamma(N_t, shape = alpha, rate = beta)  # 伽马分布模拟 Y_i
  X_t <- sum(Y)  # 复合泊松过程的和
  return(X_t)
}

# 进行多次模拟，计算均值和方差
n_simulations <- 10000
results <- replicate(n_simulations, simulate_compound_poisson_gamma(lambda, alpha, beta, t))

# 估计均值和方差
estimated_mean <- mean(results)
estimated_variance <- var(results)

# 理论均值和方差
theoretical_mean <- lambda * t * (alpha / beta)
theoretical_variance <- lambda * t * (alpha / beta^2 + (alpha / beta)^2)

# 显示结果
estimated_mean
estimated_variance
theoretical_mean
theoretical_variance

## -----------------------------------------------------------------------------

Monte_Carlo_Beta33 <- function(x=0.1,n=1e6){
  y <- runif(n,min = 0, max = x)
  result <- 30 * mean(x * y*y * (1-y)^2)
  return(result)
}

for (x in seq(0.1, 0.9, by = 0.1)) {
  print(paste("x=",x,"Monte Carlo method:", round(Monte_Carlo_Beta33(x),digits = 5) , "pbeta function:" , pbeta(x, shape1 = 3, shape2 = 3)))
}


## -----------------------------------------------------------------------------
# 从 Rayleigh 分布中生成样本
rayleigh_sample <- function(sigma, n) {
  u <- runif(n)
  return(sigma * sqrt(-2 * log(u)))
}

## -----------------------------------------------------------------------------
rayleigh_antithetic <- function(sigma, n) {
  u <- runif(n/2)
  x1 <- sigma * sqrt(-2 * log(u))
  x2 <- sigma * sqrt(-2 * log(1 - u))
  return(c(x1, x2))
}

## -----------------------------------------------------------------------------
# 参数设置
sigma <- 1
n <- 10000

# 独立样本
x1 <- rayleigh_sample(sigma, n)
x2 <- rayleigh_sample(sigma, n)
independent_mean <- (x1 + x2) / 2

# 对偶变量
x_antithetic <- rayleigh_antithetic(sigma, n)
antithetic_mean <- (x_antithetic[1:(n/2)] + x_antithetic[(n/2 + 1):n]) / 2

# 计算方差
var_independent <- var(independent_mean)
var_antithetic <- var(antithetic_mean)

# 计算方差减少百分比
percent_reduction <- (var_independent - var_antithetic) / var_independent * 100

cat("独立样本的方差:", var_independent, "\n")
cat("对偶变量的方差:", var_antithetic, "\n")
cat("方差减少百分比:", percent_reduction, "%\n")

## -----------------------------------------------------------------------------
# 打印结果
percent_reduction

## -----------------------------------------------------------------------------
set.seed(123)

# 目标函数 g(x)
g <- function(x) {
  return((x^2 / sqrt(2 * pi)) * exp(-x^2 / 2))
}

# 重要性函数 f1(x): 截断的正态分布
f1_density <- function(x) {
  return(dnorm(x) / (1 - pnorm(1)))
}

# 重要性函数 f2(x): 幂分布
f2_density <- function(x) {
  return(2 / x^3)
}

# 样本大小
N <- 10000

# 使用 f1 进行重要性采样
f1_samples <- rnorm(N)
f1_samples <- f1_samples[f1_samples > 1]  # 只取大于1的部分
weights_f1 <- g(f1_samples) / f1_density(f1_samples)
I_f1 <- mean(weights_f1)
var_f1 <- var(weights_f1)

# 使用 f2 进行重要性采样
f2_samples <- (1 + rexp(N, 1))  # 对于幂分布的采样
weights_f2 <- g(f2_samples) / f2_density(f2_samples)
I_f2 <- mean(weights_f2)
var_f2 <- var(weights_f2)

# 输出结果
cat("f1 估计值: ", I_f1, "\n")
cat("f1 方差: ", var_f1, "\n")
cat("f2 估计值: ", I_f2, "\n")
cat("f2 方差: ", var_f2, "\n")


## -----------------------------------------------------------------------------

# 设置n的值
n_values <- c(10^4, 2*10^4, 4*10^4, 6*10^4, 8*10^4)

# 创建存储平均时间的向量
mean_times <- numeric(length(n_values))

# 对每个n进行实验
for (i in 1:length(n_values)) {
  n <- n_values[i]
  times <- numeric(100)  # 存储每次的时间
  
  # 进行100次实验
  for (j in 1:100) {
    vec <- sample(1:n)  # 生成1到n的随机排列
    time_taken <- system.time(sort(vec))['elapsed']  # 测量排序的时间
    times[j] <- time_taken
  }
  
  # 计算平均时间
  mean_times[i] <- mean(times)
}

# 计算 n*log(n)
t_n <- n_values * log(n_values)

# 进行线性回归
regression_model <- lm(mean_times ~ t_n)

# 显示回归结果
summary(regression_model)

# 绘制散点图和回归线
plot(t_n, mean_times, main = "Sorting Time vs n log(n)", xlab = "n log(n)", ylab = "Mean Time (s)")
abline(regression_model, col="red")  # 添加回归线



## -----------------------------------------------------------------------------
# 加载所需库
library(moments)  # 用于计算偏度

# 定义模拟参数
set.seed(123)
n <- 100  # 样本量
num_simulations <- 10000  # 蒙特卡罗模拟次数
quantiles <- c(0.025, 0.05, 0.95, 0.975)

# 存储模拟结果
skewness_sqrt_b1 <- numeric(num_simulations)

# 蒙特卡罗模拟
for (i in 1:num_simulations) {
  sample_data <- rnorm(n)  # 生成正态分布样本
  b1 <- skewness(sample_data)  # 计算偏度
  
  # 对负的偏度取绝对值
  skewness_sqrt_b1[i] <- sqrt(abs(b1))  # 保存偏度的绝对值的平方根
}

# 计算分位数
estimated_quantiles <- quantile(skewness_sqrt_b1, quantiles, na.rm = TRUE)
print(estimated_quantiles)

# 计算标准误差 (公式2.14)
# 正态分布的密度函数在每个分位数的值
density_values <- dnorm(qnorm(quantiles))
n_sample <- length(skewness_sqrt_b1)

# 公式(2.14)
standard_errors <- sqrt(quantiles * (1 - quantiles) / (n_sample * density_values^2))
print(standard_errors)

# 大样本近似
large_sample_approx <- sqrt(6/n) * qnorm(quantiles)
print(large_sample_approx)

# 输出比较结果
results <- data.frame(
  Quantile = quantiles,
  Estimated = estimated_quantiles,
  Standard_Error = standard_errors,
  Large_Sample_Approx = large_sample_approx
)

print(results)

## -----------------------------------------------------------------------------
# 加载所需的库
set.seed(123)  # 固定随机种子
library(bootstrap)
library(coda)
library(MASS)  # 用于生成双变量正态分布
library(coin)  # 用于计算功效
library(survival)
# 生成双变量正态分布数据
n <- 100  # 样本大小
mu <- c(0, 0)  # 均值向量
Sigma <- matrix(c(1, 0.8, 0.8, 1), 2, 2)  # 协方差矩阵，相关系数为 0.8
bivariate_normal <- mvrnorm(n, mu, Sigma)

X <- bivariate_normal[, 1]
Y <- bivariate_normal[, 2]

# 皮尔逊相关检验
pearson_test <- cor.test(X, Y, method = "pearson")

# 斯皮尔曼相关检验
spearman_test <- cor.test(X, Y, method = "spearman")

# 肯德尔相关检验
kendall_test <- cor.test(X, Y, method = "kendall")

# 输出检验结果
pearson_test$p.value
spearman_test$p.value
kendall_test$p.value

## -----------------------------------------------------------------------------
# 生成非线性关系的数据: Y = X^2
n <- 100  # 样本大小
X <- rnorm(n)
Y <- X^2 + rnorm(n, sd = 0.1)  # 加入一些噪声

# 皮尔逊相关检验
pearson_test_nonlin <- cor.test(X, Y, method = "pearson")

# 斯皮尔曼相关检验
spearman_test_nonlin <- cor.test(X, Y, method = "spearman")

# 肯德尔相关检验
kendall_test_nonlin <- cor.test(X, Y, method = "kendall")

# 输出非线性关系下的检验结果
pearson_test_nonlin$p.value
spearman_test_nonlin$p.value
kendall_test_nonlin$p.value

## -----------------------------------------------------------------------------
# 给定的数据
n1 <- 10000  # 方法1的样本量
n2 <- 10000  # 方法2的样本量
p1 <- 0.651  # 方法1的功效
p2 <- 0.676  # 方法2的功效

# 组合后的比例
p_combined <- (p1 * n1 + p2 * n2) / (n1 + n2)

# 标准误差
se <- sqrt(p_combined * (1 - p_combined) * (1/n1 + 1/n2))

# Z统计量
z_stat <- (p1 - p2) / se

# P值
p_value <- 2 * (1 - pnorm(abs(z_stat)))

# 输出结果
z_stat
p_value

# 基于显著性水平0.05做出决策
if (p_value < 0.05) {
  cat("拒绝原假设：两种方法的功效显著不同。")
} else {
  cat("未能拒绝原假设：两种方法的功效没有显著差异。")
}

## -----------------------------------------------------------------------------
# 设置随机种子，确保结果可重复
set.seed(123)

# 参数设置
N <- 1000       # 假设总数
m <- 10000      # 模拟次数
null_count <- 950  # 空假设的数量
alt_count <- 50    # 备择假设的数量
alpha <- 0.1    # 名义显著性水平

# 数据存储
results <- data.frame(matrix(numeric(6), nrow = 3, ncol = 2))
colnames(results) <- c("Bonferroni", "B-H")
rownames(results) <- c("FWER", "FDR", "TPR")


## -----------------------------------------------------------------------------
# 函数用于生成一个实验中的p值
generate_pvalues <- function() {
  # 生成空假设的p值
  null_pvals <- runif(null_count)
  
  # 生成备择假设的p值
  alt_pvals <- rbeta(alt_count, 0.1, 1)
  
  # 合并p值
  pvals <- c(null_pvals, alt_pvals)
  return(pvals)
}

# 测试p值生成
pvals <- generate_pvalues()
head(pvals)

## -----------------------------------------------------------------------------
# Bonferroni 校正
bonferroni_adjust <- function(pvals) {
  return(p.adjust(pvals, method = "bonferroni"))
}

# B-H 校正
bh_adjust <- function(pvals) {
  return(p.adjust(pvals, method = "BH"))
}

# 计算 FWER, FDR, 和 TPR
calculate_metrics <- function(adjusted_pvals, true_alt_indices, alpha) {
  # 检测为显著的假设
  discoveries <- which(adjusted_pvals < alpha)
  
  # 计算 FWER (至少一个假阳性的概率)
  fwer <- ifelse(any(discoveries <= null_count), 1, 0)
  
  # 计算 FDR (假阳性率)
  false_discoveries <- sum(discoveries <= null_count)
  total_discoveries <- length(discoveries)
  fdr <- ifelse(total_discoveries > 0, false_discoveries / total_discoveries, 0)
  
  # 计算 TPR (真正率)
  true_discoveries <- sum(discoveries > null_count)
  tpr <- true_discoveries / alt_count
  
  return(c(fwer, fdr, tpr))
}

## ----message=FALSE, warning=FALSE---------------------------------------------
# 存储 Bonferroni 和 B-H 校正结果的矩阵
bonferroni_results <- matrix(0, nrow = m, ncol = 3)
bh_results <- matrix(0, nrow = m, ncol = 3)

# 进行模拟
for (i in 1:m) {
  pvals <- generate_pvalues()
  
  # Bonferroni 校正
  bonf_pvals <- bonferroni_adjust(pvals)
  bonferroni_results[i, ] <- calculate_metrics(bonf_pvals, null_count, alpha)
  
  # B-H 校正
  bh_pvals <- bh_adjust(pvals)
  bh_results[i, ] <- calculate_metrics(bh_pvals, null_count, alpha)
}

# 计算平均值作为最终结果
results["FWER", "Bonferroni"] <- mean(bonferroni_results[, 1])
results["FDR", "Bonferroni"] <- mean(bonferroni_results[, 2])
results["TPR", "Bonferroni"] <- mean(bonferroni_results[, 3])

results["FWER", "B-H"] <- mean(bh_results[, 1])
results["FDR", "B-H"] <- mean(bh_results[, 2])
results["TPR", "B-H"] <- mean(bh_results[, 3])

# 输出结果表格
results

## -----------------------------------------------------------------------------
# 评论结果
print("Bonferroni 校正相对保守，因此它会较好地控制 FWER，但可能导致较低的 TPR，即真正发现的比例较低。")
print("B-H 校正更倾向于控制 FDR，允许更多的假阳性，但可能提高 TPR。")

## -----------------------------------------------------------------------------
# 空调设备故障时间数据（单位：小时）
failure_times <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
failure_times

## ----mle_lambda---------------------------------------------------------------
# 计算 lambda 的最大似然估计
lambda_mle <- 1 / mean(failure_times)
lambda_mle

## ----bootstrap----------------------------------------------------------------
# 加载必要的包
library(boot)

# 定义用于 bootstrap 的统计函数
mle_stat <- function(data, indices) {
  resample_data <- data[indices]
  1 / mean(resample_data)
}

# 使用 1000 次重采样进行自助法
set.seed(123)
bootstrap_results <- boot(data = failure_times, statistic = mle_stat, R = 1000)

# 显示自助法结果
bootstrap_results

## ----bias_se------------------------------------------------------------------
# 计算自助法得到的偏差和标准误
bias <- mean(bootstrap_results$t) - lambda_mle
standard_error <- sd(bootstrap_results$t)

bias
standard_error

## ----data---------------------------------------------------------------------
# 空调设备故障时间数据（单位：小时）
failure_times <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
failure_times

## ----mle_mean_time------------------------------------------------------------
# 计算平均故障间隔时间（即 1/lambda 的估计值）
mean_time <- mean(failure_times)
mean_time

## ----bootstrap_intervals------------------------------------------------------
# 加载必要的包
library(boot)

# 定义统计函数，返回 1/lambda
mean_time_stat <- function(data, indices) {
  resample_data <- data[indices]
  mean(resample_data)
}

# 使用 1000 次重采样进行自助法
set.seed(123)
bootstrap_results <- boot(data = failure_times, statistic = mean_time_stat, R = 1000)

# 计算标准正态、基本、百分位和 BCa 置信区间
ci_norm <- boot.ci(bootstrap_results, type = "norm")
ci_basic <- boot.ci(bootstrap_results, type = "basic")
ci_perc <- boot.ci(bootstrap_results, type = "perc")
ci_bca <- boot.ci(bootstrap_results, type = "bca")

# 展示置信区间
ci_norm
ci_basic
ci_perc
ci_bca

## ----compare_intervals--------------------------------------------------------
# 将置信区间结果提取出来并汇总
ci_summary <- data.frame(
  Method = c("Standard Normal", "Basic", "Percentile", "BCa"),
  Lower = c(ci_norm$normal[2], ci_basic$basic[4], ci_perc$perc[4], ci_bca$bca[4]),
  Upper = c(ci_norm$normal[3], ci_basic$basic[5], ci_perc$perc[5], ci_bca$bca[5])
)

ci_summary

## -----------------------------------------------------------------------------
library("bootstrap")

## -----------------------------------------------------------------------------

# 计算协方差矩阵
cov_matrix <- cov(scor)

# 计算协方差矩阵的特征值
eigenvalues <- eigen(cov_matrix)$values

# 计算 \hat{\theta} 的估计值
theta_hat <- eigenvalues[1] / sum(eigenvalues)
print(paste("Theta 的估计值:", theta_hat))

## -----------------------------------------------------------------------------
# 设置自助法重复次数
B <- 1000
theta_bootstrap <- numeric(B)

set.seed(123)  # 设置随机种子以确保可重复性

for (i in 1:B) {
  # 自助法抽样
  bootstrap_sample <- scor[sample(1:nrow(scor), replace = TRUE), ]
  
  # 计算自助样本的协方差矩阵
  bootstrap_cov <- cov(bootstrap_sample)
  
  # 计算特征值
  bootstrap_eigenvalues <- eigen(bootstrap_cov)$values
  
  # 计算 \hat{\theta} 值
  theta_bootstrap[i] <- bootstrap_eigenvalues[1] / sum(bootstrap_eigenvalues)
}

# 计算偏差和标准误差
bootstrap_bias <- mean(theta_bootstrap) - theta_hat
bootstrap_se <- sd(theta_bootstrap)

print(paste("自助法估计的偏差:", bootstrap_bias))
print(paste("自助法估计的标准误差:", bootstrap_se))

## -----------------------------------------------------------------------------
# 初始化Jackknife估计
n <- nrow(scor)
theta_jackknife <- numeric(n)

for (i in 1:n) {
  # 留出第 i 个样本
  jackknife_sample <- scor[-i, ]
  
  # 计算留一法样本的协方差矩阵
  jackknife_cov <- cov(jackknife_sample)
  
  # 计算特征值
  jackknife_eigenvalues <- eigen(jackknife_cov)$values
  
  # 计算 \hat{\theta} 值
  theta_jackknife[i] <- jackknife_eigenvalues[1] / sum(jackknife_eigenvalues)
}

# 计算偏差和标准误差
jackknife_bias <- (n - 1) * (mean(theta_jackknife) - theta_hat)
jackknife_se <- sqrt((n - 1) * mean((theta_jackknife - mean(theta_jackknife))^2))

print(paste("Jackknife法估计的偏差:", jackknife_bias))
print(paste("Jackknife法估计的标准误差:", jackknife_se))

## -----------------------------------------------------------------------------
# 加载必要的包
library(DAAG)

# 清除环境中的变量
rm(list = ls())

# 加载 ironslag 数据集
data("ironslag", package = "DAAG")
magnetic <- ironslag$magnetic
chemical <- ironslag$chemical

# 定义计算模型的预测误差的函数
cross_validation <- function(x, y) {
  n <- length(y)
  errors <- matrix(0, nrow = n, ncol = 4)
  
  for (k in 1:n) {
    # 留一法样本
    y_train <- y[-k]
    x_train <- x[-k]
    
    # 模型1：线性模型
    model1 <- lm(y_train ~ x_train)
    yhat1 <- predict(model1, newdata = data.frame(x_train = x[k]))
    errors[k, 1] <- (y[k] - yhat1)^2
    
    # 模型2：二次多项式模型
    model2 <- lm(y_train ~ x_train + I(x_train^2))
    yhat2 <- predict(model2, newdata = data.frame(x_train = x[k]))
    errors[k, 2] <- (y[k] - yhat2)^2
    
    # 模型3：三次多项式模型
    model3 <- lm(y_train ~ x_train + I(x_train^2) + I(x_train^3))
    yhat3 <- predict(model3, newdata = data.frame(x_train = x[k]))
    errors[k, 3] <- (y[k] - yhat3)^2
    
    # 模型4：对数-对数模型
    model4 <- lm(log(y_train) ~ log(x_train))
    logyhat4 <- predict(model4, newdata = data.frame(x_train = x[k]), type = "response")
    yhat4 <- exp(logyhat4)
    errors[k, 4] <- (y[k] - yhat4)^2
  }
  
  # 计算每个模型的均方误差
  mse <- colMeans(errors)
  names(mse) <- c("Linear", "Quadratic", "Cubic", "Log-Log")
  return(mse)
}

# 定义计算调整后 R^2 的函数
adjusted_r_squared <- function(x, y) {
  models <- list(
    lm(y ~ x),
    lm(y ~ x + I(x^2)),
    lm(y ~ x + I(x^2) + I(x^3)),
    lm(log(y) ~ log(x))
  )
  adj_r2 <- sapply(models, function(model) summary(model)$adj.r.squared)
  names(adj_r2) <- c("Linear", "Quadratic", "Cubic", "Log-Log")
  return(adj_r2)
}

# 运行交叉验证和计算调整后 R^2
mse_results <- cross_validation(chemical, magnetic)
adj_r2_results <- adjusted_r_squared(chemical, magnetic)

# 显示结果
print("均方误差 (MSE) 结果:")
print(mse_results)
print("调整后 R^2 结果:")
print(adj_r2_results)

# 清除内存
rm(list = ls())


## -----------------------------------------------------------------------------
# 清除环境中的变量
rm(list = ls())

# 加载数据集
data("chickwts", package = "datasets")

# 提取豆粕和亚麻籽组的数据
x <- sort(as.vector(chickwts$weight[chickwts$feed == "soybean"]))
y <- sort(as.vector(chickwts$weight[chickwts$feed == "linseed"]))

# 定义Cramér-von Mises统计量计算函数
cvm_statistic <- function(x, y) {
  n <- length(x)
  m <- length(y)
  combined <- sort(c(x, y))
  Fx <- ecdf(x)
  Fy <- ecdf(y)
  statistic <- sum((Fx(combined) - Fy(combined))^2) * (n * m) / (n + m)
  return(statistic)
}

# 置换检验函数
cvm_permutation_test <- function(x, y, num_permutations = 1000) {
  observed_stat <- cvm_statistic(x, y)
  combined <- c(x, y)
  n <- length(x)
  
  perm_stats <- numeric(num_permutations)
  for (i in 1:num_permutations) {
    permuted <- sample(combined)
    x_perm <- permuted[1:n]
    y_perm <- permuted[(n+1):length(permuted)]
    perm_stats[i] <- cvm_statistic(x_perm, y_perm)
  }
  
  # 计算p值
  p_value <- mean(perm_stats >= observed_stat)
  
  list(statistic = observed_stat, p_value = p_value)
}

# 执行置换检验
set.seed(123)  # 设置随机种子以保证可重复性
result <- cvm_permutation_test(x, y, num_permutations = 1000)

# 打印结果
print(paste("Cramér-von Mises 统计量:", result$statistic))
print(paste("p 值:", result$p_value))

# 清除内存
rm(list = ls())


## -----------------------------------------------------------------------------
# 清除环境中的变量
rm(list = ls())

# 定义计算Spearman秩相关性的函数
spearman_correlation <- function(x, y) {
  return(cor(x, y, method = "spearman"))
}

# 定义置换检验函数
spearman_permutation_test <- function(x, y, num_permutations = 1000) {
  # 计算观察到的Spearman相关性
  observed_stat <- spearman_correlation(x, y)
  
  # 初始化置换统计量
  perm_stats <- numeric(num_permutations)
  
  # 进行置换
  for (i in 1:num_permutations) {
    y_permuted <- sample(y)  # 随机置换y
    perm_stats[i] <- spearman_correlation(x, y_permuted)
  }
  
  # 计算p值
  p_value <- mean(abs(perm_stats) >= abs(observed_stat))
  
  # 返回结果
  list(statistic = observed_stat, p_value = p_value)
}

# 测试数据
set.seed(123)  # 设置随机种子保证结果可重复
x <- rnorm(30)  # 随机生成数据
y <- x + rnorm(30) * 0.5  # 添加噪声的相关数据

# 执行置换检验
result_permutation <- spearman_permutation_test(x, y, num_permutations = 1000)

# 使用 cor.test 计算 p 值
result_cor_test <- cor.test(x, y, method = "spearman")

# 显示结果
print(paste("置换检验的Spearman相关性统计量:", result_permutation$statistic))
print(paste("置换检验的p值:", result_permutation$p_value))
print(paste("cor.test的p值:", result_cor_test$p.value))

# 清除内存
rm(list = ls())


## -----------------------------------------------------------------------------
# 设置参数
theta <- 1  # 标准柯西分布的尺度参数
eta <- 0    # 标准柯西分布的位置参数
n <- 10000  # 每条链生成的样本数量
burn_in <- 1000  # 舍弃的样本数
chains <- 4  # 生成的链数

# 1. 定义目标密度函数 (标准柯西分布的概率密度函数)
dcauchy_density <- function(x, theta, eta) {
  return(1 / (theta * pi * (1 + ((x - eta) / theta)^2)))
}

# 2. Metropolis-Hastings采样函数
metropolis_hastings <- function(n, burn_in, theta, eta) {
  samples <- numeric(n)  # 预分配内存
  samples[1] <- 0  # 初始值
  
  for (i in 2:n) {
    # 提议分布: 使用正态分布提议
    proposal <- rnorm(1, mean = samples[i-1], sd = 1)
    
    # 计算接受率
    acceptance_ratio <- dcauchy_density(proposal, theta, eta) / dcauchy_density(samples[i-1], theta, eta)
    
    # 接受或拒绝
    if (runif(1) < acceptance_ratio) {
      samples[i] <- proposal
    } else {
      samples[i] <- samples[i-1]
    }
  }
  
  # 舍弃前 burn_in 个样本
  return(samples[(burn_in + 1):n])
}

# 3. 计算Gelman-Rubin统计量
gelman_rubin <- function(chains) {
  m <- length(chains)
  # 计算各链均值和方差
  means <- sapply(chains, mean)
  variances <- sapply(chains, var)
  
  # 计算整体均值和整体方差
  overall_mean <- mean(means)
  B <- sum((means - overall_mean)^2) * n / (m - 1)  # between-chain variance
  W <- mean(variances)  # within-chain variance
  var_hat <- (n - 1) / n * W + B / n  # combined variance
  
  return(sqrt(var_hat / W))  # Gelman-Rubin统计量
}

# 4. 运行链直到收敛
run_chains_until_convergence <- function(n, burn_in, theta, eta, chains) {
  chain_samples <- lapply(1:chains, function(x) metropolis_hastings(n, burn_in, theta, eta))
  hat_R <- gelman_rubin(chain_samples)
  
  while (hat_R >= 1.2) {
    cat("当前 \\hat{R}: ", hat_R, "\n")
    chain_samples <- lapply(1:chains, function(x) metropolis_hastings(n, burn_in, theta, eta))
    hat_R <- gelman_rubin(chain_samples)
  }
  
  cat("最终 \\hat{R}: ", hat_R, "\n")  # 输出最终的Gelman-Rubin统计量
  return(chain_samples)
}

# 5. 计算并比较十分位数的函数
compare_quantiles <- function(samples, theta, eta) {
  sample_quantiles <- quantile(samples, probs = seq(0.1, 0.9, by = 0.1))
  theoretical_quantiles <- qcauchy(seq(0.1, 0.9, by = 0.1), location = eta, scale = theta)
  
  cat("生成样本的十分位数：\n")
  print(sample_quantiles)
  
  cat("\n标准柯西分布的理论十分位数：\n")
  print(theoretical_quantiles)
}

# 6. 可视化函数
plot_samples <- function(samples, theta, eta) {
  hist(samples, breaks = 50, probability = TRUE, main = "Metropolis-Hastings 生成的标准柯西分布样本",
       xlab = "样本值", ylab = "密度")
  curve(dcauchy(x, location = eta, scale = theta), col = "red", lwd = 2, add = TRUE)
  legend("topright", legend = c("生成样本密度", "理论密度"), col = c("black", "red"), lwd = 2)
}

# 主程序
set.seed(42)  # 设置随机种子
chain_samples <- run_chains_until_convergence(n, burn_in, theta, eta, chains)  # 运行链直到收敛

# 计算并输出每条链的十分位数比较结果
for (i in 1:chains) {
  cat(paste("链", i, "的十分位数比较：\n"))
  compare_quantiles(chain_samples[[i]], theta, eta)
  plot_samples(chain_samples[[i]], theta, eta)
}

# 清理内存
rm(chain_samples)
gc()

## -----------------------------------------------------------------------------
# 加载必要的包
library(coda)
library(ggplot2)

# 设置参数
a <- 2  # a的值
b <- 2  # b的值
n <- 10 # n的值
num_samples <- 20000  # 增加采样数量
burn_in <- 2000       # 适当增加burn-in
chains <- 4           # 生成的链数

# 1. 定义从条件分布Binomial(n, y)采样的函数
sample_x_given_y <- function(y, n) {
  return(rbinom(1, size = n, prob = y))
}

# 2. 定义从条件分布Beta(x + a, n - x + b)采样的函数
sample_y_given_x <- function(x, a, b, n) {
  return(rbeta(1, shape1 = x + a, shape2 = n - x + b))
}

# 3. Gibbs采样函数，随机初始化每条链的x和y
gibbs_sampler <- function(num_samples, burn_in, a, b, n) {
  samples <- matrix(NA, nrow = num_samples, ncol = 2)
  x <- sample(0:n, 1)           # 随机选择初始x
  y <- runif(1, 0, 1)           # 随机选择初始y
  
  for (i in 1:num_samples) {
    x <- sample_x_given_y(y, n)
    y <- sample_y_given_x(x, a, b, n)
    samples[i, ] <- c(x, y)
  }
  
  return(samples[(burn_in + 1):num_samples, ])
}

# 4. 计算Gelman-Rubin统计量
run_chains_until_convergence <- function(num_samples, burn_in, a, b, n, chains) {
  chain_samples <- lapply(1:chains, function(x) gibbs_sampler(num_samples, burn_in, a, b, n))
  chain_samples_mcmc <- lapply(chain_samples, function(samples) as.mcmc(samples))
  
  gelman_diag_result <- gelman.diag(mcmc.list(chain_samples_mcmc))
  hat_R <- gelman_diag_result$psrf
  
  while (any(hat_R >= 1.2)) {
    cat("当前 \\hat{R}: ", hat_R, "\n")
    chain_samples <- lapply(1:chains, function(x) gibbs_sampler(num_samples, burn_in, a, b, n))
    chain_samples_mcmc <- lapply(chain_samples, function(samples) as.mcmc(samples))
    gelman_diag_result <- gelman.diag(mcmc.list(chain_samples_mcmc))
    hat_R <- gelman_diag_result$psrf
  }
  
  cat("最终 \\hat{R}: ", hat_R, "\n")
  return(chain_samples)
}

# 5. 定义联合密度函数
joint_density <- function(x, y, a, b, n) {
  coeff <- choose(n, x)  # 二项式系数
  density <- coeff * (y^(x + a - 1)) * ((1 - y)^(n - x + b - 1))
  return(density)
}

# 6. 可视化函数：绘制x和y的联合分布图
plot_samples <- function(samples) {
  plot(samples[, 1], samples[, 2], pch = 20, col = "blue",
       xlab = "x", ylab = "y", main = "Gibbs采样生成的(x, y)联合分布")
}

# 7. 可视化目标联合密度
plot_joint_density <- function(a, b, n) {
  x_values <- 0:n
  y_values <- seq(0, 1, length.out = 100)
  
  # 创建数据框以存储密度值
  density_data <- expand.grid(x = x_values, y = y_values)
  density_data$f_xy <- mapply(joint_density, density_data$x, density_data$y, MoreArgs = list(a = a, b = b, n = n))
  
  # 绘制密度图
  ggplot(density_data, aes(x = y, y = x, fill = f_xy)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis_c() +
    labs(title = "Joint Density f(x, y)",
         x = "y",
         y = "x",
         fill = "Density") +
    theme_minimal()
}

# 主程序
set.seed(42)  # 设置随机种子
chain_samples <- run_chains_until_convergence(num_samples, burn_in, a, b, n, chains)  # 运行链直到收敛

# 可视化生成的每条链的样本
for (i in 1:chains) {
  cat(paste("链", i, "的样本可视化：\n"))
  plot_samples(chain_samples[[i]])
}

# 可视化目标联合密度
plot_joint_density(a, b, n)

# 清理内存
rm(chain_samples, a, b, n, num_samples, burn_in, chains)
gc()

## -----------------------------------------------------------------------------
# 清空环境变量，释放内存
rm(list = ls())
gc()

# 加载必要的包
if(!requireNamespace("pracma", quietly = TRUE)) install.packages("pracma")
if(!requireNamespace("gsl", quietly = TRUE)) install.packages("gsl")
library(pracma)  # 用于计算伽马函数
library(gsl)     # 提供更高效的伽马函数计算

# 定义一个函数来计算第 k 项
kth_term <- function(k, a_norm, d) {
  # 计算分子的 (-1)^k / (k! * 2^k)
  term1 <- (-1)^k / (factorial(k) * 2^k)
  
  # 计算分子的 ||a||^(2k + 2) / ((2k + 1)(2k + 2))
  term2 <- a_norm^(2 * k + 2) / ((2 * k + 1) * (2 * k + 2))
  
  # 计算伽马函数项
  gamma_term <- gamma((d + 1) / 2) * gamma(k + 3 / 2) / gamma(k + d / 2 + 1)
  
  # 计算第 k 项的值
  result <- term1 * term2 * gamma_term
  return(result)
}

# 定义计算整个和的函数
compute_sum <- function(a, d, tolerance = 1e-10, max_iter = 1000) {
  # 计算向量 a 的欧几里得范数 ||a||
  a_norm <- norm(a, type = "2")
  
  # 初始化总和
  total_sum <- 0
  k <- 0
  
  # 计算逐项累加，直到项小于容忍度或达到最大迭代次数
  repeat {
    term <- kth_term(k, a_norm, d)
    total_sum <- total_sum + term
    
    # 如果当前项小于容忍度，则停止
    if (abs(term) < tolerance) break
    # 如果达到最大迭代次数，也停止
    if (k >= max_iter) {
      warning("达到最大迭代次数，结果可能不准确")
      break
    }
    
    # 更新迭代计数器
    k <- k + 1
  }
  
  return(total_sum)
}

# 测试程序
# 清空内存
gc()

# (c) 当 a = (1, 2)^T 时计算和的值
a <- c(1, 2)
d <- length(a)
result <- compute_sum(a, d)

# 输出结果
cat("当 a = (1, 2)^T 时的和为:", result, "\n")


## -----------------------------------------------------------------------------
# 清空环境变量，释放内存
rm(list = ls())
gc()

# 加载必要的包
if (!requireNamespace("pracma", quietly = TRUE)) install.packages("pracma")
library(pracma)     # 用于计算伽马函数

# 定义计算积分的函数
integrate_func <- function(f, lower, upper, k) {
  tryCatch({
    # 使用 base R 的 integrate 函数以更好地控制积分误差
    result <- integrate(function(u) f(u, k), lower, upper)$value
    return(result)
  }, error = function(e) {
    # 如果积分失败，返回一个大值表示错误
    return(1e10)
  })
}

# 定义方程中的积分核函数
integrand_left <- function(u, k) {
  (1 + u^2 / (k - 1))^(-k / 2)
}

integrand_right <- function(u, k) {
  (1 + u^2 / k)^(-(k + 1) / 2)
}

# 定义函数来求解方程
solve_for_a <- function(k, tolerance = 1e-6, max_iter = 100) {
  # 伽马函数项
  gamma_left <- 2 * gamma(k / 2) / (sqrt(pi * (k - 1)) * gamma((k - 1) / 2))
  gamma_right <- 2 * gamma((k + 1) / 2) / (sqrt(pi * k) * gamma(k / 2))
  
  # 定义误差函数，用于优化求解 a
  error_func <- function(a) {
    # 确保 a 的范围有效，避免出现分母为零的情况
    if (a^2 >= k || a^2 >= k + 1) return(1e10)
    
    # 计算 c_k 和 c_{k-1}，并检查是否接近无效的情况
    denominator_k_minus1 <- k - a^2
    denominator_k <- k + 1 - a^2
    if (denominator_k_minus1 <= 0 || denominator_k <= 0) {
      return(1e10)  # 返回大值表示错误
    }
    
    c_k_minus1 <- sqrt(a^2 * (k - 1) / denominator_k_minus1)
    c_k <- sqrt(a^2 * k / denominator_k)
    
    # 动态设置积分上限
    max_limit <- min(sqrt(k) * 10, 1e3)
    if (c_k_minus1 > max_limit || c_k > max_limit) {
      return(1e10)  # 返回大值以跳过这个计算
    }
    
    # 计算左侧积分和右侧积分
    integral_left <- integrate_func(integrand_left, 0, c_k_minus1, k)
    integral_right <- integrate_func(integrand_right, 0, c_k, k)
    
    # 检查是否为有效数值，主动将 NA/Inf 替换为大值
    if (!is.finite(integral_left) || !is.finite(integral_right) || is.nan(integral_left) || is.nan(integral_right)) {
      return(1e10)  # 主动返回一个大值
    }
    
    # 计算误差
    error_value <- abs(gamma_left * integral_left - gamma_right * integral_right)
    return(error_value)
  }
  
  # 使用 suppressWarnings 包裹 optimize 以隐藏警告
  result <- suppressWarnings(optimize(error_func, interval = c(0.2, sqrt(k) - 1), tol = tolerance))
  return(result$minimum)
}

# 计算 A(k) 的点，进行比较
compute_A_k <- function(k) {
  return(sqrt(k))
}

# 主函数，运行求解并比较结果
main <- function() {
  # 设置参数 k 的值
  k_values <- c(4, 25, 100, 500, 1000)
  results <- data.frame(k = integer(), a = numeric(), A_k = numeric())
  
  for (k in k_values) {
    # 求解 a 的值
    a <- solve_for_a(k)
    
    # 计算 A(k)
    A_k <- compute_A_k(k)
    
    # 存储结果
    results <- rbind(results, data.frame(k = k, a = a, A_k = A_k))
  }
  
  # 输出结果
  print(results)
  
  # 清空内存
  gc()
}

# 运行主函数
main()



## -----------------------------------------------------------------------------
# 清理内存并设置环境
rm(list = ls()) # 清空所有内存
gc()            # 垃圾回收

# 检查和安装所需的包
if (!require("stats")) install.packages("stats")

# 定义 EM 算法的函数
em_algorithm <- function(observed_data, tau, max_iter = 1000, tol = 1e-6) {
  n <- length(observed_data)
  # 初始估计的 lambda
  lambda <- 1 / mean(observed_data) 
  
  # EM 迭代
  for (iter in 1:max_iter) {
    # E 步：计算每个观测值的 "未观测部分" 的期望
    expected_values <- ifelse(observed_data < tau, observed_data, 
                              tau + 1 / lambda)
    
    # M 步：使用期望值更新 lambda
    new_lambda <- n / sum(expected_values)
    
    # 检查收敛
    if (abs(new_lambda - lambda) < tol) {
      lambda <- new_lambda
      break
    }
    
    # 更新 lambda
    lambda <- new_lambda
  }
  
  return(lambda)
}

# 定义最大似然估计（MLE）的函数
mle_estimate <- function(observed_data) {
  return(1 / mean(observed_data)) # MLE for lambda based on observed data
}

# 观测数据和参数
observed_data <- c(0.54, 0.48, 0.33, 0.43, 1.00, 0.91, 1.00, 0.21, 0.85)
tau <- 1

# 使用 EM 算法估计 lambda
lambda_em <- em_algorithm(observed_data, tau)
cat("EM 算法估计的 lambda 值：", lambda_em, "\n")

# 计算 MLE 估计
lambda_mle <- mle_estimate(observed_data)
cat("基于观测数据的 MLE 估计的 lambda 值：", lambda_mle, "\n")

# 再次清空内存以确保资源释放
rm(list = ls())
gc() # 垃圾回收


## -----------------------------------------------------------------------------
# 加载 lpSolve 包
library(lpSolve)

# 定义目标函数的系数
objective <- c(4, 2, 9)  # 对应于 4x + 2y + 9z

# 定义约束矩阵
constraints <- matrix(c(2, 1, 1,   # 2x + y + z
                        1, -1, 3), # x - y + 3z
                      nrow = 2, byrow = TRUE)

# 定义约束右侧的值
rhs <- c(2, 3)  # 右侧的约束值

# 定义约束方向
directions <- c("<=", "<=")

# 使用 lp 函数求解最小化问题
solution <- lp(direction = "min",      # 指定为最小化问题
               objective.in = objective, # 目标函数
               const.mat = constraints,  # 约束矩阵
               const.dir = directions,   # 约束方向
               const.rhs = rhs,          # 约束右侧值
               all.int = FALSE)          # 变量可以是连续的

# 输出结果
cat("目标函数的最小值：", solution$objval, "\n")
cat("对应的变量值：\n")
cat("x =", solution$solution[1], "\n")
cat("y =", solution$solution[2], "\n")
cat("z =", solution$solution[3], "\n")

## -----------------------------------------------------------------------------
# 加载 mtcars 数据集
data(mtcars)

# 定义公式列表
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 方法一：使用 for 循环拟合模型
models_for <- list()  # 用于存储模型结果
for (i in seq_along(formulas)) {
  models_for[[i]] <- lm(formulas[[i]], data = mtcars)
}

# 打印 for 循环拟合的模型结果摘要
cat("For 循环拟合结果摘要：\n")
for (i in seq_along(models_for)) {
  print(summary(models_for[[i]]))
}

# 方法二：使用 lapply() 拟合模型
models_lapply <- lapply(formulas, function(f) lm(f, data = mtcars))

# 打印 lapply() 拟合的模型结果摘要
cat("\nlapply() 拟合结果摘要：\n")
lapply(models_lapply, summary)


## -----------------------------------------------------------------------------
# 清空环境变量以节约内存
rm(list = ls())

# 加载数据集
data(mtcars)

# 定义函数：生成 Bootstrap 数据集
generate_bootstrap <- function(data, n) {
  # 创建 n 个 bootstrap 样本
  lapply(1:n, function(i) {
    rows <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
    data[rows, ]
  })
}

# 定义函数：拟合线性模型
fit_model <- function(data_list, formula) {
  # 针对每个数据子集拟合模型
  lapply(data_list, function(sub_data) lm(formula, data = sub_data))
}

# 定义函数：清理内存
clean_memory <- function() {
  rm(list = ls())
  gc()  # 强制垃圾回收
}

# 步骤 1：生成 10 个 Bootstrap 样本
bootstraps <- generate_bootstrap(mtcars, 10)

# 步骤 2：拟合模型 mpg ~ disp 到每个 Bootstrap 样本
models <- fit_model(bootstraps, mpg ~ disp)

# 步骤 3：检查模型结果（简要输出系数）
cat("模型回归系数:\n")
lapply(models, function(model) print(coef(model)))

# 清空内存以节省空间
clean_memory()

## -----------------------------------------------------------------------------
# 清空环境变量
rm(list = ls())

# 加载数据集
data(mtcars)

# 定义公式列表
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 定义函数：拟合模型（for 循环实现）
fit_models_for <- function(formulas, data) {
  models <- list()  # 存储模型
  for (i in seq_along(formulas)) {
    models[[i]] <- lm(formulas[[i]], data = data)
  }
  models
}

# 定义函数：拟合模型（lapply 实现）
fit_models_lapply <- function(formulas, data) {
  lapply(formulas, function(f) lm(f, data = data))
}

# 定义函数：提取 R² 值
extract_r_squared <- function(models) {
  # 使用 lapply 提取每个模型的 R² 值
  sapply(models, function(model) summary(model)$r.squared)
}

# 步骤1：使用 for 循环拟合模型
models_for <- fit_models_for(formulas, mtcars)

# 步骤2：使用 lapply 拟合模型
models_lapply <- fit_models_lapply(formulas, mtcars)

# 步骤3：提取 R² 值（for 循环模型）
r_squared_for <- extract_r_squared(models_for)

# 步骤4：提取 R² 值（lapply 模型）
r_squared_lapply <- extract_r_squared(models_lapply)

# 输出 R² 值
cat("For 循环模型的 R² 值：\n")
print(r_squared_for)

cat("\nlapply 模型的 R² 值：\n")
print(r_squared_lapply)

# 清理内存
rm(list = ls())
gc()


## -----------------------------------------------------------------------------
# 清空环境变量以节约内存
rm(list = ls())

# 加载数据集
data(mtcars)

# 定义函数：生成 Bootstrap 数据集
generate_bootstrap <- function(data, n) {
  # 创建 n 个 bootstrap 样本
  lapply(1:n, function(i) {
    rows <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
    data[rows, ]
  })
}

# 定义函数：拟合线性模型
fit_model <- function(data_list, formula) {
  # 针对每个数据子集拟合模型
  lapply(data_list, function(sub_data) lm(formula, data = sub_data))
}

# 定义函数：提取 R² 值
extract_r_squared <- function(models) {
  # 提取每个模型的 R² 值
  sapply(models, function(model) summary(model)$r.squared)
}

# 定义函数：清理内存
clean_memory <- function() {
  rm(list = ls())
  gc()  # 强制垃圾回收
}

# 步骤 1：生成 10 个 Bootstrap 样本
bootstraps <- generate_bootstrap(mtcars, 10)

# 步骤 2：拟合模型 mpg ~ disp 到每个 Bootstrap 样本
models <- fit_model(bootstraps, mpg ~ disp)

# 步骤 3：提取模型的 R² 值
r_squared_values <- extract_r_squared(models)

# 打印 R² 值
cat("每个模型的 R² 值：\n")
print(r_squared_values)

# 清空内存以节省空间
clean_memory()


## -----------------------------------------------------------------------------
# 清空环境变量
rm(list = ls())

# 定义函数：生成 t 检验结果
generate_t_tests <- function(n) {
  replicate(
    n,
    t.test(rpois(10, 10), rpois(7, 10)),
    simplify = FALSE
  )
}

# 定义函数：提取 p 值
extract_p_values <- function(t_tests) {
  # 使用 sapply 提取 p 值，直接使用 [[ 消除匿名函数
  sapply(t_tests, "[[", "p.value")
}

# 定义函数：清理内存
clean_memory <- function() {
  rm(list = ls())
  gc()  # 强制垃圾回收
}

# 步骤1：生成 100 次 t 检验
t_test_results <- generate_t_tests(100)

# 步骤2：提取每次试验的 p 值
p_values <- extract_p_values(t_test_results)

# 打印 p 值
cat("提取的 p 值：\n")
print(p_values)

# 清空内存
clean_memory()

## -----------------------------------------------------------------------------
# 清空环境变量
rm(list = ls())

# 定义函数：结合 Map 和 vapply 实现并行迭代，并存储为向量或矩阵
custom_lapply_variant <- function(FUN, ..., output_type = "vector") {
  # 检查 output_type 参数是否有效
  if (!output_type %in% c("vector", "matrix")) {
    stop("Invalid output_type. Choose 'vector' or 'matrix'.")
  }
  
  # 使用 Map 进行并行迭代
  map_results <- Map(FUN, ...)
  
  # 根据 output_type 转换结果
  if (output_type == "vector") {
    # 转换为向量
    return(unlist(map_results))
  } else if (output_type == "matrix") {
    # 转换为矩阵
    return(do.call(rbind, map_results))
  }
}

# 定义函数：清理内存
clean_memory <- function() {
  rm(list = ls())
  gc()  # 强制垃圾回收
}

# 示例1：对两个向量的对应元素求和，并存储为向量
vector_result <- custom_lapply_variant(
  FUN = `+`, 
  1:5, 
  6:10, 
  output_type = "vector"
)

cat("结果存储为向量：\n")
print(vector_result)

# 示例2：对两个向量的对应元素求和，并存储为矩阵
matrix_result <- custom_lapply_variant(
  FUN = function(x, y) c(sum = x + y, product = x * y), 
  1:5, 
  6:10, 
  output_type = "matrix"
)

cat("\n结果存储为矩阵：\n")
print(matrix_result)

# 清空内存
clean_memory()

## -----------------------------------------------------------------------------
# 清空环境变量
rm(list = ls())

# 定义函数：快速计算卡方检验统计量
fast_chisq_test <- function(x, y) {
  # 检查输入是否为数值向量
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Inputs must be numeric vectors.")
  }
  
  # 检查输入是否有缺失值
  if (any(is.na(x)) || any(is.na(y))) {
    stop("Inputs must not contain missing values.")
  }
  
  # 检查输入长度是否匹配
  if (length(x) != length(y)) {
    stop("Inputs must have the same length.")
  }
  
  # 计算观察值的频数
  observed <- table(x, y)
  
  # 计算期望值
  row_sums <- rowSums(observed)
  col_sums <- colSums(observed)
  total <- sum(observed)
  expected <- outer(row_sums, col_sums) / total
  
  # 计算卡方统计量
  chisq_stat <- sum((observed - expected)^2 / expected)
  
  return(chisq_stat)
}

# 定义函数：清理内存
clean_memory <- function() {
  rm(list = ls())
  gc()  # 强制垃圾回收
}

# 示例1：快速计算卡方统计量
x <- c(1, 2, 1, 2, 3, 3, 1, 2, 3)
y <- c(2, 2, 3, 3, 1, 1, 2, 2, 3)

chisq_result <- fast_chisq_test(x, y)
cat("快速计算的卡方统计量：\n", chisq_result, "\n")

# 示例2：比较标准 chisq.test() 的结果
observed_table <- table(x, y)
chisq_test_result <- chisq.test(observed_table)
cat("标准 chisq.test() 的卡方统计量：\n", chisq_test_result$statistic, "\n")

# 清空内存
clean_memory()

## -----------------------------------------------------------------------------
# 清空环境变量
rm(list = ls())

# 定义函数：更快的 table() 实现
fast_table <- function(x, y) {
  # 检查输入是否为整数向量
  if (!is.integer(x) || !is.integer(y)) {
    stop("Inputs must be integer vectors.")
  }
  
  # 检查输入是否有缺失值
  if (any(is.na(x)) || any(is.na(y))) {
    stop("Inputs must not contain missing values.")
  }
  
  # 检查输入长度是否匹配
  if (length(x) != length(y)) {
    stop("Inputs must have the same length.")
  }
  
  # 获取唯一值
  x_levels <- unique(x)
  y_levels <- unique(y)
  
  # 初始化频数表
  freq_table <- matrix(0, nrow = length(x_levels), ncol = length(y_levels),
                       dimnames = list(x_levels, y_levels))
  
  # 填充频数表
  for (i in seq_along(x)) {
    freq_table[as.character(x[i]), as.character(y[i])] <- freq_table[as.character(x[i]), as.character(y[i])] + 1
  }
  
  return(freq_table)
}

# 定义函数：更快的卡方检验
fast_chisq_test <- function(x, y) {
  # 调用 fast_table() 构造频数表
  observed <- fast_table(x, y)
  
  # 计算期望值
  row_sums <- rowSums(observed)
  col_sums <- colSums(observed)
  total <- sum(observed)
  expected <- outer(row_sums, col_sums) / total
  
  # 计算卡方统计量
  chisq_stat <- sum((observed - expected)^2 / expected)
  
  return(chisq_stat)
}

# 定义函数：清理内存
clean_memory <- function() {
  rm(list = ls())
  gc()  # 强制垃圾回收
}

# 示例：使用更快的 table 和卡方检验
x <- as.integer(c(1, 2, 1, 2, 3, 3, 1, 2, 3))
y <- as.integer(c(2, 2, 3, 3, 1, 1, 2, 2, 3))

# 快速生成频数表
freq_table <- fast_table(x, y)
cat("快速生成的频数表：\n")
print(freq_table)

# 快速卡方检验
chisq_stat <- fast_chisq_test(x, y)
cat("\n快速卡方检验统计量：\n", chisq_stat, "\n")

# 清空内存
clean_memory()

## -----------------------------------------------------------------------------
# 保存以下代码到 Rcpp 文件中
library(Rcpp)
sourceCpp(code = '
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix gibbs_sampler(int n_samples, int n, double a, double b, double y_init) {
  NumericMatrix samples(n_samples, 2);
  
  int x = 0;
  double y = y_init;

  for (int i = 0; i < n_samples; ++i) {
    x = R::rbinom(n, y);                  // 从 Binomial(n, y) 中采样 x
    y = R::rbeta(x + a, n - x + b);       // 从 Beta(x + a, n - x + b) 中采样 y

    samples(i, 0) = x;
    samples(i, 1) = y;
  }

  return samples;
}
')

## -----------------------------------------------------------------------------
# 参数设置
n_samples <- 10000
n <- 10
a <- 2
b <- 2
y_init <- 0.5

## -----------------------------------------------------------------------------
# 调用 Gibbs 采样器
samples <- gibbs_sampler(n_samples, n, a, b, y_init)

# 转换为数据框
samples_df <- as.data.frame(samples)
colnames(samples_df) <- c("x", "y")
head(samples_df)

## -----------------------------------------------------------------------------
hist(samples_df$y, breaks = 30, main = "Histogram of y", xlab = "y", freq = FALSE)
lines(density(samples_df$y), col = "blue", lwd = 2)

## -----------------------------------------------------------------------------
plot(samples_df$x, samples_df$y, pch = 20, col = "darkgreen", 
     main = "Scatter plot of x and y", xlab = "x", ylab = "y")

## -----------------------------------------------------------------------------
# 从 Gibbs 采样结果提取 y 的样本
gibbs_y <- samples_df$y

# 使用 R 的 rbeta 生成相同数量的随机数
beta_y <- rbeta(n_samples, shape1 = a, shape2 = b)

## -----------------------------------------------------------------------------
# 绘制 Q-Q 图
qqplot(beta_y, gibbs_y, 
       main = "Q-Q Plot: Gibbs vs Beta Random Numbers", 
       xlab = "Quantiles of Beta Distribution (R Function)", 
       ylab = "Quantiles of Gibbs Sampler")
abline(0, 1, col = "red", lwd = 2)  # 添加 y = x 的参考线

## -----------------------------------------------------------------------------
# 加载 microbenchmark 包
library(microbenchmark)

# 使用 R 内置函数实现相同逻辑的采样
gibbs_sampler_r <- function(n_samples, n, a, b, y_init) {
  x <- numeric(n_samples)
  y <- numeric(n_samples)
  y[1] <- y_init
  
  for (i in 1:(n_samples - 1)) {
    # 从 Binomial(n, y) 中采样 x
    x[i] <- rbinom(1, n, y[i])
    # 从 Beta(x + a, n - x + b) 中采样 y
    y[i + 1] <- rbeta(1, x[i] + a, n - x[i] + b)
  }
  data.frame(x = x, y = y)
}

# 比较两个函数的执行时间
benchmark_results <- microbenchmark(
  Rcpp = gibbs_sampler(n_samples, n, a, b, y_init),
  R = gibbs_sampler_r(n_samples, n, a, b, y_init),
  times = 10  # 重复实验 10 次
)

# 打印比较结果
print(benchmark_results)

## -----------------------------------------------------------------------------
library(ggplot2)
autoplot(benchmark_results)

