##pb 01
---------

  ni <- c(4, 10, 24, 12, 6, 3)
  n <- sum(ni)
  
  
  chisquare <- function(a) {
    sapply(a, function(single_a) {
      lower <- c(0, 1, 2, 3, 4, 5)
      upper <- c(1, 2, 3, 4, 5, Inf)
      p <- exp(-lower/single_a) - exp(-upper/single_a)
      
      sum((ni - n * p)^2 / (n * p))
    })
  }
  
  est <- optimize(chisquare, c(0, 10))
  
  # Print results
  cat("Estimated theta:", est$minimum, "\n")
  cat("Minimum chi-square value:", est$objective, "\n")
  
  # Plotting the chi-square function
  curve(chisquare, from = 0.1, to = 10, col = "blue", lwd = 2,
        xlab = expression(theta), ylab = "Chi^2 (theta)",
        ylim = c(20, 70))
  abline(h = est$objective, v = est$minimum, lwd=2, lty = c(2,3), col = c(2,3))


##pb 02
---------
  
    ni <- c(4, 10, 24, 12, 6, 3)
  n <- sum(ni)
  
  
  chisquare <- function(a) {
    sapply(a, function(single_a) {
      lower <- c(0, 1, 2, 3, 4, 5)
      upper <- c(1, 2, 3, 4, 5, Inf)
      p <- exp(-lower*single_a) - exp(-upper*single_a)
      
      sum((ni - n * p)^2 / (n * p))
    })
  }
  
  est <- optimize(chisquare, c(0, 10))
  
  # Print results
  cat("Estimated theta:", est$minimum, "\n")
  cat("Minimum chi-square value:", est$objective, "\n")
  
  # Plotting the chi-square function
  curve(chisquare, from = 0, to = 1, col = "blue", lwd = 2,
        xlab = expression(theta), ylab = "Chi^2 (theta)",
        ylim = c(20, 70))
  abline(h = est$objective, v = est$minimum, lwd=2, lty = c(2,3), col = c(2,3))

  ##pb 03
  ---------
  
    library(stats4)
  x = c(105, 140, 20, 113, 121, 10, 44, 150, 60, 30, 30, 11)
  n = length(x)
  
  logL = function(a, b) {
    # if (a <= 0 || b <= 0) return(Inf)
    
    term1 = n*log(b/a)
    term2 = (b-1)*sum(log(x/a))
    term3 = - sum((x/a)^b)
    
    -(term1 + term2 + term3)
  }
  
  
  fit = mle(logL,start = list(a=quantile(x,.632),b=1.5))
  summary(fit)
  coefs = coef(fit)
  
  var = vcov(fit)
  
  cat("MLE estimates: ", coefs, "\n")
  
  cat("Variance are", diag(var),  "\n")
  
  ### b
  cat("95% confidence intervals for a (scale parameter):\n")
  cat("Lower:", coefs[1] - qnorm(0.975) * sqrt(var[1, 1]), "\n")
  cat("Upper:", coefs[1] + qnorm(0.975) * sqrt(var[1, 1]), "\n")
  
  cat("95% confidence intervals for b (shape parameter):\n")
  cat("Lower:", coefs[2] - qnorm(0.975) * sqrt(var[2, 2]), "\n")
  cat("Upper:", coefs[2] + qnorm(0.975) * sqrt(var[2, 2]), "\n")
  
  percentile = function(p) {
    qweibull(p, scale = coefs[1], shape = coefs[2])
  }
  
  ### c
  cat("Estimated median:", percentile(0.5), "\n")
  cat("Estimated 63.2th percentile:", percentile(0.632), "\n")
  cat("Estimated 75th percentile:", percentile(.75), "\n")
  
summary(fit)
  
  ##pb 04
  ---------
  
  library(stats4)

x <- c(105, 140, 20, 113, 121, 10, 44, 150, 60, 30, 30, 11)

n <- length(x)
logL <- function(a, b) {
  # if (a <= 0 || b <= 0) return(Inf)  # ensure valid values
  
  term1 <- n * lgamma(a)             # n * log Gamma(a)
  term2 <- n * a * log(b)            # n * a * log(b)
  term3 <- - (a - 1) * sum(log(x))   # - (a-1) * sum(log(x))
  term4 <- sum(x)/b                  # sum(x) / b
  
  term1 + term2 + term3 + term4      # return negative log-likelihood
}

# Start values (use method of moments as a better guess)
fit <- mle(logL,
           start = list(a=quantile(x,.632),b=1.5),
           method = "L-BFGS-B",
           lower = c(0.01, 0.01))  # Ensure positive a and b

# Extract MLE and variance-covariance matrix
coefs <- coef(fit)
var <- vcov(fit)


cat("MLE estimates: ", coefs, "\n")

cat("Variance are", diag(var),  "\n")

### b
cat("95% confidence intervals for a (shape parameter):\n")
cat("Lower:", coefs[1] - qnorm(0.975) * sqrt(var[1, 1]), "\n")
cat("Upper:", coefs[1] + qnorm(0.975) * sqrt(var[1, 1]), "\n")

cat("95% confidence intervals for b (scale parameter):\n")
cat("Lower:", coefs[2] - qnorm(0.975) * sqrt(var[2, 2]), "\n")
cat("Upper:", coefs[2] + qnorm(0.975) * sqrt(var[2, 2]), "\n")

percentile = function(p) {
  qgamma(p, shape = coefs[1], scale = coefs[2])
}

### c
cat("Estimated median:", percentile(0.5), "\n")
cat("Estimated 63.2th percentile:", percentile(0.632), "\n")
cat("Estimated 75th percentile:", percentile(.75), "\n")
  
  
  ##pb 05
  ---------
  
  a1 = 0.025
a2 = 0.025

x = c(86, 146, 251, 623, 98, 175, 176, 76, 264, 15, 157, 220, 42, 321,
      180, 198, 38, 20, 61, 121, 282, 224, 189, 180, 325)

s = sum(x)
n = length(x)

# Pivotal CI
theta_lower <- qgamma(a1, shape = n, rate = s)
theta_upper <- qgamma(1-a2, shape = n, rate = s)

cat("95% CI using pivotal:", theta_lower, theta_upper, "\n")
cat("Range of CI", theta_upper - theta_lower, "\n")


# Bayesian method CI
alpha0 <- 1
beta0 <- 1

# Posterior parameters
alpha_post <- alpha0 + n
beta_post <- beta0 + s

# Credible interval
theta_bayes_lower <- qgamma(a1, shape = alpha_post, rate = beta_post)
theta_bayes_upper <- qgamma(1 - a2, shape = alpha_post, rate = beta_post)


cat("95% CI using Bayesian:", theta_bayes_upper, theta_bayes_lower, "\n")
cat("Range of CI", theta_bayes_upper - theta_bayes_lower, "\n")


##pb 06
---------
  
x = c(2.1, 5.2, 2.3, 1.4, 2.2, 2.3, 1.6)
k = 2

theta1 = median(x)
d = median(abs(x - theta1)) / 0.6745
h = (x - theta1)/d
psi <- ifelse(h < -k, -k,
              ifelse(h < k, h, k)
)

psi.prime = ifelse(h < -k, 0, ifelse(h > k, 0, 1))
theta2 = theta1 + d*sum(psi) / sum(psi.prime)

theta1
theta2


# 2nd
h = (x - theta2)/d
psi <- ifelse(h < -k, -k,
              ifelse(h < k, h, k)
)

psi.prime = ifelse(h < -k, 0, ifelse(h > k, 0, 1))

theta3 = theta2 + d*sum(psi) / sum(psi.prime)
theta3

var = d^2 * sum(psi^2) / (sum(psi.prime))^2
cat("robust var", var, "\n")

n = length(x)
## Variance of xbar
cat("Normal variance", var(x)/n)
----
#another method
library(MASS)
x=c(2.1,5.2,2.3,1.4,2.2,2.3,1.6)
firstattempt=huber(x,k=2)
mu=firstattempt$mu
mu
s=firstattempt$s
secondattempt=hubers(x, k = 2, mu, s, initmu = mu, tol = 1e-06)

mu2=secondattempt$mu
sd2=secondattempt$s
cat("Mu_2", mu2, "\n")

var= sd2^2
var

xvar=var(x)/length(x)
xvar
  
  
  