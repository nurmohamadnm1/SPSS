f <- c(3,3,5,565,76,8,768)
m <- 2^32
a <- 1103515245
c <- 12345
#problem 01
lcg <- function(n) {
  U <- c()
  m <- 2^32
  a <- 1103515245
  c <- 12345
  
  # Set the seed using the current system time in microseconds
  Z <- as.numeric(Sys.time())*1000
  
  for(i in 1:n) {
    Z <- (a * Z + c) %% m  # Update Z using the LCG formula
    U[i] <- Z / m          # Normalize to get a uniform random number
  }
  return(U)
}

### b
n = c(100, 500, 1000)
sample_1 = lcg(n[1])
sample_2 = lcg(n[2])
sample_3 = lcg(n[3])

### c
par(mfrow = c(1, 3))
hist(sample_1, col = 2, breaks = 10)
hist(sample_2, col = 3, breaks = 10)
hist(sample_3, col = 4, breaks = 10)

### d
ks.test(sample_1, "punif")
ks.test(sample_2, "punif")
ks.test(sample_3, "punif")

# graph Conclusion: As the sample size increases, the empirical distribution becomes more uniform. This is
# consistent with the Law of Large Numbers. So, visually, the LCG appears to perform well in approximating
# uniform randomness.

# Interpretation:
#   In all three cases, the p-values are greater than 0.05, so we fail to reject the null hypothesis that the data
# comes from a uniform distribution.
# This suggests that there is no significant deviation from uniformity in the output of the LCG, at least for
# these sample sizes.
# 􊵓􊵔􊵕􊵖 Conclusion: The LCG-generated numbers pass the Kolmogorov-Smirnov test for uniformity, indicating
# statistically acceptable behavior for basic simulations.

# problem 02
### a
X = seq(-5,10,by=0.1)
pdf.X = dnorm(X,3,2)
pdf.X

### b
plot(X, pdf.X, type="l", main="PDF of N(3,4)",
     xlab="x", ylab="Density", col="blue", lwd=2)

### c
set.seed(111)
n = 1000
nor.x = rnorm(n,3,2)
summary(nor.x)
sd(nor.x)
hist(nor.x, main="Histogram of N(3,4)", freq = F,
     xlab="x", ylab="Density", col="lightblue", border="black")
lines(X, pdf.X, col="red", lwd=2)

# Adding a legend to the histogram
legend("topright", legend=c("PDF", "Histogram"),
       fill=c("red", "lightblue"))

# Conclusion: The simulated data closely matches the theoretical 𝑁(3, 2􀬶) distribution, confirming the
# effectiveness of rnorm() for normal random variate generation. The summary statistics and visual fit
# validate the simulation.

#problem 03
### a
set.seed(123)  # For reproducibility
x <- rpois(1000, 5)
hist(x, main = "Histogram of Poisson(5)", xlab = "Value", ylab = "Frequency", col = "lightgreen", border = "black")
boxplot(x, main = "Boxplot of Poisson(5)", ylab = "Value", col = "lightgreen")
summary(x)
mean(x)
var(x)
# The simulation perfectly captures Poisson properties 
# (discreteness, mean=variance, right skew).
### b
set.seed(123)  # For reproducibility
y <- rexp(1000, 1/5)
hist(y, main = "Histogram of Exponential(5)", xlab = "Value", ylab = "Frequency", col = "hotpink", border = "black")
boxplot(y, main = "Boxplot of Exponential(5)", ylab = "Value", col = "hotpink")
summary(y)
mean(y)
var(y)
# The simulation accurately replicates an Exponential(5) distribution, capturing its key
# properties (right-skew, mean/variance relationship, heavy tail). Minor deviations are normal for n=1000
# samples
#problem 04
### a
library(MASS)
set.seed(123)  # For reproducibility
n <- 1000

# a) MLEs for Normal Distribution
nor_data <- rnorm(n, mean = 10, sd = 2)
mle_nor <- fitdistr(nor_data, "normal")
print("Normal")
mle_nor$estimate

# b) MLEs for Weibull Distribution
weib_data <- rweibull(n, shape = 2.1, scale = 1.1)
mle_weib <- fitdistr(weib_data, "weibull")
print("Weibull")
mle_weib$estimate

# c) MLEs for Gamma Distribution
gamma_data <- rgamma(n, shape = 3.5, rate = 0.5)
mle_gamma <- fitdistr(gamma_data, "gamma")
print("Gamma")
mle_gamma$estimate

# Comment on MLEs from Generated Data
# 1. Normal Distribution (N(10, 2²))
# o The MLE estimates were mean ≈ 10.03 and sd ≈ 1.98, very close to the true values (μ = 10, σ =                                                                                     2).
# o This confirms that MLE is consistent, as the estimates are accurate with a large sample size (n                                                                                             = 1000).
# 2. Weibull Distribution (shape = 2.1, scale = 1.1)
# o The estimated shape ≈ 2.10 and scale ≈ 1.10 also match the true values very closely.
# o This shows that the Weibull MLEs are also highly efficient with sufficient data.
# 3. Gamma Distribution (shape = 3.5, rate = 0.5)
# o MLEs yielded shape ≈ 3.38, rate ≈ 0.48, which are close but slightly biased.
# o Some small bias is expected due to the skewness of the gamma distribution, but the estimates
# still show good convergence to the true parameters.
# MLE performs very well for large samples. All three distributions produced parameter estimates close to
# the true values, validating the effectiveness of MLE in practical applications.

# #problem 05
library(MASS)
### a
invt.Weib <- function(n, beta, eta) {
  U <- runif(n)  # Generate n uniform random numbers
  X <- eta * (-log(1 - U))^(1 / beta)  # Inverse transformation for Weibull distribution
  return(X)
}

### b
set.seed(123)  # For reproducibility
x.weib <- invt.Weib(100, 2.0, 10.0)
mle_weib <- fitdistr(x.weib, "weibull")
mle_weib$estimate

### c
n <- seq(10, 3000, 50)
beta.true <- 2.0
eta.true <- 10.0
beta.hat <- c()
eta.hat <- c()

for(i in 1:length(n)) {
  x.weib <- invt.Weib(n[i], beta.true, eta.true)
  mle_weib <- fitdistr(x.weib, "weibull")
  beta.hat[i] <- mle_weib$estimate[1]
  eta.hat[i] <- mle_weib$estimate[2]
}

plot(n, beta.hat, type = "l", col = "blue", lwd=2,
     xlab = "Sample Size (n)", ylab = "MLE of Beta",
     main = "MLE of Beta vs Sample Size")
abline(h = beta.true, col = "red", lty = 2)
legend("topright", legend = c("MLE of Beta", "True Beta"), col = c("blue", "red"), lty = 1:2, cex = 0.6)

plot(n, eta.hat, type = "l", col = "green", lwd=2,
     xlab = "Sample Size (n)", ylab = "MLE of Eta",
     main = "MLE of Eta vs Sample Size")
abline(h = eta.true, col = "red", lty = 2)
legend("topright", legend = c("MLE of Eta", "True Eta"), col = c("green", "red"), lty = 1:2, cex = 0.6)

# As sample size increases from 10 to 3000, the MLEs of both β (shape) and η (scale) for the Weibull
# distribution show classical convergence behavior. Initially, with small samples, the estimators are unstable
# and biased, particularly β. However, after n>1000, the estimates for both parameters stabilize tightly
# around their true values (β = 2.0, η = 10.0), demonstrating the consistency and asymptotic unbiasedness of
# MLEs. The plot of β shows more fluctuation than η, suggesting the shape parameter is more sensitive to
# sample variation, a known property in Weibull modeling

#problem 06
### a
mu=2
sigma=2
n=10
nsim=10

xbar = c()
xvar = c()
set.seed(123)

for(i in 1:nsim){
  x <- rnorm(n,mu,sigma)
  xbar[i] <- mean(x)
  xvar[i] <- var(x)
  cat(i, "Mean:", xbar[i], "Var:", xvar[i],"\n")
}


### b
sample_mean <- mean(xbar)
cat("Sample Mean:", sample_mean, "\nPopulation Mean:", mu, "\n")


### c
nsim = 1000

for(i in 1:nsim){
  x <- rnorm(n,mu,sigma)
  xbar[i] <- mean(x)
  xvar[i] <- var(x)
}

par(mfrow = c(1, 2))

hist(xbar, breaks = 10, main = "Sample Means (Hist)", xlab = "Sample Mean", col = "lightblue", border = "black")
qqnorm(xbar, main = "QQ-Plot")
qqline(xbar, col = "red")

# Comment: The histogram of sample means shows a roughly normal distribution centered around the
# population mean (2). The QQ-plot indicates that the sample means follow a normal distribution closely, as
# the points lie along the red line.

### d
mu <- 2; sigma <- 2
n <- c(10, 100, 1000)
nsim <- 1000

xbar1 <- c()
xbar2 <- c()
xbar3 <- c()
xvar1 <- c()
xvar2 <- c()
xvar3 <- c()
set.seed(123)

for(i in 1:nsim){
  x1<-rnorm(n[1], mu, sigma)
  x2<-rnorm(n[2], mu, sigma)
  x3<-rnorm(n[3], mu, sigma)
  
  xbar1[i]<-mean(x1)
  xbar2[i]<-mean(x2)
  xbar3[i]<-mean(x3)
  
  xvar1[i]<-var(x1)
  xvar2[i]<-var(x2)
  xvar3[i]<-var(x3)
}

mean = c(mean(xbar1), mean(xbar2), mean(xbar3))
var  = c(var(xbar1), var(xbar2), var(xbar3))

data.frame(n = n, xbar = mean, variance = var)

# Comment: As the sample size increases from n=10 to n=1000, the sample means converge towards the
# population mean(2), and the variance of the sample means decreases. This is consistent with the Central
# Limit Theorem, which states that larger samples yield more accurate estimates of the population
# parameters.

### e (part 1)
par(mfrow = c(1, 3))
hist(xbar1, breaks = 10, main = "n=10" , xlab = "Sample Mean", col = "lightblue", border = "black")
hist(xbar2, breaks = 10, main = "n=100", xlab = "Sample Mean", col = "lightgreen", border = "black")
hist(xbar3, breaks = 10, main = "n=1000", xlab = "Sample Mean", col = "lightcoral", border = "black")

### e (part - 2)
par(mfrow = c(1, 3))
qqnorm(xbar1, main = "n=10")
qqline(xbar1, col = "red")
qqnorm(xbar2, main = "n=100")
qqline(xbar2, col = "red")
qqnorm(xbar3, main = "n=1000")
qqline(xbar3, col = "red")

### f
mu <- 2; sigma <- 2
n = 100; nsim = 1000
lower_95 <- c()
upper_95 <- c()
lower_99 <- c()
upper_99 <- c()

set.seed(123)
for(i in 1:nsim){
  x <- rnorm(n, mu, sigma)
  xbar <- mean(x)
  se <- sigma / sqrt(n)
  
  # 95% CI
  lower_95[i] <- xbar - qnorm(0.975) * se
  upper_95[i] <- xbar + qnorm(0.975) * se
  
  # 99% CI
  lower_99[i] <- xbar - qnorm(0.995) * se
  upper_99[i] <- xbar + qnorm(0.995) * se
}

# data.frame(simulation = 1:nsim, lower_95, upper_95, lower_99, upper_99)

ave.in.ci95 = mean((lower_95<=2)&(2<=upper_95))
ave.in.ci99 = mean((lower_99<=2)&(2<=upper_99))

cat("Coverage of 95% CI:", ave.in.ci95, "\n")
cat("Coverage of 99% CI:", ave.in.ci99, "\n")

# Comment: The coverage of the 95% confidence interval is 0.963 ≈ 0.95, and for the 99% confidence
# interval, it is approximately 99.4 ≈ 0.99. This indicates that both confidence intervals are performing as
# expected, capturing the true population mean (2) in about 95% and 99% of the simulations, respectively.

#g
mu <- 2; sigma <- 2
n <- 100; nsim <- 1000
lower_95 <- c()
upper_95 <- c()
lower_99 <- c()
upper_99 <- c()

set.seed(123)
for(i in 1:nsim){
  x <- rnorm(n, mu, sigma)
  xbar <- mean(x)
  S <- sd(x)  # Sample standard deviation
  
  # 95% CI
  lower_95[i] <- xbar - qt(0.975, df = n - 1) * (S / sqrt(n))
  upper_95[i] <- xbar + qt(0.975, df = n - 1) * (S / sqrt(n))
  
  # 99% CI
  lower_99[i] <- xbar - qt(0.995, df = n - 1) * (S / sqrt(n))
  upper_99[i] <- xbar + qt(0.995, df = n - 1) * (S / sqrt(n))
}

# data.frame(simulation = 1:nsim, lower_95, upper_95, lower_99, upper_99)

ave.in.ci95 = mean((lower_95<=2)&(2<=upper_95))
ave.in.ci99 = mean((lower_99<=2)&(2<=upper_99))

cat("Coverage of 95% CI:", ave.in.ci95, "\n")
cat("Coverage of 99% CI:", ave.in.ci99, "\n")
# Comment: The coverage of the 95% confidence interval is approximately 0.968 ≈ 0.95, and for the
# 99% confidence interval, it is approximately 0.995 ≈ 0.99. This indicates that both confidence intervals are
# performing as expected, capturing the true population mean (2) in about 95% and 99% of the simulations,
# respectively, even when the population variance is unknown and estimated from the sample.

#problem 07

#install.packages("caret")
library(caret)      # for computing cross-validation methods
library(dplyr)
data("swiss")       # Load the data
#sample_n(swiss, 3)  # Display first 5 rows of the dataset

# a) Splitting the dataset into training (80%) and test (20%)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(swiss$Fertility, p = 0.8, list = FALSE)
train_data <- swiss[train_index, ]
test_data <- swiss[-train_index, ]


# b) Fit the model on the training data
model <- lm(Fertility ~ . , data = train_data)  # . means every other variable in the dataset
# Predict on the test data
predictions <- predict(model, newdata = test_data)

val_results = data.frame(
  R2 = R2(predictions, test_data$Fertility),
  RMSE = RMSE(predictions, test_data$Fertility),
  MAE = MAE(predictions, test_data$Fertility),
  PER = RMSE(predictions, test_data$Fertility) / mean(test_data$Fertility)
)

# R2:   R-squared
# RMSE: Root Mean Squared Error
# MAE:  Mean Absolute Error
# PER:  Prediction error rate
val_results
# Comment on Prediction Error: The model explains about 59.5% of the variance in fertility (R² ≈ 0.59),
# indicating a moderate fit. The Root Mean Squared Error (RMSE ≈ 6.41) and Mean Absolute Error (MAE ≈
# 5.65) suggest that, on average, predictions deviate from actual values by about 5–6 units. The percentage
# error (PER ≈ 8.8%) indicates relatively low average prediction error compared to the scale of the data.
# Overall, the model performs reasonably well, but there is room for improvement.

### c) i)
# Define training control for LOOCV
train.control <- trainControl(method = "LOOCV")

# Train the model using LOOCV
model.loocv<-train(Fertility ~., data = swiss,
                   method = "lm", trControl= train.control)


model.loocv$results

### c) ii)
# Define training control for k-fold CV
set.seed(123)  # For reproducibility
train.control <- trainControl(method = "cv", number = 10)

# Train the model using k-fold CV
model.kfold <- train(Fertility ~ ., data = swiss,
                     method = "lm", trControl = train.control)

model.kfold$results

### c) iii)

# Define training control for repeated k-fold CV
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# Train the model using repeated k-fold CV
model.repeated <- train(Fertility ~ ., data = swiss,
                        method = "lm", trControl = train.control)


model.repeated$results
# Validation Set Approach
# The model was trained on 80% and tested on 20% of the data.
# o It produced R² ≈ 0.595, RMSE ≈ 6.41, and PER ≈ 8.8%, showing moderate predictive
# accuracy.
# 2. Leave-One-Out Cross-Validation (LOOCV)
# o Each observation is used once as test data; model is trained on the remaining 46.
# 3. 10-Fold Cross-Validation
# o The data is split into 10 folds; each fold is used once for testing.
# o Performed best with RMSE ≈ 7.28 and R² ≈ 0.76, indicating better generalization than
# LOOCV or a single validation set.
# o It offers a good balance between bias and variance.

#problem 08-----

set.seed(123)  # For reproducibility
n <- 1000
sigma <- 1
b <- c(67, -0.17, -0.26, -0.87, 0.10, 1.08)  # Coefficients

# Generate independent variables from specified distributions
Agriculture <- runif(n, min = 1, max = 100)
Examination <- runif(n, min = 1, max = 40)
Education <- runif(n, min = 1, max = 60)
Catholic <- runif(n, min = 1, max = 100)
Infant.Mortality <- runif(n, min = 1, max = 30)

e <- rnorm(n, mean = 0, sd = sigma)  # Error term

# Generate random variables for the linear regression model
Fertility <- b[1] + b[2] * Agriculture + b[3] * Examination +
  b[4] * Education + b[5] * Catholic +
  b[6] * Infant.Mortality + e

# Create a data frame
swiss_generated <- data.frame(
  Fertility,
  Agriculture,
  Examination,
  Education,
  Catholic,
  Infant.Mortality
)

#swiss_generated

library(tidyverse) # for data manipulation & visualization
library(caret) # for computing cross-validation methods
# Splitting the dataset into training (80%) and test (20%)
set.seed(123) # For reproducibility
train_index <- createDataPartition(swiss_generated$Fertility, p = 0.8, list = FALSE)
train_data <- swiss_generated[train_index, ]
test_data <- swiss_generated[-train_index, ]
# Fit the model on the training data
model <- lm(Fertility ~ . , data = train_data) # . means every other variable in the

# Predict on the test data
predictions <- predict(model, newdata = test_data)
val_results = data.frame(
  R2 = R2(predictions, test_data$Fertility),
  RMSE = RMSE(predictions, test_data$Fertility),
  MAE = MAE(predictions, test_data$Fertility),
  PER = RMSE(predictions, test_data$Fertility) / mean(test_data$Fertility)
)
val_results

# Define training control for LOOCV
train.control <- trainControl(method = "LOOCV")

# Train the model using LOOCV
model.loocv<-train(Fertility ~., data = swiss_generated,
                   method = "lm", trControl= train.control)

model.loocv$results

# Define training control for k-fold CV

set.seed(123)  # For reproducibility
train.control <- trainControl(method = "cv", number = 10)

# Train the model using k-fold CV
model.kfold <- train(Fertility ~ ., data = swiss_generated,
                     method = "lm", trControl = train.control)

model.kfold$results

# Define training control for repeated k-fold CV
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# Train the model using repeated k-fold CV
model.repeated <- train(Fertility ~ ., data = swiss_generated,
                        method = "lm", trControl = train.control)

model.repeated$results


##### 7 nbr manual process
data("swiss")       # Load the data

n = nrow(swiss)
set.seed(123)
train_index = sample(1:n, n*0.8)

train_data = swiss[train_index, ]
test_data = swiss[-train_index, ]

# Fit a linear model on the training data
model = lm(Fertility ~ . , data = train_data)

# Make predictions on the test data
yhat = predict(model, newdata = test_data)
y = test_data$Fertility

# Evaluate the model performance using Mean Squared Error (MSE)
e = y - yhat
mse = mean(e^2)
rmse = sqrt(mse)

mse
rmse
### LOOCV
data("swiss")       # Load the data
n = nrow(swiss)
mse_vec = c()

for(i in 1:n) {
  train_data = swiss[-i, ]
  test_data = swiss[i, ]
  model = lm(Fertility ~ ., data = train_data)
  yhat = predict(model, test_data)
  y = test_data$Fertility
  
  mse_vec[i] = (y - yhat)^2
}

mse = mean(mse_vec)
rmse = sqrt(mse)
mse
rmse
set.seed(133)
data("swiss")

k <- 10
n <- nrow(swiss)
folds <- sample(rep(1:k, length.out = n))  # random fold assignment
folds
mse_vec <- c()

for(i in 1:k){
  train <- swiss[folds != i, ]
  test  <- swiss[folds == i, ]
  
  model <- lm(Fertility ~ ., data = train)
  yhat <- predict(model, test)
  mse_vec <- c(mse_vec, (test$Fertility - yhat)^2)
}

MSE  <- mean(mse_vec)
RMSE <- sqrt(MSE)

MSE
RMSE

set.seed(123)
data("swiss")

k <- 10        # number of folds
repeats <- 3   # number of repetitions
n <- nrow(swiss)

mse_vec <- c()  # store all squared errors

for(r in 1:repeats){
  # Randomly assign folds for this repetition
  folds <- sample(rep(1:k, length.out = n))
  
  for(i in 1:k){
    train <- swiss[folds != i, ]
    test  <- swiss[folds == i, ]
    
    model <- lm(Fertility ~ ., data = train)
    yhat <- predict(model, test)
    
    mse_vec <- c(mse_vec, (test$Fertility - yhat)^2)
  }
}

# Final MSE and RMSE
MSE  <- mean(mse_vec)
RMSE <- sqrt(MSE)

MSE
RMSE


