#Question 01
#--------------------------------------------------------------------------------------------
#PDF
# Function to generate Binomial probabilities using PMF concept
generate_binomial_probs <- function(n, p = 0.5) {
  q <- 1 - p
  k <- 0:n
  probs <- choose(n, k) * p^k * q^(n - k)
  return(probs)
}
# Generate and print for n = 1 to 6
for (i in 1:6) {
  cat("\n--- n =", i, "---\n")
  probs <- generate_binomial_probs(i)
  df <- data.frame(X = 0:i, Probability = round(probs, 4))
  print(df)
}

#PGF
prob_gen=function(n,p){
  q=1-p
  s=0
  probs=numeric(n+1);probs
  for(i in 0:n){
    pgf=expression((q+p*s)^n)
    di=pgf
    if(i>0){
      for(j in 1:i){
        di=D(di,"s")
      }
    }
    val=eval(di,list(s=s,p=p,q=q,n=n))
    probs[i+1]=val/factorial(i)
  }
  return(probs)
}

for (n in 1:6) {
  cat("\n--- n =", n, "---\n")
  probs <- prob_gen(n, p = 0.5)
  df <- data.frame(X = 0:n, Probability = round(probs, 4))
  print(df)
}


# Question 02
# -----------------------------------------------------------------------------------
 
z <- c(9, 90, 900, 950, 8000, 9, 90, 99, 90, 99)
a <- c(10, 100, 1000, 1000, 10000, 10, 100, 100, 100, 100)
p <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.45, 0.45, 0.45, 0.4, 0.4)


prob_ruin=function(a,z,p){
  q=1-p
  if(p==0.5){
    ruin=(a-z)/a
    
  }else{
    ruin=((q/p)^z-(q/p)^a)/(1-(q/p)^a)
  }
  return(ruin)
}

expected_dur=function(a,z,p){
  
  q=1-p
  if(p==0.5){
    e=z*(a-z)
  }
  else{
    e=(z/(q-p))-((a/(q-p))*((1-(q/p)^z)/((1-(q/p)^a))))
  }
  return(e)
}

qz=mapply(prob_ruin,a,z,p);qz
pz=1-p

dz=mapply(expected_dur,a,z,p)
dz=ceiling(dz)
dz

out=data.frame(initial_capital=a,combined_capital=z,proba_win=pz,proba_ruin=1-pz,expected_duration=dz)
out

# Question 03
# -------------------------------------------------------------------------
library(MASS)
x1 <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3, byrow = TRUE)
# Recurrence relation for matrix power
matrix_power <- function(mat, p) {
  if(p == 1) {
    return(mat)
  } else {
    return(mat %*% matrix_power(mat, p - 1))
  }
}
x2 = matrix_power(x1, 2)
x3 = matrix_power(x1, 3)
x4 = matrix_power(x1, 4)
f1 = matrix(0, 3, 3)
f2 = matrix(0, 3, 3)
f3 = matrix(0, 3, 3)
sumF = c()
mu = c()
for (i in 1:3) {
  f1[i, i] = x1[i, i]
  f2[i, i] = x2[i, i] - f1[i, i]*x1[i, i]
  f3[i, i] = x3[i, i] - f1[i, i]*x2[i, i] - f2[i, i]*x1[i, i]
  sumF[i] = f1[i, i] + f2[i, i] + f3[i, i]
  mu[i] = 1*f1[i, i] + 2*f2[i, i] + 3*f3[i, i]
}
data.frame(State = 1:3, Fii = sumF, Result = ifelse(sumF == 1, "Persistent", "Not pers
istent"))

data.frame(State = 1:3, muii = mu, Result = ifelse(mu < Inf, "Non-null persistent", "N
ull or Transient"))

#C
perriodicity <- function(mat, n=2, max_n = 100) {
  null_mat <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  if(identical(mat, matrix_power(mat, n))) {
    return(n-1)
  } else if (n < max_n) {
    return(perriodicity(mat, n + 1, max_n))
  } else {
    return(NA) # No periodicity found within max_n steps
  }
}
period <- perriodicity(x1)
period

# Question 04
# ----------------------------------------------------------------------
library(MASS)
x1 = matrix(c(1, 0, 0, 0, 0, 
              7/10, 0, 3/10, 0, 0, 
              0, 7/10, 0, 3/10, 0, 
              0, 0, 7/10, 0, 3/10, 
              0, 0, 0, 0, 1), nrow = 5, byrow = TRUE)

matpower<- function(mat,p){
  if(p==1){
    return(mat)
  }
  else{
    return(mat%*%matpower(mat,p-1))
  }
}
x2=matpower(x1,2)
x3=matpower(x1,3)
x4=matpower(x1,4)
x5=matpower(x1,5)

f1=matrix(0,5,5)
f2=matrix(0,5,5)
f3=matrix(0,5,5)
f4=matrix(0,5,5)
f5=matrix(0,5,5)
sumF=c()
mu=c()
for(i in 1:5){
  f1[i,i]=x1[i,i]
  f2[i,i]=x2[i,i]-f1[i,i]*x1[i,i]
  f3[i,i]=x3[i,i]-f1[i,i]*x2[i,i]-f2[i,i]*x1[i,i]
  f4[i,i]=x4[i,i]-f1[i,i]*x3[i,i]-f2[i,i]*x2[i,i]-f3[i,i]*x1[i,i]
  f5[i,i]=x5[i,i]-f1[i,i]*x4[i,i]-f2[i,i]*x3[i,i]-f3[i,i]*x2[i,i]-f4[i,i]*x1[i,i]
  sumF[i]=f1[i,i]+f2[i,i]+f3[i,i]+f4[i,i]+f5[i,i]
  mu[i]=1*f1[i,i]+2*f2[i,i]+3*f3[i,i]+4*f4[i,i]+5*f5[i,i]
}
sumF
mu
result <- data.frame(
  State = 1:5,
  Fii = sumF,
  muii = mu,
  Decision = ifelse(
    sumF < 1, "Transient",
    ifelse(mu < Inf, "Non-null persistent", "Null persistent")
  )
)
result

## checking period
period=function(mat,n=2,maxn=100){
  if(identical(mat,matpower(mat,n))){
    return(n-1)
  }
  else if (n<maxn){
    return(period(mat,n=n+1,maxn=100))
  }
  else{
    return("Aperiodic")
  }
}
cat("calculating the perriod of TPM :")
period(x1)

# Question 05
# -------------------------------------------------------------------------------
P1 <- matrix(c(0, 1, 0, 0, 0, 0, 0,
               0, 1/2, 1/2, 0, 0, 0, 0,
               1/2, 0, 1/2, 0, 0, 0, 0,
               0, 0, 1/4, 1/2, 1/4, 0, 0,
               0, 0, 0, 0, 0, 1/2, 1/2,
               0, 0, 0, 1,   0,   0,   0,
               0, 0, 0,   0,   0,   0 ,1), nrow =7 , byrow = TRUE)

mat_pow = function(P, pow) {
  if(pow == 1) {
    return(P)
  } else {
    return(P %*% mat_pow(P, pow - 1))
  }
}
P2 = mat_pow(P1, 2)
P3 = mat_pow(P1, 3)
P4 = mat_pow(P1, 4)
P5 = mat_pow(P1, 5)
P6 = mat_pow(P1, 6)

f1 = numeric(nrow(P1))
f2 = numeric(nrow(P1))
f3 = numeric(nrow(P1))
f4 = numeric(nrow(P1))
f5 = numeric(nrow(P1))
f6 = numeric(nrow(P1))

for(i in 1:nrow(P1)) {
  f1[i] = P1[i, i]
  f2[i] = P2[i, i] - f1[i] * P1[i, i]
  f3[i] = P3[i, i] - f1[i] * P2[i, i] - f2[i] * P1[i, i]
  f4[i] = P4[i, i] - f1[i] * P3[i, i] - f2[i] * P2[i, i] - f3[i] * P1[i, i]
  f5[i] = P5[i, i] - f1[i] * P4[i, i] - f2[i] * P3[i, i] - f3[i] * P2[i, i] - f4[i] * P1[i, i]
  f6[i] = P6[i, i] - f1[i] * P5[i, i] - f2[i] * P4[i, i] - f3[i] * P3[i, i] - f4[i] * P2[i, i] - f5[i] * P1[i, i]
}
Fii = f1 + f2 + f3 + f4 + f5 + f6
muii = 1*f1 + 2*f2 + 3*f3 + 4*f4 + 5*f5 + 6*f6

result = ifelse(Fii >= 0.8, "Persistent", "Trensient")
result2 = ifelse(Fii >= 0.8, ifelse(muii < Inf, "Non-null", "Null") , "-" )

data.frame(state = 1:nrow(P1), Fii, Result = result, muii, Result2 = result2)

periodicity = function(P, pow = 2, max_pow = 100) {
  if(identical(P, mat_pow(P, pow))){
    return(pow - 1)
  } else if(pow >= max_pow) {
    return(NA)
  } else {
    
    periodicity(P, pow = pow + 1)
  }
}
periodicity(P1)


# Question 06 poisson
# -------------------------------------------------------------------------
rate1 <- 2   # accidents per hour (7 AM - 10 AM)
time1 <- 3   # hours

rate2 <- 4   # accidents per hour (10 AM - 4 PM)
time2 <- 6   # hours

# Expected total accidents (lambda)
lambda <- rate1 * time1 + rate2 * time2
cat("Expected total accidents (lambda):", lambda, "\n")

# Probability of 10 or fewer accidents
prob_10_or_fewer <- ppois(10, lambda)
cat("Probability of 10 or fewer accidents:", prob_10_or_fewer,"\n")


#Question6 solve poisson
# -------------------------------------------------------------------------
lambda = 3
t = 3
lt = lambda * t
# Poisson process follows poisson dist with parameter lt
# dpois for pdf, pois for cdf
cat("Exactly 4:", dpois(4, lt), "\n")
cat("Higher than 4:", 1 - ppois(4, lt), "\n")
cat("Less than 4:", ppois(3, lt), "\n")

# graph
# -----------------------------------------------------------------------------
set.seed(22)
N <- 100
n <- 12
sample <- rbinom(N, n, prob = 0.5)
freq <- table(sample)
# #barplot(freq,
#         main = "Binomial Distribution (n=12, p=0.5)",
#         xlab = "Number of Successes",
#         ylab = "Frequency",
#         col = "skyblue")
# #hist(sample,
#      main = "Binomial Distribution (n=12, p=0.5)",
#      xlab = "Number of Successes",
#      ylab = "Frequency",
#      col = "skyblue")
# 6x6 matrix
P1 = matrix(c(1/3, 2/3, 0, 0, 0, 0,
              2/3, 1/3, 0, 0, 0, 0,
              0, 0, 1/4, 3/4, 0, 0,
              0, 0, 1/5, 4/5, 0, 0,
              1/4, 0, 1/4, 0, 1/4, 1/4,
              1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ), nrow = 6, byrow=TRUE)
mat_pow = function(P, pow) {
  if(pow == 1) {
    return(P)
  } else {
    return(P %*% mat_pow(P, pow - 1))
  }
}
P2 = mat_pow(P1, 2)
P3 = mat_pow(P1, 3)
P4 = mat_pow(P1, 4)
P5 = mat_pow(P1, 5)
P6 = mat_pow(P1, 6)
P7 = mat_pow(P1, 7)
P8 = mat_pow(P1, 8)
P9 = mat_pow(P1, 9)

f1 = numeric(nrow(P1))
f2 = numeric(nrow(P1))
f3 = numeric(nrow(P1))
f4 = numeric(nrow(P1))
f5 = numeric(nrow(P1))
f6 = numeric(nrow(P1))
f7 = numeric(nrow(P1))
f8 = numeric(nrow(P1))
f9 = numeric(nrow(P1))

for(i in 1:nrow(P1)) {
  f1[i] = P1[i, i]
  f2[i] = P2[i, i] - f1[i] * P1[i, i]
  f3[i] = P3[i, i] - f1[i] * P2[i, i] - f2[i] * P1[i, i]
  f4[i] = P4[i, i] - f1[i] * P3[i, i] - f2[i] * P2[i, i] - f3[i] * P1[i, i]
  f5[i] = P5[i, i] - f1[i] * P4[i, i] - f2[i] * P3[i, i] - f3[i] * P2[i, i] - f4[i] * P1[i, i]
  f6[i] = P6[i, i] - f1[i] * P5[i, i] - f2[i] * P4[i, i] - f3[i] * P3[i, i] - f4[i] * P2[i, i] - f5[i] * P1[i, i]
  f7[i] = P7[i, i] - f1[i] * P6[i, i] - f2[i] * P5[i, i] - f3[i] * P4[i, i] - f4[i] * P3[i, i] - f5[i] * P2[i, i] - f6[i] * P1[i, i]
  f8[i] = P8[i, i] - f1[i] * P7[i, i] - f2[i] * P6[i, i] - f3[i] * P5[i, i] - f4[i] * P4[i, i] - f5[i] * P3[i, i] - f6[i] * P2[i, i] - f7[i] * P1[i, i]
  f9[i] = P9[i, i] - f1[i] * P8[i, i] - f2[i] * P7[i, i] - f3[i] * P6[i, i] - f4[i] * P5[i, i] - f5[i] * P4[i, i] - f6[i] * P3[i, i] - f7[i] * P2[i, i] - f8[i] * P1[i, i]
}
Fii = f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9
muii = 1*f1 + 2*f2 + 3*f3 + 4*f4 + 5*f5 + 6*f6 + 7*f7 + 8*f8 + 9*f9

result = ifelse(Fii >= 0.87, "Persistent", "Trensient")
result2 = ifelse(Fii >= 0.87, ifelse(muii < Inf, "Non-null", "Null") , "-" )
# 10
data.frame(state = 1:nrow(P1), Fii, Result = result, muii, Result2 = result2)

# 2023 qs solve
# ---------------------------------------------------------------
# --- (a) Customer arrivals ---
lambda <- 3 * 2
cat("P(X=4) =", dpois(4, lambda), "\n")
cat("P(X>4) =", 1 - ppois(4, lambda), "\n")
cat("P(X<4) =", ppois(3, lambda), "\n\n")

# --- (b) Traffic accidents ---
λ1 <- 4*3; λ2 <- 7*6; λ3 <- 2*2
# (i)
λ_total <- λ1 + λ2 + λ3
cat("P(X≤15) =", ppois(15, λ_total), "\n")
# (ii)
λ_11to5 <- 7*6
cat("P(X≥4) =", 1 - ppois(3, λ_11to5), "\n")
# (iii)
λ_7to4 <- (4*3)+(7*6)
cat("P(X=5) =", dpois(5, λ_7to4), "\n")
# (iv)
λ_9to6 <- (4*1)+(7*6)+(2*2)
cat("Avg accidents (9–6) =", λ_9to6, "\n")
# question 2 comment
# ----------------------------------------------------------------------
# c) i)
# When win and loss probabilities are equal (p = 0.5), ruin probability depends on the ratio z/a. If both z and
# a increase proportionally, ruin probability remains the same, but expected duration increases significantly.
# For example, when z = 9 and a = 10, ruin probability is 0.1 and duration is 9. When z = 90 and a = 100, ruin
# is still 0.1 but duration becomes 900.
# However, when win probability is less than loss probability (p < 0.5), ruin probability increases, and success
# becomes less likely even with high capital. Also, expected duration decreases compared to the fair case, as
# ruin happens faster.
# When total capital a is fixed, increasing the initial capital z reduces the probability of ruin and increases the
# chance of success. For example, when a=100, if z=90, ruin probability is 0.1; but if z=99, ruin probability
# drops further to 0.01 in the fair case (p = 0.5).
# Similarly, with unequal probabilities (e.g., p=0.45), increasing z from 90 to 99 reduces ruin probability from
# 0.866 to 0.182. This shows that higher starting capital gives a clear advantage, even when the game is
# unfair.
# ii) If the game has infinite total capital (i.e., there is no absorbing upper barrier), and the probability of
# winning each round is greater than losing (i.e., p > 0.5), then the gambler has a positive probability of
# growing endlessly rich and may never be ruined.
# iii) If p<0.5, i.e., each gamble is not in the gambler's favor, then with probability 1, the gambler will
# eventually be ruined.
# 
# question 3 comment periodic kina oitar comment
# --------------------------------------------------
# d) Comment on Results:
# At first from the transition graph, we can see states 1, 2, 3 are irreducible and persistent. Then by using
# mathematical formulas we can also find that the states are persistent and specifically they non-null
# persistent.
# After that, we found that the transition probability matrix is periodic with a period of 3, indicating that the
# states cycle through every 3 steps. This cyclic behavior is consistent with the structure of the transition
# matrix, where each state transitions to the next in a fixed order.
# 
# qs 4 poisson defi
# ------------------------------------------------
# Based on the issue, 07:00 AM to 10:00 AM has a rate of 2 accidents per hour, and from 10:00 AM to 4:00
# PM has a rate of 4 accidents per hour. The total time from 7:00 AM to 4:00 PM is 9 hours, with the first 3
# hours at a rate of 2 accidents per hour and the remaining 6 hours at a rate of 4 accidents per hour. So,
# 𝜆􀬵𝑡􀬵 = 2 × 3 = 6 and 𝜆􀬶𝑡􀬶 = 4 × 6 = 24
# Following the Poisson additive property, 07:00 AM to 04:00 PM has a rate of 𝜆𝑡 = 𝜆􀬵t􀬵 + 𝜆􀬶t􀬶 = 6 + 24 =
# 30 accidents.
# Now, the probability that there will be 10 or fewer accidents on a day between 7:00 AM and 4:00 PM can
# be calculated using the cumulative distribution function (CDF) of the Poisson distribution:
