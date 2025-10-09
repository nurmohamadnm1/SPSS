1 no question

R code
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



# Generate binomial probabilities using PGF derivative formula
prob_pgf <- function(n, p = 0.5) {
  q <- 1 - p
  s <- 0
  pgf <- expression((q + p*s)^n)
  probs <- numeric(n + 1)
  for (i in 0:n) {
    # Take i derivatives wrt s
    for (j in 1:i) {
      di <- D(pgf, "s")
    }
    # Evaluate at s = 0, then divide by i!
    val <- eval(di, list(s = s, p = p, q = q, n = n))
    probs[i + 1] <- val / factorial(i)
  }
  return(probs)
}
# Run for n = 1 to 6
for (n in 1:6) {
  cat("\n--- n =", n, "---\n")
  probs <- prob_pgf(n, p = 0.5)
  df <- data.frame(X = 0:n, Probability = round(probs, 4))
  print(df)
}


2 no question

prob_ruin <- function(z, a, p) {
  q <- 1 - p
  if (p == 0.5) {
    return((a - z) / a) # Symmetric case
  } else {
    return(( (q/p)^a - (q/p)^z ) / ( (q/p)^a - 1 )) # Asymmetric case
  }
}
expected_duration <- function(z, a, p) {
  q <- 1 - p
  if (p == 0.5) {
    return(z * (a - z)) # Symmetric case
  } else {
    term1 = z / (q - p)
    term2 = (a / (q - p)) * (1 - (q/p)^z) / (1 - (q/p)^a)
    return(term1 - term2) # Asymmetric case
  }
}
data <- data.frame(
  z = c(9, 90, 900, 950, 8000, 9, 90, 99, 90, 99),
  a = c(10, 100, 1000, 1000, 10000, 10, 100, 100, 100, 100),
  p = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.45, 0.45, 0.45, 0.4, 0.4),
  q = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.55, 0.55, 0.55, 0.6, 0.6)
)
qz = round(mapply(prob_ruin, data$z, data$a, data$p), 3)
data$qz <- qz
data$pz <- 1 - qz
data$Dz <- round(mapply(expected_duration, data$z, data$a, data$p), 2)
data$Dz <- format(data$Dz, scientific = FALSE)
data


3 no question

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
State Fii Result
1 1 1 Persistent
2 2 1 Persistent
3 3 1 Persistent
data.frame(State = 1:3, muii = mu, Result = ifelse(mu < Inf, "Non-null persistent", "N
ull or Transient"))


c) Periodicity of Transition Probability Matrix:
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
  
  
  4 no question
  
  library(MASS)
  x1 = matrix(c(1, 0, 0, 0, 0,
                7/10, 0, 3/10, 0, 0,
                0, 7/10, 0, 3/10, 0,
                0, 0, 7/10, 0, 3/10,
                0, 0, 0, 0, 1), nrow = 5, byrow = TRUE)
  # fractions(x1) # Display the matrix with fractions
  # Recurrence relation for matrix power
  matrix_power <- function(mat, p) {
    if(p == 1) {
      return(mat)
    } else {
      return(mat %*% matrix_power(mat, p - 1))
    }
  }
  n_max <- 10 # up to P^10
  X <- list()
  for (i in 1:n_max) {
    X[[i]] <- matrix_power(x1, i)
  }
  f <- vector("list", length = nrow(x1)) # f[[i]] will store all f_ii^(n)
  for (i in 1:5) {
    f[[i]] <- numeric(n_max) # empty vector for each state
    for (n in 1:n_max) {
      if (n == 1) {
        f[[i]][n] <- X[[1]][i, i]
      } else {
        sum_term <- 0
        for (r in 1:(n - 1)) {
          sum_term <- sum_term + f[[i]][r] * X[[n - r]][i, i]
        }
        f[[i]][n] <- X[[n]][i, i] - sum_term
      }
    }
  }
  Fii <- sapply(f, sum)
  muii <- sapply(f, function(v) sum(seq_along(v) * v))
  result <- data.frame(
    State = 1:5,
    Fii = round(Fii, 5),
    Persistence = ifelse(Fii == 1, "Persistent", "Transient"),
    muii = round(muii, 5),
    Type = ifelse(Fii == 1 & muii < Inf, "Non-null Persistent", "Null or Transient")
  )
  print(result)
  
  
  c) Periodicity of Transition Probability Matrix:
  perriodicity <- function(mat, n=2, max_n = 100) {
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
  [1] NA
  
  
  5 no question
  
  library(MASS)
  x1 <- matrix(c(0, 1, 0, 0, 0, 0, 0,
                 0, 1/2, 1/2, 0, 0, 0, 0,
                 1/2, 0, 1/2, 0, 0, 0, 0,
                 0, 0, 1/4, 1/2, 1/4, 0, 0,
                 0, 0, 0, 0, 0, 1/2, 1/2,
                 0, 0, 0, 1, 0, 0, 0,
                 0, 0, 0, 0, 0, 0 ,1), nrow =7 , byrow = TRUE)
  # fractions(x1)
  # Recurrence relation for matrix power
  matrix_power <- function(mat, p) {
    if(p == 1) {
      return(mat)
    } else {
      return(mat %*% matrix_power(mat, p - 1))
    }
  }
  n_max <- 20 # up to P^20
  X <- list()
  for (i in 1:n_max) {
    X[[i]] <- matrix_power(x1, i)
  }
  f <- vector("list", length = nrow(x1)) # f[[i]] will store all f_ii^(n)
  for (i in 1:ncol(x1)) {
    f[[i]] <- numeric(n_max) # empty vector for each state
    for (n in 1:n_max) {
      if (n == 1) {
        f[[i]][n] <- X[[1]][i, i]
      } else {
        sum_term <- 0
        for (r in 1:(n - 1)) {
          sum_term <- sum_term + f[[i]][r] * X[[n - r]][i, i]
        }
        f[[i]][n] <- X[[n]][i, i] - sum_term
      }
    }
  }
  Fii <- sapply(f, sum)
  muii <- sapply(f, function(v) sum(seq_along(v) * v))
  result <- data.frame(
    State = 1:ncol(x1),
    Fii = round(Fii, 4),
    Persistence = ifelse(round(Fii, 4) == 1, "Persistent", "Transient"),
    muii = round(muii, 4),
    Type = ifelse(round(Fii, 4) == 1 & muii < Inf, "Non-null Persistent", "Null or Trans
ient")
  )
  print(result)
  
  d) Periodicity of Transition Probability Matrix:
  perriodicity <- function(mat, n=2, max_n = 100) {
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
  [1] NA