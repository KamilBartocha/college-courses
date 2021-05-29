#ANOVA 3 population problem (F-test)

# 1. Create a random sample (create your own data)
# 2. State the full model Y = X beta + epsilon where X 
#     is the vector of indicators
# 3. Compute the projection of Y on the column space of X 
#     (or give X betahat)
# 4. Give the residual vector, compute RSS(1)
# 5. State the reduced model under H0: mu1 = mu2 = mu3
# 6. Compute the projection of Y onto the column space X0
# 7. Compute the residual vector, compute RSS(0)
# 8. Construct the F-test, conduct the test

# H0, H1
Fstatistic <- function(y1, y2, y3) {
  n1 <- length(y1)
  n2 <- length(y2)
  n3 <- length(y3)
  y <- c(y1, y2, y3)
  
  X1 <- c(rep(1,n1), rep(0,n2), rep(0, n3))
  X2 <- c(rep(0,n1), rep(1,n2), rep(0, n3))
  X3 <- c(rep(0,n1), rep(0,n2), rep(1, n3))
  X <- cbind(X1, X2, X3)
  
  beta.hat <- solve(t(X)%*%X) %*% t(X) %*% y
  R1 <- y - X %*% beta.hat
  RSS1 <- t(R1) %*% R1
  
  X0 <- rep(1, n1 + n2 + n3)
  beta.hat0 <- solve(t(X0)%*%X0) %*% t(X0) %*% y
  
  R0 <- y - X0 %*% beta.hat0
  RSS0 <- t(R0) %*% R0
  
  df0 <- n1 + n2 + n3 - 1
  df1 <- n1 + n2 + n3 - 3
  F.num <- (RSS0 - RSS1)/(df0 - df1)
  F.den <- RSS1/df1
  F.stat <- F.num/F.den
}


y1 <- rnorm(10, mean = 50, sd = 10)
y2 <- rnorm(10, mean = 50, sd = 10)
y3 <- rnorm(10, mean = 50, sd = 10)
print(Fstatistic(y1,y2,y3))


mc_Fstat <- function(alpha, N, pars, seed) {
  set.seed(seed)
  n.reject <- 0
  
  for (i in 1:N) {
    y1 <- rnorm(pars$n1, pars$mu1, pars$sigma1)
    y2 <- rnorm(pars$n2, pars$mu2, pars$sigma2)
    y3 <- rnorm(pars$n3, pars$mu3, pars$sigma3)
    
    df0 <- pars$n1 + pars$n2 + pars$n3 - 1
    df1 <- pars$n1 + pars$n2 + pars$n3 - 3
    
    F.stat <- Fstatistic(y1, y2, y3)
    if (F.stat > qf(1 - alpha/2, df0 - df1, df1)) {
      n.reject <-  n.reject + 1
    }
    est.sig.level <- n.reject / N
  }
  print(est.sig.level)
}

# Case 1: Null is true, mu1 = mu2 = mu3 = mu
n1 <- n2 <- n3 <- 10
mu1 <- mu2 <- mu3 <- 0
sigma1 <- sigma2 <- sigma3 <- 1

pars <- list(n1 = n1, n2 = n2, n3 = n3, mu1 = mu1, mu2 = mu2, mu3 = mu3, 
             sigma1 = sigma1, sigma2 = sigma2, sigma3 = sigma3)
mc_Fstat(alpha = 0.1, N = 10000, pars, seed = 1234)


# Null Distribution
F.simulation <- function() {
  Fstatistic(rnorm(n1, mean = mu1, sd = sigma1), 
             rnorm(n2, mean = mu2, sd = sigma2),
             rnorm(n3, mean = mu3, sd = sigma3))
}

Fstat.vector <- replicate(10000, F.simulation()) 
plot(density(Fstat.vector), xlim = c(0, 8), ylim = c(0, 1), 
     lwd = 3, main = "")
curve(df(x, 2, n1 + n2 + n3 - 3), add = TRUE, col = "red", lwd = 2)


# Case 2: Null is not true, e.g. mu1 = mu2, not equal to mu3
n1 <- n2 <- n3 <- 1000
mu1 <- mu2 <- 0
mu3 <- 1
sigma1 <- sigma2 <- sigma3 <- 1

pars <- list(n1 = n1, n2 = n2, n3 = n3, mu1 = mu1, mu2 = mu2, mu3 = mu3, 
             sigma1 = sigma1, sigma2 = sigma2, sigma3 = sigma3)
mc_Fstat(alpha = 0.1, N = 10000, pars, seed = 1234)


# Non-Null Distribution
F.simulation <- function() {
  Fstatistic(rnorm(n1, mean = mu1, sd = sigma1), 
             rnorm(n2, mean = mu2, sd = sigma2),
             rnorm(n3, mean = mu3, sd = sigma3))
}

Fstat.vector <- replicate(10000, F.simulation()) 
plot(density(Fstat.vector), xlim = c(0, max(Fstat.vector)), ylim = c(0, 0.02), 
     lwd = 3, main = "")
curve(df(x, 2, n1 + n2 + n3 - 3), add = TRUE, col = "red", lwd = 2)


# Recall: t statistic
n1 <- n2 <- n3 <- 1000
mu1 <- mu2 <- mu3 <- 0
sigma1 <- sigma2 <- sigma3 <- 1

tstatistic <- function(x, y) {
  m <- length(x)
  n <- length(y)
  sp <- sqrt(((m - 1)*sd(x)^2 + (n - 1)*sd(y)^2)/(m + n - 2))
  t.stat <- (mean(x) - mean(y))/(sp*sqrt(1/m + 1/n))  
}

t.simulation <- function() {
  x <- c(rnorm(n1, mean = mu1, sd = sigma1),rnorm(n2, mean = mu2, sd = sigma2))
  y <- rnorm(n3, mean = mu3, sd = sigma3)
  tstatistic(x,y)
}

tstat.vector <- replicate(10000, t.simulation()) 
# empirical null distribution
plot(density(tstat.vector), xlim = c(-4, 4), ylim = c(0, 0.4), 
     lwd = 3, main = "")
curve(dt(x, n1 + n2 + n3 - 2), add = TRUE, col = "red", lwd = 2)
# theoretical null distribution



#Still on the ANOVA 3 population problem: 
#examine power (null is not true) under the setting where

#pop means are very different but the variance is large

#pop means are very similar and the variance is large
#pop means are very similar but the variance is very very small