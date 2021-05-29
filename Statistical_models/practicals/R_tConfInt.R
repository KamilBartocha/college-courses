tstatistic <- function(x, y) {
  m <- length(x)
  n <- length(y)
  sp <- sqrt(((m - 1)*sd(x)^2 + (n - 1)*sd(y)^2)/(m + n - 2))
  t.stat <- (mean(x) - mean(y))/(sp*sqrt(1/m + 1/n))  
}

t.ci <- function(m, n, mu1, mu2, sig1, sig2, alpha) {
  x <- rnorm(m, mean = mu1, sd = sig1)
  y <- rnorm(n, mean = mu2, sd = sig2)
  xbar <- mean(x)
  ybar <- mean(y)
  delta <- mu1 - mu2
  delta.hat <- xbar - ybar
  sp <- sqrt(((m - 1)*sd(x)^2 + (n - 1)*sd(y)^2)/(m + n - 2))
  df <- length(x) + length(y) - 2
  tcrit <- qt(alpha/2, df = df, lower.tail = FALSE)
  ci.low <- delta.hat - tcrit*sp*sqrt(1/m + 1/n)
  ci.up <-  delta.hat + tcrit*sp*sqrt(1/m + 1/n)
  return(as.numeric(ci.low <= delta)*as.numeric(ci.up >= delta))
}


## Equal Means:
m <- 5
n <- 5
mu1 <- 0
mu2 <- 0
sig1 <- 1
sig2 <- 1
alpha <- 0.05

set.seed(1234)
ci.vector <- replicate(100, t.ci(m, n, mu1, mu2, sig1, sig2, alpha))
print(ci.vector[1])

print(ci.vector[1:10])
print(c(mean(ci.vector), sd(ci.vector)))


N <- 100
ci.values <- matrix(NA, nrow = N, ncol = 4)
set.seed(12419)
for(i in 1:N) {
  x <- rnorm(m, mean = mu1, sd = sig1)
  y <- rnorm(n, mean = mu2, sd = sig2)
  xbar <- mean(x)
  ybar <- mean(y)
  delta <- mu1 - mu2
  delta.hat <- xbar - ybar
  sp <- sqrt(((m - 1)*sd(x)^2 + (n - 1)*sd(y)^2)/(m + n - 2))
  df <- length(x) + length(y) - 2
  tcrit <- qt(alpha/2, df = df, lower.tail = FALSE)
  ci.values[i,1] <- delta.hat - tcrit*sp*sqrt(1/m + 1/n)
  ci.values[i,2] <- delta.hat + tcrit*sp*sqrt(1/m + 1/n)
  ci.values[i,3] <- as.numeric(ci.values[i,1] <= delta)*as.numeric(ci.values[i,2] >= delta)
  ci.values[i,4] <- delta.hat
}

ci.color <- c()
for (i in 1:N) {
  if (ci.values[i,3] == 0) {
    ci.color[i] <- "Red"
  } else {
    ci.color[i] <- "Black" 
  } 
}

require(plotrix)
plotCI(x = 1:N, 
       y = ci.values[,4], 
       li=ci.values[,1],
       ui=ci.values[,2], col = ci.color, lwd = 3, ylim=c(-4,4), ylab = "")
abline(h=0,lwd=3) 

print(sum(ci.values[,3])/N)
