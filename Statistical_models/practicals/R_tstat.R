tstatistic <- function(x, y) {
  m <- length(x)
  n <- length(y)
  sp <- sqrt(((m - 1)*sd(x)^2 + (n - 1)*sd(y)^2)/(m + n - 2))
  t.stat <- (mean(x) - mean(y))/(sp*sqrt(1/m + 1/n))  
}

data.x <- c(1,4,3,6,5)
data.y <- c(5,4,7,6,10)
print(tstatistic(data.x, data.y))

x <- rnorm(10, mean = 50, sd = 10)
y <- rnorm(10, mean = 50, sd = 10)


mc_tstat <- function(alpha, N, pars, seed) {
  set.seed(seed)
  n.reject <- 0
  
  for (i in 1:N) {
    x <- rnorm(pars$m, pars$mu1, pars$sigma1)
    y <- rnorm(pars$n, pars$mu2, pars$sigma2)
    t.stat <- tstatistic(x, y)
    if (abs(t.stat) > qt(1 - alpha/2, pars$n + pars$m - 2)) 
    {n.reject <-  n.reject + 1}
    est.sig.level <- n.reject / N
  }
  
  print(est.sig.level)
}

## Case 1: Normal populations with zero means and equal spreads
pars <- list(m = 10, n = 10, mu1 = 0, mu2 = 0.2, sigma1 = 0.1, sigma2 = 0.1)
mc_tstat(alpha = 0.1, N = 10000, pars, seed = 1234)

pars <- list(m = 100, n = 100, mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 1)
mc_tstat(alpha = 0.1, N = 10000, pars, seed = 1234)


## Case 2: Normal populations with zero means and very different spreads
pars <- list(m = 10, n = 10, mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 10)
mc_tstat(alpha = 0.1, N = 10000, pars, seed = 1234)

pars <- list(m = 100, n = 100, mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 10)
mc_tstat(alpha = 0.1, N = 10000, pars, seed = 1234)

## Case 3: t populations, 4 df and equal spreads
t_tstat <- function(alpha, N, pars, seed) {
  set.seed(seed)
  n.reject <- 0
  
  for (i in 1:N) {
    x <- rt(pars$m, pars$df)
    y <- rt(pars$n, pars$df)
    t.stat <- tstatistic(x, y)
    if (abs(t.stat) > qt(1 - alpha/2, pars$n + pars$m - 2)) 
    {n.reject <-  n.reject + 1}
    est.sig.level <- n.reject / N
  }
  
  print(est.sig.level)
}

pars <- list(m = 10, n = 10, df = 4)
t_tstat(alpha = 0.1, N = 10000, pars, seed = 1234)

## Case 4: Exponential populations, equal rates
exp_tstat <- function(alpha, N, pars, seed) {
  set.seed(seed)
  n.reject <- 0
  
  for (i in 1:N) {
    x <- rexp(pars$m, pars$rate)
    y <- rexp(pars$n, pars$rate)
    t.stat <- tstatistic(x, y)
    if (abs(t.stat) > qt(1 - alpha/2, pars$n + pars$m - 2)) 
    {n.reject <-  n.reject + 1}
    est.sig.level <- n.reject / N
  }
  
  print(est.sig.level)
}

pars <- list(m = 10, n = 10, rate = 1)
exp_tstat(alpha = 0.1, N = 10000, pars, seed = 1234)


## Case 5: One normal population, one exponential population
mix_tstat <- function(alpha, N, pars, seed) {
  set.seed(seed)
  n.reject <- 0
  
  for (i in 1:N) {
    x <- rnorm(pars$m, pars$mu, pars$sigma)
    y <- rexp(pars$n, pars$rate)
    t.stat <- tstatistic(x, y)
    if (abs(t.stat) > qt(1 - alpha/2, pars$n + pars$m - 2)) 
    {n.reject <-  n.reject + 1}
    est.sig.level <- n.reject / N
  }
  
  print(est.sig.level)
}

pars <- list(m = 10, n = 10, mu = 10, sigma = 2, rate = 1/10)
mix_tstat(alpha = 0.1, N = 10000, pars, seed = 1234)


## Exact sampling distribution for Case 5
m <- 10
n <- 10
t.simulation <- function() {
  tstatistic(rnorm(m, mean = 10, sd = 2), rexp(n, rate = 1/10))
}

tstat.vector <- replicate(10000, t.simulation()) 
plot(density(tstat.vector), xlim = c(-5, 8), ylim = c(0, 0.4), lwd = 3, main = "")
curve(dt(x, df = 18), add = TRUE)
legend(4, 0.4, c("exact", "t(18)"), lwd = c(3, 1))


## Critical value

cval_plot <- function(x, y, alpha, ts) {
  df <- length(x) + length(y) - 2
  points <- seq(-4, 4, length = 10000)
  ql <- qt(alpha/2, df = df, lower.tail = TRUE)
  qu <- qt(alpha/2, df = df, lower.tail = FALSE)
  
  cord.x <- c(c(qu, seq(qu, 4, 0.001), 4), 
              c(-4, seq(-4, ql, 0.001), ql)) 
  cord.y <- c(c(0, dt(seq(qu, 4, 0.001), df = df), 0), 
              c(0, dt(seq(-4, ql, 0.001), df = df), 0)) 
  
  plot(points, dt(points, df = df), type = 'l', ylab = "")
  polygon(cord.x, cord.y, col = 'skyblue')
  abline(v = ts)
}

data.x <- c(1,4,3,6,5)
data.y <- c(5,4,7,6,10)
cval_plot(data.x, data.y, 0.05, tstatistic(data.x, data.y))
pval_plot(data.x, data.y)

x <- rnorm(10, mean = 50, sd = 10)
y <- rnorm(10, mean = 50, sd = 10)
pval_plot(x, y)

set.seed(1234)
x <- rnorm(10, mean = 50, sd = 10)
y <- rnorm(10, mean = 55, sd = 10)
pval_plot(x, y)


## P-value

pval_plot <- function(x, y) {
  df <- length(x) + length(y) - 2
  ts <- abs(tstatistic(x, y))
  points <- seq(-4, 4, length = 10000)
  
  cord.x <- c(c(ts, seq(ts, 4, 0.001), 4), 
              c(-1*ts, seq(-4, -1*ts, 0.001), -1*ts)) 
  cord.y <- c(c(0, dt(seq(ts, 4, 0.001), df = df), 0), 
              c(0, dt(seq(-4, -1*ts, 0.001), df = df), 0)) 
  
  plot(points, dt(points, df = df), type = 'l', ylab = "")
  polygon(cord.x, cord.y, col = 'red')
  print(paste("test stat is ", round(ts,4), sep=""))
  print(paste("p-value is ", round(2*(1 - pt(ts, df = df)),4), sep = ""))
}

data.x <- c(1,4,3,6,5)
data.y <- c(5,4,7,6,10)
pval_plot(data.x, data.y)

x <- rnorm(10, mean = 50, sd = 10)
y <- rnorm(10, mean = 50, sd = 10)
pval_plot(x, y)

set.seed(1234)
x <- rnorm(10, mean = 50, sd = 10)
y <- rnorm(10, mean = 55, sd = 10)
pval_plot(x, y)
