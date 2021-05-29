bin.ci <- function(N, n, p, alpha) {
  y <- rbinom(N, size = n, prob = p)
  p.hat <- y/n
  z <- qnorm(1 - (alpha/2))
  se <- sqrt(p.hat*(1 - p.hat)/n)
  ci.low <- p.hat - z*se
  ci.up <- p.hat + z*se
  return(as.numeric(ci.low <= p)*as.numeric(ci.up >= p))
}


## True probability of success is 0.5:
set.seed(12419)
ci.simulation <- function() {
  sum(bin.ci(N = 20, n = 20, p = 0.50, alpha = 0.10))/20
}

ci.vector <- replicate(10000, ci.simulation())
print(ci.vector[1])

print(ci.vector[1:10])
print(c(mean(ci.vector), sd(ci.vector)))


## True probability of success is 0.05:
set.seed(12419)
ci.simulation <- function() {
  sum(bin.ci(N = 20, n = 20, p = 0.05, alpha = 0.10))/20
}

ci.vector <- replicate(10000, ci.simulation())
print(ci.vector[1])

print(ci.vector[1:10])
print(c(mean(ci.vector), sd(ci.vector)))
