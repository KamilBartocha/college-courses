library(TSA)

data(ar1.s)
plot(ar1.s)

x <- ar1.s
order.max <- 1
x <- as.vector(x) 
n.used <- length(x)

coefs <- matrix(NA, order.max+1L, order.max+1L)
var.pred <- numeric(order.max+1L)
xaic <- numeric(order.max+1L)
xm <- mean(x)

coefs[1, 1L] <- xm
var0 <- sum((x-xm)^2)/n.used
var.pred[1L] <- var0
xaic[1L] <- n.used * log(var0) + 2 * 1 + 2 + n.used + 
  n.used * log(2 * pi)

for(i in seq_len(order.max)) {
  fit <- arima0(x, order=c(i, 0L, 0L), include.mean=TRUE)
  coefs[i+1L, seq_len(i+1)] <- fit$coef[seq_len(i+1)]
  xaic[i+1L] <- fit$aic
  var.pred[i+1L] <- fit$sigma2
}

xaic <- setNames(xaic - min(xaic), 0L:order.max)
order <- (0L:order.max)[xaic == 0L]
ar <- coefs[order+1L, seq_len(order)]
x.mean <- coefs[order+1L, order+1L]
var.pred <- var.pred[order+1L]

resid <- c(rep(NA, order), embed(x - x.mean, order+1L) %*% c(1, -ar))
res <- list(order = order, ar = ar, var.pred = var.pred,
            x.mean = x.mean, aic = xaic,
            n.used = n.used, order.max = order.max)
res



# A simulated AR(1) series 
# The true parameter value here is phi = 0.9:

ar(ar1.s,order.max=1,aic=F,method='yw')  # method of moments
ar(ar1.s,order.max=1,aic=F,method='ols') # conditional sum of squares
ar(ar1.s,order.max=1,aic=F,method='mle') # maximum likelihood

# Another simulated AR(1) series 

yt <- arima.sim(list(order=c(1,0,0), ar=0.5), n=500)
plot(yt, type = "l")
ar(yt,order.max=1,aic=T,method='mle')

y <- arima.sim(list(order = c(1,0,0), ar = 1), n = 500)


# Another simulated AR(1) series (The true parameter value here is phi = 0.4):
data(ar1.2.s)
ar(ar1.2.s,order.max=1,aic=F,method='yw')  # method of moments
ar(ar1.2.s,order.max=1,aic=F,method='ols') # conditional sum of squares
ar(ar1.2.s,order.max=1,aic=F,method='mle') # maximum likelihood


# A simulated AR(2) series (the true parameter values here are 
# phi_1 = 1.5 and phi_2 = -0.75):

data(ar2.s)
ar(ar2.s,order.max=2,aic=F,method='yw')  # method of moments
ar(ar2.s,order.max=2,aic=F,method='ols') # conditional sum of squares
ar(ar2.s,order.max=2,aic=F,method='mle') # maximum likelihood

# How do these methods compare with a smaller sample size?

ar(ar2.s[1:20],order.max=2,aic=F,method='yw')  # method of moments
ar(ar2.s[1:20],order.max=2,aic=F,method='ols') # conditional sum of squares
ar(ar2.s[1:20],order.max=2,aic=F,method='mle') # maximum likelihood

# For this example, MOM appears less accurate with the smaller sample size...

# Estimation of Color property data, based on an AR(1) model
data(color)
plot(color)

ar(color,order.max=1,aic=F,method='yw')  # method of moments
ar(color,order.max=1,aic=F,method='ols') # conditional sum of squares
ar(color,order.max=1,aic=F,method='mle') # maximum likelihood

# The ML estimate of phi is 0.57.  
# The estimates using the other methods are fairly similar.


# Shumway-Stoffer examples:
library(astsa)

#Recruitment Data
rec.yw <- ar.yw(rec, order=1)
rec.yw$x.mean  # = 62.26278 (mean estimate)
rec.yw$ar      
sqrt(diag(rec.yw$asy.var.coef))
rec.yw$var.pred  

# ML estimates here for the recruitment data:
arima(rec, order=c(1,0,0), method='ML')
ar(rec, method = 'mle')


# The U.S. GNP data:
plot(gnp)
acf2(gnp,50)
gnpgr <- diff(log(gnp))
plot(gnpgr)
acf2(gnpgr)

arima(gnpgr,order=c(1,0,0),method='ML') # fits an AR(1) model using ML
ar.gnpgr <- ar(gnpgr, order.max = 1, method = 'mle') 
ar.gnpgr

# Some large sample CIs:
alpha <- 0.05

# Directly using outputted standard error from R:
phi.hat <- ar.gnpgr$ar
se <- sqrt(ar.gnpgr$asy.var.coef)
lower <- phi.hat - qnorm(1 - alpha/2)*se 
upper <- phi.hat + qnorm(1 - alpha/2)*se
