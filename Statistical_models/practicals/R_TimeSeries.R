# Practicals 15 May 2021:

# - estimating the parameters of an AR(1) model 
#   using conditional log likelihood or conditional least squares 
# - how to obtain the inverse and the Choleski of a symmetric
#   positive definite matrix in R
# - longitudinal data analysis/models




################################################################
# How to read data from external files into R and save them as a 
# time series (ts) object?

# Simple but tedious: typing data as a vector 
# (only useful for very short series):

data <- c(12,31,22,24,30)

ts.data <- ts(data, start=c(2006,2), frequency=4)
print(ts.data)  
time(ts.data) 

LifeExp <- read.table('LifeExpectancy.txt', header=F)
y <- LifeExp[,2]
length(y)

y.ts <- ts(y, start = 1816, frequency = 1)
print(y.ts)
time(y.ts)

x.ts <- ts(y, start = c(1970,1), frequency = 4)
print(x.ts)
time(x.ts)

w.ts <- ts(y, start = c(2000,1), frequency = 12)
print(w.ts)
time(w.ts)

w <- rnorm(500,0,1)
plot.ts(w, main="white noise")

# Shumway-Stoffer Examples:
library(astsa)
par(mfrow = c(2,1))
plot(soi, ylab="", xlab="", main="Southern Oscillation Index")
plot(rec, ylab="", xlab="", main="Recruitment")

par(mfrow=c(2,1))
ts.plot(fmri1[,2:5], col=1:4, ylab="BOLD", main="Cortex")
ts.plot(fmri1[,6:9], col=1:4, ylab="BOLD", main="Thalamus & Cerebellum")

par(mfrow=c(2,1))
plot(EQ5, main="Earthquake")
plot(EXP6, main="Explosion")
