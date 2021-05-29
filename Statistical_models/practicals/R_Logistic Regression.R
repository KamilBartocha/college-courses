
## Oct 1 2018
## Advanced lecture on logistic regression

setwd("/Users/ombaohc/Dropbox/KAUST-CLASS/KAUST-CLASS-DROPBOX/temp")

############################################################################
############ Example 1: Space Shuttle Challenger Data
############################################################################
### Website
### http://courses.ncssm.edu/math/Stat_Inst/PDFS/REG3_LOG.pdf

x = c(66,70,69,68,67,72,73,
70,57,63,70,78,67,53,67,
75,70,81,76,79,75,76,58)

y = c(0,1,0,0,0,0,0,
0,1,1,1,0,0,1,0,
0,0,0,0,0,1,0,1);

par(mfrow=c(1,1));
plot(x, y, xlab="Temperature", ylab="O-ring Failure");
title(paste("Space Shuttle Challenger Data"));

#########################################
### FITTING MODELS ######################
### Logistic regression model
fit1 = glm(y ~ x, family=binomial(link="logit"));
summary(fit1);
fit1$coef

b0hat = fit1$coef[1]
b1hat = fit1$coef[2]
b0hat
b1hat

b0hat = fit1$coef[1][[1]]
b1hat = fit1$coef[2][[1]]
b0hat
b1hat

xsort = sort(x)
model.est = (exp(b0hat + b1hat*xsort))/(1 + exp(b0hat + b1hat*xsort));
par(mfrow=c(1,1));
plot(x, y, xlab="Temperature", ylab="O-ring Failure");
title(paste("Space Shuttle Challenger Data"));
lines(xsort, model.est, col=2)

## est prob failure when x=37
xval = 37;
(exp(b0hat + b1hat*xval))/(1 + exp(b0hat + b1hat*xval))

## est log odds of failure when x=37
b0hat + b1hat*xval

## 95% confidence interval for the log odds of failure 
## at temp x = 70
xval = 50

log.odds.est = b0hat + b1hat*xval
log.odds.est

betahat = c(b0hat, b1hat)
c.x = t(c(1, xval))
log.odds.est = c.x%*%betahat

var.bhat = vcov(fit1)
se = sqrt(c.x%*%var.bhat%*%t(c.x))

log.lo = log.odds.est - 2*se
log.hi = log.odds.est + 2*se

log.lo
log.hi

log.lo = log.lo[[1]]
log.hi = log.hi[[1]]
cbind(log.lo, log.hi)

# 95% confidence interval for the prob 
prob.lo = exp(log.lo)/(1 + exp(log.lo))
prob.hi = exp(log.hi)/(1 + exp(log.hi))

cbind(prob.lo, prob.hi)

###########################################


#########################################################################
##################### Example 2. Beetle Mortality Data 
#########################################################################

dose = c(1.69,1.72,1.75,1.78,1.81,1.84,1.86,1.88);
ntot = c(59,60,62,56,63,59,62,60);
ytot = c(6,13,18,28,52,53,61,60);
propest = ytot/ntot;
par(mfrow=c(1,1))
plot(x=dose, y = propest); 
title(paste('Sample probabilities'));

par(mfrow=c(1,1))
plot(x=dose, y = (log((propest)/(1 - propest)))); 
title(paste('Sample log odds'));

#########################################
### FITTING MODELS ######################

### Enter the data
x = c(rep(dose[1], length=ntot[1]), rep(dose[2], length=ntot[2]), rep(dose[3], length=ntot[3]), 
rep(dose[4], length=ntot[4]), rep(dose[5], length=ntot[5]), rep(dose[6], length=ntot[6]),
rep(dose[7], length=ntot[7]), rep(dose[8], length=ntot[8]));

y = c(rep(1, length=ytot[1]), rep(0, length=(ntot[1] - ytot[1])), 
rep(1, length=ytot[2]), rep(0, length=(ntot[2] - ytot[2])),
rep(1, length=ytot[3]), rep(0, length=(ntot[3] - ytot[3])),
rep(1, length=ytot[4]), rep(0, length=(ntot[4] - ytot[4])),
rep(1, length=ytot[5]), rep(0, length=(ntot[5] - ytot[5])),
rep(1, length=ytot[6]), rep(0, length=(ntot[6] - ytot[6])),
rep(1, length=ytot[7]), rep(0, length=(ntot[7] - ytot[7])),
rep(1, length=ytot[8]), rep(0, length=(ntot[8] - ytot[8])));

cbind(x,y)

##### Fit the logistic regression model
fit1 = glm(y ~ x, family=binomial(link="logit"));
out1 = summary(fit1);
out1$coef
out1$cov.scaled

b0hat = c(out1$coef[1])
b1hat = c(out1$coef[2])
xsort=sort(x)
model.est = (exp(b0hat + b1hat*xsort))/(1 + exp(b0hat + b1hat*xsort));

par(mfrow=c(1,1))
plot(x=dose, y = propest); 
title(paste('Sample probabilities'));
lines(xsort, model.est, col=2)

#### 95% confidence interval for P(death at x=1.8)
xval = 1.8

(exp(b0hat + b1hat*xval))/(1 + exp(b0hat + b1hat*xval))

## est log odds of death when dose is xval = 1.8
b0hat + b1hat*xval

## 95% confidence interval for the log odds of death 
## at dose xval
xval = 1.8

log.odds.est = b0hat + b1hat*xval
log.odds.est

betahat = out1$coef
c.70 = t(c(1, xval))
log.odds.est = c.70%*%betahat

var.bhat = vcov(out1)
se = c(sqrt(c.70%*%var.bhat%*%t(c.70)))

log.lo = log.odds.est - 1.96*se
log.hi = log.odds.est + 1.96*se
log.lo = log.lo[[1]]
log.hi = log.hi[[1]]
cbind(log.lo, log.hi)

# 95% confidence interval for the prob 
prob.lo = exp(log.lo)/(1 + exp(log.lo))
prob.hi = exp(log.hi)/(1 + exp(log.hi))
cbind(prob.lo, prob.hi)







#############################################







