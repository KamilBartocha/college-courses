library(MASS)
library(nlme)

### set number of individuals
n <- 200

### average intercept and slope
beta0 <- 1.0
beta1 <- 6.0

### true autocorrelation
ar.val <- .4

### true error SD, intercept SD, slope SD, and intercept-slope cor
sigma <- 1.5
tau0  <- 2.5
tau1  <- 2.0
tau01 <- 0.3

### maximum number of possible observations
m <- 10

### simulate number of observations for each individual
set.seed(1234)
p <- round(runif(n,4,m))
p
sum(p)

### simulate observation moments (assume everybody has 1st obs)
obs <- unlist(sapply(p, function(x) 
  c(1, sort(sample(2:m, x-1, replace=FALSE)))))

### set up data frame
dat <- data.frame(id=rep(1:n, times=p), obs=obs)
head(dat)

### simulate (correlated) random effects for intercepts and slopes
mu  <- c(0,0)
R   <- matrix(c(1, tau01, tau01, 1), nrow=2)
tau <- c(tau0, tau1)
S   <- diag(tau) %*% R %*% diag(tau)
U   <- mvrnorm(n, mu=mu, Sigma=S)

### simulate AR(1) errors and then the actual outcomes
dat$eij <- unlist(sapply(p, function(x) 
  arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^2) * sigma))
dat$yij1 <- (beta0 + rep(U[,1], times=p))
dat$yij2 <- (beta1 + rep(U[,2], times=p)) * log(dat$obs)
dat$yij <- (beta0 + rep(U[,1], times=p)) + 
  (beta1 + rep(U[,2], times=p)) * log(dat$obs) + dat$eij

### note: use arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^2) * sigma
### construction, so that the true error SD is equal to sigma

### another set up
dat$eij <- unlist(sapply(p, function(x) 
  arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^2) * sigma))
dat$yij1 <- (beta0 + rep(U[,1], times=p))
dat$yij2 <- (beta1) * log(dat$obs)
dat$yij <- (beta0 + rep(U[,1], times=p)) + 
  (beta1) * log(dat$obs) + dat$eij



### create grouped data object
dat <- groupedData(yij ~ obs | id, data=dat)

### spaghetti plots
library(plyr)
library(ggplot2)
dat <- ddply(dat, .(id), function(x){
  x$alpha = ifelse(runif(n = 1) > 0.9, 1, 0.1)
  x$grouper = factor(rbinom(n=1, size =3 ,prob=0.5), levels=0:3)
  x
})

tspag <- ggplot(dat, aes(x=obs, y=yij)) + 
  geom_line() + guides(colour=FALSE) + 
  xlab("Observation Time Point") +
  ylab("Y") + 
  theme_bw()
spag <- tspag + aes(colour = factor(id))
spag

bwspag <- tspag + aes(alpha=alpha, group=factor(id)) + 
  guides(alpha=FALSE)
bwspag

spag + facet_wrap(~ grouper)

sspag <- spag + geom_smooth(se=FALSE, colour="black", size=2)
sspag

sspag + facet_wrap(~ grouper)

### fit corresponding growth model
res <- lme(yij ~ log(obs), 
           random = ~ log(obs) | id, correlation = corAR1(form = ~ 1 | id), 
           data=dat)
summary(res)

#############################################################
# Computerised Delivery of Cognitive 
# Behavioural Therapy-Beat the Blues

library(HSAUR)
attach(BtheB)

# Inspect the data
# A data frame with 100 observations of 100 patients 
# on the following 8 variables:

# drug - did the patient take anti-depressant drugs (No or Yes)
# length - the length of the current episode of depression, 
# a factor with levels <6m (less than six months) 
# and >6m (more than six months)
# treatment - treatment group, a factor with levels 
# TAU (treatment as usual) and BtheB (Beat the Blues)
# bdi.pre - Beck Depression Inventory II before treatment
# bdi.2m - Beck Depression Inventory II after two months
# bdi.4m - Beck Depression Inventory II after four months
# bdi.6m - Beck Depression Inventory II after six months
# bdi.8m - Beck Depression Inventory II after eight months.

BtheB$subject <- factor(rownames(BtheB))
nobs <- nrow(BtheB)

BtheB_long <- reshape(BtheB, idvar = "subject",
                      varying = c("bdi.2m", "bdi.4m", "bdi.6m", "bdi.8m"),
                      direction = "long")

BtheB_long$time <- rep(c(2, 4, 6, 8), rep(nobs, 4))
subset(BtheB_long, subject %in% c("1", "2", "3"))

layout(matrix(1:2, nrow = 1))
ylim <- range(BtheB[,grep("bdi", names(BtheB))], na.rm = TRUE)
tau <- subset(BtheB, treatment == "TAU")[,grep("bdi", names(BtheB))]
boxplot(tau, main = "Treated as usual", ylab = "BDI",
        xlab = "Time (in months)", names = c(0, 2, 4, 6, 8),
        ylim = ylim)
btheb <- subset(BtheB, treatment == "BtheB")[,grep("bdi", names(BtheB))]
boxplot(btheb, main = "Beat the Blues", ylab = "BDI",
        xlab = "Time (in months)", names = c(0, 2, 4, 6, 8),
        ylim = ylim)

library("lme4")
BtheB_lmer1 <- lmer(bdi ~ bdi.pre + time + treatment + drug +
                      length + (1 | subject), 
                    data = BtheB_long,
                    REML = FALSE, na.action = na.omit)
BtheB_lmer2 <- lmer(bdi ~ bdi.pre + time + treatment + drug +
                      length + (time | subject), 
                    data = BtheB_long,
                    REML = FALSE, na.action = na.omit)
anova(BtheB_lmer1, BtheB_lmer2)

summary(BtheB_lmer1)
summary(BtheB_lmer2)

##########################################################
str(sleepstudy)

# Inspect the data
# The average reaction time per day for subjects in a 
# sleep deprivation study. On day 0 the subjects had their 
# normal amount of sleep. Starting that night they were 
# restricted to 3 hours of sleep per night. The observations 
# represent the average reaction time on a series of tests 
# given each day to each subject.

# Reaction - Average reaction time (ms)
# Days - Number of days of sleep deprivation
# Subject - Subject number on which the observation was made.

require(lattice)
xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)", aspect = "xy")

# Model With Correlated Random Effects
model1 <- lmer(Reaction ~ 1 + Days + (1 + Days|Subject), 
               sleepstudy, REML = FALSE)
summary(model1)
head(ranef(model1)[["Subject"]])

# Interpretations:
# average initial reaction time (i.e. without sleep deprivation)
# in the population

# average increase in reaction time per day of sleep deprivation

# estimated subject-to-subject variation in the intercept 
# corresponds to a standard deviation of about 25 ms

# estimated subject-to-subject variation in the slope 
# corresponds to a standard deviation of about 6 ms/day

# little evidence of a systematic relationship 
# between these quantities; observing a subject's initial
# reaction time does not give us much information for 
# predicting whether their reaction time will be 
# strongly affected by each day of sleep deprivation or not



# Model With Uncorrelated Random Effects
model2 <- lmer(Reaction ~ 1 + Days + (1|Subject) + 
                 (0+Days|Subject), sleepstudy, REML = FALSE)
summary(model2)
head(ranef(model2)[["Subject"]])

anova(model1, model2)


# Other models:
# Random intercept with fixed mean without covariates
model3 <- lmer(Reaction ~ 1 + (1|Subject), sleepstudy, REML = FALSE)
summary(model3)

# Random intercept with fixed mean with covariates
model4 <- lmer(Reaction ~ 1 + Days + (1|Subject), sleepstudy, REML = FALSE)
summary(model4)
