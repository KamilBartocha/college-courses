#B. Poisson regression 

# Set working directory
cuse <- read.csv("Contraceptive.csv", header = TRUE, sep=",")

# Inspect the data
# distribution of 1607 currently married and fecund women interviewed 
# in the Fiji Fertility Survey, according to age, education, 
# desire for more children and current use of contraception.

head(cuse)
table(cuse$age)

cuse$want <- ifelse(cuse$wantsMore == "yes", 1, 0)
wantfit <- glm(want ~ age + education, data = cuse, family=binomial()) 
summary(wantfit)

exp(coef(wantfit))
confint(wantfit)
exp(confint(wantfit))

predict(wantfit, type="response")
residuals(wantfit, type="deviance")
plot(residuals(wantfit, type="deviance"))


# New Response
cuse$wantsMore_copy <- NA
cuse$wantsMore_copy <- cuse$wantsMore
cuse$wantsMore_copy <- ifelse(cuse$wantsMore_copy == "yes", 1, 0)
usingfit <- glm(cbind(using, notUsing) ~ age + education + wantsMore_copy,
                data=cuse, family=binomial()) 
summary(usingfit)
confint(usingfit)

# Interpretation:
# For each unit change in [insert predictor variable], the 
# log odds of [achieving the outcome of interest] increases by 
# [coefficient].

# Interpretation:
# For each unit ["increase" if OR is positive/
# "decrease" if OR is negative], 
# the odds of [achieving the outcome of interest vs. not] 
# increases/decreases by a factor of [that predictor variable's OR].

exp(coef(usingfit))
exp(confint(usingfit)) 
predict(usingfit, type="response") 
residuals(usingfit, type="deviance")
plot(residuals(usingfit, type="deviance")) 

Intusingfit <- glm(cbind(using, notUsing) ~ age*education,data=cuse,family=binomial()) 
summary(Intusingfit) 
confint(Intusingfit)
exp(coef(Intusingfit))
exp(confint(Intusingfit)) 

predict(Intusingfit, type="response")
residuals(Intusingfit, type="deviance")

anova(Intusingfit, test="Chisq") 

AgeWantfit <- glm(cbind(using,notUsing)~
                    age*wantsMore_copy,data=cuse,
                  family=binomial())
summary(AgeWantfit)

library(effects)
plot(effect("age:wantsMore_copy",
            AgeWantfit,
            xlevels = list(age=4, wantsMore_copy=c(1,0))),
     rug=F,
     multiline=T,
     main="Age by Wants More Interaction \n to Predict Using Contraceptives",
     ylab="Contraceptive Use",
     xlab="Age")

# Interpretation: 
# We see that, in general across both groups, the older women are, the more likely they are to use contraceptives. 
# However, we see an Group by Age interaction. 
# The slope is much steeper for women who DON'T want more kids: 
# they are markedly more likely to use contraceptives than 
# women who DO want more kids above. 
# Women who DO want more kids experience less of an impact of age 
# on their contraceptive use than do women who DON'T want more kids.


######################################################################
wantfit <- glm(using ~ age + education, data = cuse, family=poisson()) 
summary(wantfit)

wantfit <- glm(using ~ age * education, data = cuse, family=poisson()) 
summary(wantfit)

beta0hat <- summary(wantfit)$coef[1,1]
beta11hat <- summary(wantfit)$coef[2,1]
beta12hat <- summary(wantfit)$coef[3,1]
beta13hat <- summary(wantfit)$coef[4,1]
beta2hat <- summary(wantfit)$coef[5,1]

effCoef(wantfit)
print(c(beta0hat, beta11hat, beta12hat, beta13hat, beta2hat))
library(effects)
plot(allEffects(wantfit))
exp(confint(wantfit))

# Inspect the data
# The data set consists of counts of high school students 
# diagnosed with an infectious disease within a period of days 
# from an initial outbreak.

cases <- structure(list(Days = c(1L, 2L, 3L, 3L, 4L, 4L, 4L, 6L, 7L, 8L, 
                          8L, 8L, 8L, 12L, 14L, 15L, 17L, 17L, 17L, 18L, 19L, 19L, 20L, 
                          23L, 23L, 23L, 24L, 24L, 25L, 26L, 27L, 28L, 29L, 34L, 36L, 36L, 
                          42L, 42L, 43L, 43L, 44L, 44L, 44L, 44L, 45L, 46L, 48L, 48L, 49L, 
                          49L, 53L, 53L, 53L, 54L, 55L, 56L, 56L, 58L, 60L, 63L, 65L, 67L, 
                          67L, 68L, 71L, 71L, 72L, 72L, 72L, 73L, 74L, 74L, 74L, 75L, 75L, 
                          80L, 81L, 81L, 81L, 81L, 88L, 88L, 90L, 93L, 93L, 94L, 95L, 95L, 
                          95L, 96L, 96L, 97L, 98L, 100L, 101L, 102L, 103L, 104L, 105L, 
                          106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 114L, 115L), 
                 Students = c(6L, 8L, 12L, 9L, 3L, 3L, 11L, 5L, 7L, 3L, 8L, 
                              4L, 6L, 8L, 3L, 6L, 3L, 2L, 2L, 6L, 3L, 7L, 7L, 2L, 2L, 8L, 
                              3L, 6L, 5L, 7L, 6L, 4L, 4L, 3L, 3L, 5L, 3L, 3L, 3L, 5L, 3L, 
                              5L, 6L, 3L, 3L, 3L, 3L, 2L, 3L, 1L, 3L, 3L, 5L, 4L, 4L, 3L, 
                              5L, 4L, 3L, 5L, 3L, 4L, 2L, 3L, 3L, 1L, 3L, 2L, 5L, 4L, 3L, 
                              0L, 3L, 3L, 4L, 0L, 3L, 3L, 4L, 0L, 2L, 2L, 1L, 1L, 2L, 0L, 
                              2L, 1L, 1L, 0L, 0L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 0L, 0L, 
                              0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L)), .Names = c("Days", "Students"
                              ), class = "data.frame", row.names = c(NA, -109L))

attach(cases)
plot(Days, Students, xlab = "Days", ylab = "Students", pch = 16)
title(main = "Spread of Teenage Disease Post-Outbreak", sub = NULL,
      line = NA, outer = FALSE) 

# Fit the model
PoissonModel <- glm(Students ~ Days, family = poisson)
summary(PoissonModel)
exp(coef(PoissonModel))
exp(confint(PoissonModel))

# Interpretation:The expected log count for each unit 
# increase/decrease (depending on the sign of the coefficient) 
# in [outcome variable] given [predictor variable] is [coefficient].