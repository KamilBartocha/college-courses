library(leaps)
library(faraway)
library(car)
sur <- read.table("surgical.txt")
val <- read.table("validation.txt")
attach(sur)
View(sur)

summary(sur)
plot(sur)
cor(sur)

boxplot(blood)
plot(blood)
identify(blood) # => 28

boxplot(survival)
plot(survival)
identify(survival) # => 13

reg <- lm(survival ~ ., sur)
summary(reg) # blood, prognostic, enzyme 
plot(reg$residuals ~ reg$fit)
abline(h=0)
# We can see that we will need transform Y
# becouse varaince it rising
plot(reg$residuals ~ blood) # no
plot(reg$residuals ~ prognostic) # no
plot(reg$residuals ~ enzyme) # no(only little)
plot(reg$residuals ~ liver) # no

boxplot(reg$residuals)
qqnorm(reg$residuals)
qqline(reg$residuals) # looks bad

# sur[,-5] - all except 5 
sur1 <- cbind(sur[,-5], ls=log(survival))
plot(sur1)
cor(sur)
cor(sur1)

# reg1 - survival is transformed with log
reg1 <- lm(ls ~ ., sur1)
summary(reg1)
# Adjusted R-squared:  0.9701 - better
# enzyme prognostic blood -depend

plot(reg1$residuals ~ reg1$fit)
abline(h=0)

plot(reg1$residuals ~ blood)
plot(reg1$residuals ~ prognostic)
plot(reg1$residuals ~ enzyme)
plot(reg1$residuals ~ liver)

boxplot(reg1$residuals)
qqnorm(reg1$residuals)
qqline(reg1$residuals)
# nie jest źle choć ogony odstają,
# widać w boxplot i w qqnorm

shapiro.test(reg1$residuals)
# p-value = 0.002496 -> odrzucamy hipotezę o normlaności
plot(reg1$residuals ~ I(blood * prognostic))
plot(reg1$residuals ~ I(blood * enzyme))
plot(reg1$residuals ~ I(blood * liver))
plot(reg1$residuals ~ I(enzyme * prognostic))
plot(reg1$residuals ~ I(enzyme * liver))
plot(reg1$residuals ~ I(liver * prognostic))

# reg2 - no liver
reg2 <-lm(ls ~ .-liver, sur1)
summary(reg2) # its better
step(reg1, direction = "both")
# library leaps is needed

x <- model.frame(reg1)[,-1]
y <- sur1$ls
leaps(x, y, method="Cp") -> cp
Cpplot(cp)
# 123 is good choice

plot(reg2$residuals ~ reg2$fit) # looks good
plot(reg2$residuals ~ blood)
plot(reg2$residuals ~ prognostic)
plot(reg2$residuals ~ enzyme)

boxplot(reg2$residuals)
qqnorm(reg2$residuals)
qqline(reg2$residuals)

shapiro.test(reg2$residuals)
plot(reg2$residuals ~ I(blood * prognostic))
plot(reg2$residuals ~ I(blood * enzyme))
plot(reg2$residuals ~ I(enzyme * prognostic))

reg3 <- lm(ls ~ (blood + prognostic + enzyme)^2, sur1)
summary(reg3)
avPlots(reg1)
avPlots(reg2)
# VIF - Varianve Inflation Factor
# VIF > 10 => strong coleration
vif(reg2)
# dfbeta itd potrzebny
inf = influence.measures(reg2)
summary(inf)
pf(0.53, 4, 50)
# validation data - check
reg_val <- lm(log(survival) ~ .-liver, val) 
summary(reg_val)
summary(reg2)
avPlots(reg_val)
pred <- predict(reg2, val)
cbind(pred, log(val$survival))

sum((pred - log(val$survival))^2) / 54 # -> Mean Sq = 0.038
anova(reg2)                            # -> Mean Sq = 0.0117
