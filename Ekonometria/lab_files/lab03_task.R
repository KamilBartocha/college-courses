cafeteria <- read.table("cafeteria.txt")
View(cafeteria)
attach(cafeteria)
plot(sales~dispensers)
# wypukłe liniowe
reg<-lm(sales ~ dispensers)
summary(reg)
abline(reg)
plot(reg$res ~ dispensers)
cor(dispensers, dispensers^2)
dif<-dispensers-mean(dispensers)
cor(dif, dif^2)
# d
# d^2
reg<-lm(sales~d + I(d^2))
summary(reg)
plot(reg$res, d)

reg1<-lm(sales~d + I(d^2) + I(d^3))
summary(reg1)
plot(reg1$res, d)
anova(reg, reg1)
# nie warto dodać kolejnych potęg - nic nie wniosło

bata1 <- reg$coefficients[1]
beta2 <- reg$coefficients[2]
beta3 <- reg$coefficients[3]

mean <- mean(dispensers)
beta11 <- beta2 - 2* beta3 * mean
beta11

beta_0 <- (beta1 - beta2 * mean) + beta3*mean^2 
beta_0
#coś tu źle jest, złe wyniki

plot(sales~dif)
lines(d,reg$coef[1]+reg$coef[2]*d+reg$coef[3]*d^2, col="red")
