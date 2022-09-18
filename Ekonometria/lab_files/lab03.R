library(car)
library(ellipse)
zarthan <- read.table("zarthan.txt")
View(zarthan)
attach(zarthan)

plot(zarthan)
cor(zarthan)
summary(zarthan)

#~. - all data
# or reg <- lm(sales~population + income, zarthan)
reg <- lm(sales~., zarthan)
summary(reg)
plot(reg$res ~ reg$fit)
plot(reg$res ~ population)
plot(reg$res ~ income)

boxplot(reg$res)
qqnorm(reg$res)
qqline(reg$res)
avPlots(reg)

# wykres zależności reszt od składnika interakcji
plot(reg$res~I(population*income))
reg2<-lm(sales~. + I(population*income), zarthan) 
summary(reg2)
anova(reg2)


reg0<-lm(sales~1,zarthan)
anova(reg0,reg)

# A - przedział ufności pokrywa \beta_1
# B - przedział ufności pokrywa \beta_2
# chcemy P(a iloczyn b) >= 1-\alfa, rozpisując wychodzi
# \alpha_0 = \frac{\alpha}{2}

?confint
confint(reg,c("population","income"),level=0.95)
plot(ellipse(reg,c("population","income"),level=0.9),type="l")
rect(0.482813482,0.007089742,0.50919647,0.01130842)
?predict
predict(reg,data.frame(population=220,income=2500),interval="confidence",level=0.95)
predict(reg,data.frame(population=c(220,375),income=c(2500,3500)),
        interval="prediction",level=0.95)
