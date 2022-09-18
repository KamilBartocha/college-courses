library(SemiPar)
data(janka)
View(janka)
# hardness dependent, dens independent
plot(janka$hardness~janka$dens)

reg<-lm(janka$hardnes~janka$dens)
summary(reg)
abline(reg)
plot(reg$res~reg$fit)
boxplot(reg$res)
qqnorm(reg$res)
qqline(reg$res)

# data is not linear, we need to make transformation
# Y' = sqrt(Y) or Y' = log(Y), we choose log

plot(log(janka$hardness)~janka$dens)
reg_logY<-lm(log(janka$hardness)~janka$dens)
summary(reg_logY)
plot(reg_logY$res~reg_logY$fit)
abline(h=0)
boxplot(reg_logY$res)
qqnorm(reg_logY$res)
qqline(reg_logY$res)

# we need to make transformation of X, coz we can see
# that data looks like log
# X' = sqrt(X) or X' = log(X), we choose log

plot(log(janka$hardness)~log(janka$dens))
reg_logXY<-lm(log(janka$hardness)~log(janka$dens))
summary(reg_logXY)
plot(reg_logXY$res~reg_logXY$fit)
abline(h=0)
boxplot(reg_logXY$res)
qqnorm(reg_logXY$res)
qqline(reg_logXY$res)

# Shapiro-Wilk test
shapiro.test(reg_logXY$res)
#p-value = 0.5177 