library(nlme)

cmort <- ts(scan("cmort.dat"))
temp <- ts(scan("temp.dat"))
part <- ts(scan("part.dat"))

plot(cbind(cmort, temp, part))
pairs(cbind(cmort, temp, part))
plot(cmort~temp)

plot(temp)

trend <- 1:length(cmort)
reg1 <- lm(cmort ~ trend + temp+ part) 
summary(reg1)
plot(reg1$residuals ~ temp)

reg2 <- lm(cmort ~ trend + temp + part + I(temp^2))
summary(reg2)
plot(reg2$residuals ~ temp)
plot(reg2$residuals ~ part)

temp = temp - mean(temp)
temp2 = temp^2

reg <- lm(cmort ~ trend + temp + temp2 + part)
summary(reg)

plot.ts(reg$res)
acf(reg$res)
pacf(reg$res)

ar = arima(reg$residuals, order=c(2,0,0))
ar

tsdiag(ar)

?gls
fit.gls = gls(cmort~trend+temp+temp2+part, correlation=corARMA(p=2), method="ML")
fit.gls
summary(fit.gls)
# summary(reg)
w=filter(residuals(fit.gls),filter=c(1,-.3848530, -.4326282),sides=1)
w
w = w[3:508]
plot.ts(w)
acf(w)
