gnp96 <- read.table("gnp96.txt")
gnp <- ts(gnp96$V2, start=1947, f=4)
plot(gnp)
# rosnący ->różnicujemy
dgnp = diff(gnp)
plot(dif)
abline(h=0)
ddgnp = diff(dgnp)
plot(ddif)
abline(h=0)
lgnp <- log(gnp)
dlgnp = diff(lgnp)
plot(dlgnp)

par(mfrow=c(1,1))
acf(dlgnp)
pacf(dlgnp)

?arima
gnp.ar<-arima(dlgnp,order=c(1,0,0))
gnp.ar

gnp.ma<-arima(dlgnp,order=c(0,0,2))
gnp.ma

acf(gnp.ar$res,100)
?Box.test
Box.test(gnp.ar$res, type="Ljung")
qqnorm(gnp.ar$res); qqline(gnp.ar$res)

Box.test(gnp.ar$res, 10, type="Ljung")
qqnorm(gnp.ar$res); qqline(gnp.ar$res)
tsdiag(gnp.ar)
tsdiag(gnp.ma)

gnp.arma<-arima(dlgnp,order=c(1,0,2))
tsdiag(gnp.arma)
AIC(gnp.ar); AIC(gnp.ma); AIC(gnp.arma);

log(gnp.ma$sigma2)+(222+2)/(222-2-2)
log(gnp.ar$sigma2)+(222+1)/(222-1-2)
log(gnp.arma$sigma2)+(222+2)/(222-2-2)

log(gnp.ma$sigma2)+(2*log(222)/222)
log(gnp.ar$sigma2)+(1*log(222)/222)
log(gnp.arma$sigma2)+(2*log(222)/222)

# task3

prod <- ts(scan("prod.txt"),f=12, start=1948)
plot(prod)
dprod <- diff(prod)
plot(dprod)
acf(dprod, 100)
ddprod <- diff(dprod, lag=12)
plot(ddprod)
par(mfrow=c(2,1))
acf(ddprod, 100)
pacf(ddprod, 100)

?arima
prod1 = arima(prod,order=c(1,1,1),
            seasonal=list(order=c(0,1,1),period=12))
prod1
tsdiag(prod1)

prod2 = arima(prod,order=c(1,1,1),
            seasonal=list(order=c(2,1,0),period=12))
tsdiag(prod2)

prod3 = arima(prod,order=c(4,1,0),
            seasonal=list(order=c(2,1,0),period=12))
tsdiag(prod3)

prod4 = arima(prod,order=c(1,1,1),
              seasonal=list(order=c(4,1,1),period=12))
prod4

# c(1,1,1) c=(4,4,1) the best result
tsdiag(prod4)

pred = predict(prod4, n.ahead=24)
pred
par(mfrow=c(1,1))
plot(prod, xlim=c(1970,1981), ylim=c(80, 180))

lines(pred$pred,col="red")
lines(pred$pred+1.64*pred$se,col="red",lty="dashed")
lines(pred$pred-1.64*pred$se,col="red",lty="dashed")
