
varve <- ts(scan("varve.dat"),f=12, start =1950)
plot(varve)
dvarve <- diff(varve)
plot(dvarve)

lvarve <- log(varve)

dlvarve <- diff(lvarve)
plot(dlvarve)

par(mfrow=c(2,1))
acf(dlvarve, 100)
pacf(dlvarve, 100)

varve1 = arima(dlvarve, order=c(0,0,1))
varve1
tsdiag(varve1)

varve2 = arima(dlvarve, order=c(1,0,1))
varve2
tsdiag(varve2)

gas <- ts(scan("gas.dat"),f=12, start=1973)
plot(gas)
dgas <- diff(gas)
plot(dgas)

lgas <- log(gas)
plot(lgas)

ldgas <- diff(lgas)
plot(ldgas)

par(mfrow=c(2,1))
acf(ldgas, 100)
pacf(ldgas, 100)

gasArima = arima(dlvarve, order=c(0,0,1))

# file itall.R Functions sarima 
sarima(lgas, 0 ,1 ,1)
sarima(lgas, 3 ,1 ,0)
sarima(lgas, 3 ,1 ,0)
sarima(lgas, 1 ,1 ,1)
sarima(lgas, 2 ,1 ,0)
sarima(lgas, 2 ,1 ,1)
sarima(lgas, 3 ,1 ,1)

sarima.for
sarima.for(lgas, nahead=12, 3, 1, 0)
