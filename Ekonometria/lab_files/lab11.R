recruit <- ts(scan("recruit.txt"),f=12, start =1950)
plot(recruit)
acf(recruit, 50)
pacf(recruit)
# ucięte po 2 -> AR2
?ar
ols= ar(recruit, alc=F, order.mas=2, dmean=2, intercept=T, method="ols")
ols
yw = ar(recruit, order=2, method="yw")
yw
pred = predict(yw, n.ahead=24)
plot(recruit,xlim=c(1980,1990))
lines(pred$pred, col="red")

lines(pred$pred+1.64*pred$se,col="red",lty="dashed")
lines(pred$pred-1.64*pred$se,col="red",lty="dashed")
