w <- rnorm(500,0,1)
plot.ts(w)
acf(w)
v<-filter(w,side=2,c(1/3,1/3,1/3))
x<-filter(w,method="recursive",c(1,-0.9))

w<-rnorm(200,0,1)
x=cumsum(w)
xd<-cumsum(w+0.2)
plot.ts(xd)
lines(x,col="blue")
abline(0,0.2,lty="dashed",col="red")

dx<-diff(x)

w<-rnorm(500,0,1); t<-1:500
s<-2*cos(2*pi*t/50+0.6*pi)
s1<-s+w; s5<-s+5*w
par(mfrow=c(3,1))
plot.ts(s)
plot.ts(s1)
plot.ts(s5)

st<-s1+log(t)
jj<-ts(scan("jj.dat"),frequency=4,start=c(1960,1))
print(jj,calendar=T)
plot.ts(jj)
ljj<-log(jj)
k<-c(1,2,2,2,1)
k<-k/sum(k)
fjj<-filter(ljj,k,sides=2)
lines(fjj,col="red")
lines(lowess(ljj),lty="dashed",col="blue")
plot(decompose(ljj))
plot(stl(jj,s.window="periodic"))


deaths <- ts(scan("deaths.txt"),f=12, start =1973)
plot(deaths)
acf(deaths, 50)
?diff
ddeaths <- diff(deaths, lag=12)
ddeaths
plot(ddeaths)
acf(ddeaths, 50)

dddeaths <- diff(ddeaths)
plot(dddeaths)
acf(dddeaths)
