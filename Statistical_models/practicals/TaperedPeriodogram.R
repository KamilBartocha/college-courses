
## True Spectrum
rootM = 1.2;
samprate = 1000;
B=5;
T = B*samprate;
## Observe the data for 10 seconds
freqx = 10;
lambda = freqx/samprate;
rootph = 2*pi*lambda;
r1 = complex(modulus = rootM, argument = rootph);
r2 = complex(modulus = rootM, argument = -rootph);
phi1 = (1/r1 + 1/r2); 
phi2 = -1/(r1*r2);
phi1 = Re(phi1);
phi2 = Re(phi2);

par(mfrow=c(1,1));
spec1 = armaspec(samprate, arcoeff=c(phi1,phi2), macoeff=c(0), 1)
plot(x=seq(0,1, length=samprate), y=spec1, type="l")

par(mfrow=c(1,1));
data1 = arima.sim(T, model=list(ar=c(phi1, phi2)));
plot(data1, type="l");


## Compute the periodogram at each time block (1 sec)
## then average
data1 = data1 - mean(data1);
period.mat = matrix(ncol=B, nrow=samprate);
for(b in 1:B){
index.start = (b-1)*samprate + 1;
index.end = b*samprate;
tempdata = data1[index.start:index.end];
tempperiod1 = (1/samprate)*(abs(fft(tempdata)))^2;
period.mat[,b] = tempperiod1;
}

maxy = max(max(period.mat))
par(mfrow=c(1,1));
plot(spec1, ylim=c(0, maxy), type="l", col=2);
for(b in 1:B){
lines(period.mat[,b], type="l", col="light blue"); 
}
lines(spec1, type="l", col=2);

##spectral estimate 
smpred = apply(period.mat, 1, mean)
plot(smpred, type="l")
lines(spec1, col=2)

## Apply a taper 
## Hahn window
W = 1 - cos(2*pi*seq(1,samprate)/samprate);
W = W/sum(W);
par(mfrow=c(1,1))
plot(W);

Wperiod.mat = matrix(ncol=B, nrow=samprate);
for(b in 1:B){
  index.start = (b-1)*samprate + 1;
  index.end = b*samprate;
  tempdata = data1[index.start:index.end];
  tempdata = tempdata*W;
  tempperiod1 = ((abs(fft(tempdata)))^2)/(sum(W^2));
  Wperiod.mat[,b] = tempperiod1;
}

plot(spec1, ylim=c(0, maxy), type="l", col=2);
for(b in 1:B){
  lines(Wperiod.mat[,b], type="l", col="light blue"); 
}
lines(spec1, type="l", col=2);

##spectral estimate 
Wsmpred = apply(Wperiod.mat, 1, mean)
plot(Wsmpred, type="l")
lines(spec1, col=2)

par(mfrow=c(1,2));
plot(smpred, type="l"); 
lines(spec1, col=2);
plot(Wsmpred, type="l"); 
lines(spec1, col=2);

par(mfrow=c(3,1));
diff1 = abs(smpred - spec1);
diff2 = abs(Wsmpred - spec1);
plot(diff1, col=1); 
plot(diff2, col=1);
plot(diff1 - diff2);

par(mfrow=c(1,1));
plot((diff1 - diff2), type="l")






### 
### Rectangular taper ### 
W2 = rep(1, T); 
W = c(rep(0, T/2), W2, rep(0, T/2));
#W = W2;
dW = fft(W);
par(mfrow=c(2,1));
plot(Re(dW));
plot(Im(dW));
summary(Im(dW));
par(mfrow=c(1,1))
plot(abs(dW), type="l")















