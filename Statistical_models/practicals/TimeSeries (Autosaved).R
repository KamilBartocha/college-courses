
## Generate time series data from different models


### Example 1. AR(1) 
phi1 = 0.9;
T=1000;
data1 = arima.sim(T, model=list(ar=c(phi1)));
plot(data1)

phi1 = -0.9;
T=1000;
data2 = arima.sim(T, model=list(ar=c(phi1)));
plot(data2)

par(mfrow=c(2,1));
plot(data1);
plot(data2);

acf1 = acf(data1); 
acf2 = acf(data2);

#### Example 2. MA(1) and MA(5)
theta1 = 1.5; 
T=1000;
data1 = arima.sim(T, model=list(ma=c(theta1)));
plot(data1)

theta1 = 1.5; theta2 = 1; theta3 = 0.9; theta4 = 0.5; theta5 = 0.7;
T=1000;
data2 = arima.sim(T, model=list(ma=c(theta1,theta2,theta3,theta4,theta5)));
plot(data2)

par(mfrow=c(2,1));
plot(data1);
plot(data2);

acf1 = acf(data1); 
acf2 = acf(data2);

####### 
########### Part I: Cramer Representation, Spectrum 

########### Create the basis waveforms

T=128;

timeT = 1:T

freqseq = (2*pi/T)*seq(-(T/2-1), (T/2), 1);

BasisMat = matrix(nrow=T, ncol=T);

for(k in 1:T){
freqhere = freqseq[k];
sinhere = sin(freqhere*timeT);
coshere = cos(freqhere*timeT);
BasisMat[,k] = complex(real=coshere, imag=sinhere);
}



## Plots of the basis waveforms 

par(mfrow=c(2,2));

## Low Frequency k=1; 
q = (T/2 +1);
plot(timeT, Re(BasisMat[,q]), ylab="", xlab="Time index", type="l", col=1);

lines(Im(BasisMat[,q]), col=2);

title(paste("Freq = 1"));



## Low Frequency k=-1; 

q = (T/2 -1);
plot(timeT, Re(BasisMat[,q]), ylab="", xlab="Time index", type="l", col=1);
lines(Im(BasisMat[,q]), col=2);
title(paste("Freq = -1"));



## Middle Frequency k = 20
q = (T/2 + 20);
plot(timeT, Re(BasisMat[,q]), ylab="", xlab="Time index", type="l", col=1);
lines(Im(BasisMat[,q]), col=2);
title(paste("Freq = 20"));


## Show orthogonality 
matout = Conj(t(BasisMat))%*%BasisMat;
matout[(1:5),(1:5)]


######## Specify the variance function of the coefficients

### Case A: Low frequency is dominant 
specfunc = c(1:(T/2+1)); 
specfunc[1:5] = 10; specfunc[6:20] = 2; specfunc[21:(T/2+1)] = 0.25;
par(mfrow=c(1,1)); 
plot(seq(0,pi,length=(T/2+1)), specfunc, type="l", ylab="", xlab="Freq");
title(paste("Discrete Spectrum"))



## Generate the coefficients

randcoeff = c(1:T);

## zero freq
randcoeff[(T/2)] = rnorm(1,0,specfunc[1]);

## pi freq
randcoeff[T] = rnorm(1,0,specfunc[(T/2+1)]);

## positive freq
for(k in (T/2+1):(T-1)){
realhere = rnorm(1,0, 0.5*specfunc[k-(T/2 -1)]);
imaghere = rnorm(1,0, 0.5*specfunc[k-(T/2 -1)]);
randcoeff[k] = complex(real=realhere, imag=imaghere);
randcoeff[T-k] = complex(real=realhere, imag=-1*imaghere);
}



## plot the real and imaginary parts of the coeffs
par(mfrow=c(2,1)); 
plot(seq(-(T/2-1), (T/2), length=T), Re(randcoeff), type="l", ylab="", xlab="Freq"); 
title(paste("Real Part"));
plot(seq(-(T/2-1), (T/2), length=T), Im(randcoeff), type="l", ylab="", xlab="Freq"); title(paste("Imaginary Part"));


par(mfrow=c(1,1)); 
plot(seq(-(T/2-1), (T/2), length=T), abs(randcoeff), type="l", ylab="", xlab="Freq"); 
title(paste("Magnitude Coefficients"));



## Form the time series from the randomly generated coefficients 
tsdata = BasisMat%*%randcoeff;

## tsdata should be theoretically real-valued but 
## numerically it has a small imaginary component which we 
## should force to be equal to 0
tsdata
par(mfrow=c(1,1)); 
plot(Im(tsdata));


tsdata = Re(tsdata);
par(mfrow=c(1,1)); 
plot(timeT, tsdata, xlab="Time Index", ylab="", type="l");


### Case B: High frequency is dominant 
specfunc = c(1:(T/2+1)); 
specfunc[1:30] = 0.25; specfunc[31:50] = 2; specfunc[51:(T/2+1)] = 10;

par(mfrow=c(1,1)); 
plot(seq(0,pi,length=(T/2+1)), specfunc, type="l", ylab="", xlab="Freq");
title(paste("Discrete Spectrum"))



## Generate the coefficients

randcoeff = c(1:T);

## zero freq
randcoeff[(T/2)] = rnorm(1,0,specfunc[1]);

## pi freq
randcoeff[T] = rnorm(1,0,specfunc[(T/2+1)]);

## positive freq
for(k in (T/2+1):(T-1)){
realhere = rnorm(1,0, 0.5*specfunc[k-(T/2 -1)]);
imaghere = rnorm(1,0, 0.5*specfunc[k-(T/2 -1)]);
randcoeff[k] = complex(real=realhere, imag=imaghere);
randcoeff[T-k] = complex(real=realhere, imag=-1*imaghere);
}



## plot the real and imaginary parts of the coeffs

par(mfrow=c(2,1)); 
plot(seq(-(T/2-1), (T/2), length=T), Re(randcoeff), type="l", ylab="", xlab="Freq"); 
title(paste("Real Part"));
plot(seq(-(T/2-1), (T/2), length=T), Im(randcoeff), type="l", ylab="", xlab="Freq"); 
title(paste("Imaginary Part"));


par(mfrow=c(1,1)); 
plot(seq(-(T/2-1), (T/2), length=T), abs(randcoeff), type="l", ylab="", xlab="Freq"); 
title(paste("Magnitude Coefficients"));



## Time Series 
tsdata = BasisMat%*%randcoeff;
par(mfrow=c(1,1)); plot(Im(tsdata));
tsdata = Re(tsdata);
par(mfrow=c(1,1)); 
plot(timeT, tsdata, xlab="Time Index", ylab="", type="l");


## AR(1) Process
phi1 = -0.9; 
T = 128;

tsdata = arima.sim(T, model=list(ar=c(phi1)));
plot(tsdata)

sig2N = 1;
freqseq = (2*pi/T)*seq(-(T/2-1), (T/2), 1);
truespec = sig2N/(abs(1 - phi1*complex(mod=1, arg=-freqseq)))^2;
plot(freqseq, truespec, type="l")

## Estimate the spectrum 
## Suppose that P=2
out = arima(tsdata, order=c(2,0,0))
names(out)
p1est = out$coef[1]
p2est = out$coef[2]
den = (abs(1 - p1est*complex(mod=1, argument=-freqseq) - p2est*complex(mod=1, argument = -2*freqseq)))^2;
specest = out$sigma2/den;
lines(freqseq, specest, col=2)




## true spectrum
#source("/Users/ombaohc/Dropbox/KAUST Biostatistics Modeling Group/#LECTURES/armaspec.R")
source("/Users/ombaohc/Dropbox (Personal)/KAUST-CLASS/STAT-MODELS/COMPUTING/armaspec.R")
spec1=armaspec(T, arcoeff=c(phi1), macoeff=c(0), 1)
plot(x=seq(0,1, length=T), y=spec1, type="l")

### AR(2) Examples

## X(t) = phi1*X(t-1) + phi2*X(t-2) + E(t)
## Two complex roots

######################################################
## Example 1 and 2
## Role of the phase of tne roots
#######################################################
rootM = 1.1;
samprate = 1000;
T = 1000;
freqx = 100;
lambda = freqx/samprate;
rootph = 2*pi*lambda;
r1 = complex(modulus = rootM, argument = rootph);
r2 = complex(modulus = rootM, argument = -rootph);
phi1 = (1/r1 + 1/r2); 
phi2 = -1/(r1*r2);
phi1 = Re(phi1);
phi2 = Re(phi2);
data1 = arima.sim(T, model=list(ar=c(phi1, phi2)));
plot(data1, type="l");

spec1 = armaspec(T, arcoeff=c(phi1,phi2), macoeff=c(0), 1)
plot(x=seq(0,1, length=T), y=spec1, type="l")


## Example 2
rootM = 1.1;
samprate = 1000;
T = 1000;
freqx = 10;
lambda = freqx/samprate;
rootph = 2*pi*lambda;
rootph = 2*pi*lambda
r1 = complex(modulus = rootM, argument = rootph);
r2 = complex(modulus = rootM, argument = -rootph);
phi1 = (1/r1 + 1/r2); 
phi2 = -1/(r1*r2);
T = 1000;
data2 = arima.sim(T, model=list(ar=c(phi1, phi2)));
plot(data2, type="l");

par(mfrow=c(2,1)); 
plot(data1, type="l");
plot(data2, type="l");

##########################################
## Example 3 and 4
## Role of the magnitude of the roots
##########################################

rootM = 1.01;
samprate = 1000;
T = 1000;
freqx = 100;
lambda = freqx/samprate;
rootph = 2*pi*lambda;
r1 = complex(modulus = rootM, argument = rootph);
r2 = complex(modulus = rootM, argument = -rootph);
phi1 = (1/r1 + 1/r2); 
phi2 = -1/(r1*r2);
data3 = arima.sim(T, model=list(ar=c(phi1, phi2)));
plot(data3, type="l");

rootM = 1.5;
samprate = 1000;
T = 1000;
freqx = 100;
lambda = freqx/samprate;
rootph = 2*pi*lambda;
r1 = complex(modulus = rootM, argument = rootph);
r2 = complex(modulus = rootM, argument = -rootph);
phi1 = (1/r1 + 1/r2); 
phi2 = -1/(r1*r2);
T = 1000;
data4 = arima.sim(T, model=list(ar=c(phi1, phi2)));
plot(data4, type="l");

par(mfrow=c(2,1)); 
m1 = min(c(data3,data4));
m2 = max(c(data3,data4));
plot(data3, type="l", ylim=c(m1,m2));
plot(data4, type="l", ylim=c(m1,m2));

####################################################
##
## Mixtures of AR(2) oscillations
####################################################

rootM = 1.05;
samprate = 1000;
T = 1000;
freqx = 200;
lambda = freqx/samprate;
rootph = 2*pi*lambda;
r1 = complex(modulus = rootM, argument = rootph);
r2 = complex(modulus = rootM, argument = -rootph);
phi1 = (1/r1 + 1/r2); 
phi2 = -1/(r1*r2);
T = 1000;
latent1 = arima.sim(T, model=list(ar=c(phi1, phi2)));
par(mfrow=c(1,1));
plot(latent1, type="l");

rootM = 1.01;
samprate = 1000;
T = 1000;
freqx = 5;
lambda = freqx/samprate;
rootph = 2*pi*lambda;
r1 = complex(modulus = rootM, argument = rootph);
r2 = complex(modulus = rootM, argument = -rootph);
phi1 = (1/r1 + 1/r2); 
phi2 = -1/(r1*r2);
T = 1000;
latent2 = arima.sim(T, model=list(ar=c(phi1, phi2)));
par(mfrow=c(1,1));
plot(latent2, type="l");

noise = rnorm(T,0,1);
W1 = 1; W2 = 10;
data1 = W1*latent1 + W2*latent2 + noise;
par(mfrow=c(1,1));
plot(data1, type="l");