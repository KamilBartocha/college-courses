
### AR(1) Example
phi1 = 0.9;
T=1000;
data1 = arima.sim(T, model=list(ar=c(phi1)));
plot(data1)

phi1 = -0.9;
T=1000;
data1 = arima.sim(T, model=list(ar=c(phi1)));
plot(data1)

## true spectrum
source("/Users/ombaohc/Dropbox/KAUST Biostatistics Modeling Group/LECTURES/armaspec.R")
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