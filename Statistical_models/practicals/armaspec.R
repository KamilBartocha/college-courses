
## armaspec.R

armaspec=function(T,arcoeff,macoeff,vare){

## Input
## T = resolution in frequency (length of time series)
## arcoeff = vector of coefficients of the AR component of the time series
## macoeff = vector of coefficients of the MA component
## vare = variance of the white noise error

## Output 
## specout = spectrum of the ARMA process defined from (0, pi]

## Note: This is the parameterization of the ARMA(p,q) process 
## that I will adopt in this program 
## X(t) = arcoeff[1]X(t-1) + ... + arcoeff[p]X(t-p) + 
## W(t) + macoeff[1]W(t-1) + ... + macoeff[q]W(t-q)
## I will assume that the process is already causal and 
## invertible, i.e., the user has done these checks before
## attempting to derive the spectrum.

freq = 2*pi*seq(0, T-1)/T;

arcoeff = c(1, -arcoeff);
macoeff = c(1, macoeff);

theta=c(1:T); phi=c(1:T);

for (k in 1:(T)){

theta.temp=0;
for (m in 1: (length(macoeff))){
theta.temp = theta.temp + macoeff[m]*complex(mod=1, arg=-1*m*freq[k])}
theta[k] = (abs(theta.temp))^2

phi.temp=0;
for (m in 1: (length(arcoeff))){
phi.temp = phi.temp + arcoeff[m]*complex(mod=1, arg=-1*m*freq[k])}
phi[k] = (abs(phi.temp))^2

}

spec = vare*(theta)/(phi);
#specout = spec[1:(T/2)];
specout = spec;
}
  






