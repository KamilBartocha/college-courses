
## periodsmother.R

periodsmoother=function(rawperiod, L){

## Input
## rawperiod	=	raw periodograms 
## 			=     (1/T)(abs(fft(timeseries)))^2
## L  		= any positive integer; span is 2*L + 1


## Output 
## smoothedper 		= smoothed periodogram defined from (0, 2*pi]

T = length(rawperiod);
temp = rawperiod;
#temp = c(rev(temp[2:(L+1)]), temp[1:(T/2 + 1 + L)]);
temp = c(rev(temp[2:(L+1)]), temp[1:T], rev(temp[(T-1):(T-L)]));

#smoothedper = c(1:(T/2+1));
smoothedper = c(1:T);

for (k in c(1:T)){

startindex = k; 
endindex =  startindex + 2*L;
smoothedper[k] = mean(temp[startindex:endindex]);
}

#smoothedper = (1/(2*pi))*smoothedper[2:(T/2+1)];
return(smoothedper)
}









