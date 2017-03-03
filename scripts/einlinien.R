library(RcppFaddeeva)#install.packages('RcppFaddeeva')

source("readFiles.R")
source("functions.R")
source("expfit.R")
source("lorentzfit.R")
source("gausfit1.R")
source("voigtfit.R")

par(mar=c(5,5,1,1))

data = readTXT(paste("einlinien/einlinien2",sep=""))

data[[2]]=data[[2]]*10^-3

rate=data[[3]]/data[[2]]
sdata=sqrt(data[[3]])
srate=rate/sdata

untergrund=18.5
suntergrund=0.3
rate=rate-untergrund
srate=sqrt(srate^2+suntergrund^2)

k=1.243
sk=0.010
srate=rate*k*sqrt((srate/rate)^2+(sk/k)^2)
rate=rate*k

x=data[[1]]

drawCI(x,rate,srate,xlab=expression(v / mms^-1),ylab=expression(r / s^-1))
fit=lorentzfit(data.frame(x=x,y=rate,sy=srate),neg=TRUE,weighted=TRUE)
plotlorentz(fit,c(-6,6),lwd=1.5,lty=3)
fit2=gausfit(data.frame(x=x,y=rate,sy=srate),c(-6,6),weighted=TRUE,N0=-5)
plotgaus(fit2,c(-6,6),col="blue",lwd=1.5,lty=2)
A0=fit2['N','Estimate']
C0=fit2['C','Estimate']
mu0=fit2['mu','Estimate']
sigma0=fit2['sig','Estimate']
gamma0=fit['omega','Estimate']
fit3=voigtfit(data.frame(x=x,y=rate,sy=srate),A0,C0,mu0,sigma0,gamma0,weighted=TRUE)
plotvoigt(fit3,c(-6,6),col="green",lwd=2.5,lty=1)

legend(2,27,c("Lorentzfit","Gaussfit","Voigtfit"),col=c("red","blue","green"),lty=c(3,2,1),lwd=c(1.5,1.5,2.5))

sigma=fit3['sigma','Estimate']
ssigma=fit3['sigma','Std. Error']
gamma=fit3['gamma','Estimate']
sgamma=fit3['gamma','Std. Error']

fg=2*sigma*sqrt(2*log(2,exp(1)))
fl=2*gamma

fv=0.5346*fl+sqrt(0.2166*fl^2+fg^2)

sfg=2*ssigma*sqrt(2*log(2,exp(1)))
sfl=2*sgamma
sfv=fv*sqrt(0.02^2+(sfg/fg)^2+(sfl/fl)^2)

gammaL=fit['tau','Estimate']
sgammaL=fit['tau','Std. Error']

fL=2*gammaL

sfL=2*sgammaL
