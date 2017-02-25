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
