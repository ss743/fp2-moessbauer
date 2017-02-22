library(RcppFaddeeva)#install.packages('RcppFaddeeva')

source("readFiles.R")
source("functions.R")
source("expfit.R")
source("lorentzfit.R")
source("gausfit1.R")
source("voigtfit.R")

par(mar=c(5,5,1,1))

data = readTXT(paste("einlinien/einlinien2",sep=""))

rate=data[[3]]/data[[2]]
sdata=sqrt(data[[3]])
srate=rate/sdata

x=data[[1]]

drawCI(x,rate,srate)
fit=lorentzfit(data.frame(x=x,y=rate),neg=TRUE)
plotlorentz(fit,c(-6,6))
fit2=gausfit(data.frame(x=x,y=rate),c(-6,6))
plotgaus(fit2,c(-6,6),col="blue")
A0=fit2['N','Estimate']
C0=fit2['C','Estimate']
mu0=fit2['mu','Estimate']
sigma0=fit2['sig','Estimate']
gamma0=fit['omega','Estimate']
fit3=voigtfit(data.frame(x=x,y=rate),A0,C0,mu0,sigma0,gamma0)
plotvoigt(fit3,c(-6,6),col="green")
