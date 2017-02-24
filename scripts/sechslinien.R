library(RcppFaddeeva)#install.packages('RcppFaddeeva')

source("readFiles.R")
source("functions.R")
source("expfit.R")
source("lorentzfit.R")
source("gausfit1.R")
source("voigtfit.R")

par(mar=c(5,5,1,1))

data = readTXT(paste("sechslinien/sechslinien",sep=""))

rate=data[[3]]/data[[2]]
sdata=sqrt(data[[3]])
srate=rate/sdata

x=data[[1]]

drawCI(x,rate,srate,xlab=expression(v / mms^-1),ylab=expression(r / s^-1))
 fit=sixlorentz(data.frame(x=x,y=rate,sy=srate),neg=TRUE,weighted=TRUE,-5.23,-2.97,-0.51,0.92,3.24,5.43)
 plotsixlorentz(fit,c(-8.05,8.05),lwd=1.5,lty=3)
 fit2=sixgaus(data.frame(x=x,y=rate,sy=srate),c(-8.05,8.05),weighted=TRUE,-5,-3,-0.5,1,3,5)
 plotsix(fit2,c(-8.05,8.05),col="blue",lwd=1.5,lty=2)
 A01=fit2['N1','Estimate']
 A02=fit2['N2','Estimate']+0.00015
 A03=fit2['N3','Estimate']
 A04=fit2['N4','Estimate']
 A05=fit2['N5','Estimate']
 A06=fit2['N6','Estimate']-0.0004
 C0=0.0286#fit2['C','Estimate']
 mu01=fit2['mu1','Estimate']
 mu02=fit2['mu2','Estimate']
 mu03=fit2['mu3','Estimate']
 mu04=fit2['mu4','Estimate']
 mu05=fit2['mu5','Estimate']
 mu06=fit2['mu6','Estimate']
 sigma01=fit2['sig1','Estimate']
 sigma02=fit2['sig2','Estimate']
 sigma03=fit2['sig3','Estimate']
 sigma04=fit2['sig4','Estimate']
 sigma05=fit2['sig5','Estimate']
 sigma06=fit2['sig6','Estimate']
 gamma=0.1
 gamma01=gamma#fit['omega1','Estimate']
 gamma02=gamma#fit['omega2','Estimate']
 gamma03=gamma#fit['omega3','Estimate']
 gamma04=gamma#fit['omega4','Estimate']
 gamma05=gamma#fit['omega5','Estimate']
 gamma06=gamma#fit['omega6','Estimate']
 fit3=sixvoigt(data.frame(x=x,y=rate,sy=srate),A01,A02,A03,A04,A05,A06,C0,mu01,mu02,mu03,mu04,mu05,mu06,sigma01,sigma02,sigma03,sigma04,sigma05,sigma06,gamma01,gamma02,gamma03,gamma04,gamma05,gamma06,weighted=TRUE)
 plotsixvoigt(fit3,c(-8.05,8.05),col="green",lwd=2.5,lty=1)

 legend(0,0.0272,c("Lorentzfit","Gaussfit","Voigtfit"),col=c("red","blue","green"),lty=c(3,2,1),lwd=c(1.5,1.5,2.5))
 