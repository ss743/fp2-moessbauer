library(RcppFaddeeva)#install.packages('RcppFaddeeva')

voigtfit <- function(input,A0,C0,mu0,sigma0,gamma0,weighted=FALSE) {
  
  y <- input$y
  x <- input$x
  
  thevoigt <- y ~ A * Voigt(x,mu,sigma,gamma) + C
  
  try({
    fit=nls(thevoigt,input,start=list(A=A0,C=C0,mu=mu0,sigma=sigma0,gamma=gamma0))
    chiquadratndf=sum(residuals(fit)^2/abs(fitted(fit)))/summary(fit)$df[2]
    fitdata=rbind(summary(fit)$parameters,c(chiquadratndf,0,0,0))
    return(fitdata)
  })
}

plotvoigt<-function(fitdata,bereich,col="red"){
  A<-fitdata["A","Estimate"]
  C<-fitdata["C","Estimate"]
  mu<-fitdata["mu","Estimate"]
  sigma<-fitdata["sigma","Estimate"]
  gamma<-fitdata["gamma","Estimate"]
  
  try({plot(function(x){A * Voigt(x,mu,sigma,gamma) + C},bereich[1],bereich[2],add=TRUE,col=col,n=10000)})
}
