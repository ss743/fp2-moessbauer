drawCI <- function(x,y,sy,xlab="x",ylab="y",col="black",scol="darkgrey"){
  plot(x,y,pch=4,cex=0.6,bty="l",col=col,xlab=xlab,ylab=ylab)
  arrows(x,y,x,y-sy,cex=0.6,pch=4,bty="l",col=scol,length=0.05,angle=90)
  arrows(x,y,x,y+sy,cex=0.6,pch=4,bty="l",col=scol,length=0.05,angle=90)
  points(x,y,cex=0.6,pch=4,col=col)
  grid()
}

draw <- function(x,y,xlab="x",ylab="y",col="black",scol="darkgrey"){
  plot(x,y,pch=4,cex=0.6,bty="l",col=col,xlab=xlab,ylab=ylab)
  grid()
}

