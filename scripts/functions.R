drawCI <- function(x,y,sy,xlab="x",ylab="y",col="black",scol="darkgrey"){
  plot(x,y,pch=4,cex=0.6,bty="l",col=col,xlab=xlab,ylab=ylab)
  arrows(x,y,x,y-sy,cex=0.6,pch=4,bty="l",col=scol,length=0.05,angle=90)
  arrows(x,y,x,y+sy,cex=0.6,pch=4,bty="l",col=scol,length=0.05,angle=90)
  points(x,y,cex=0.6,pch=4,col=col)
  grid()
}
drawCIx <- function(x,y,sy,sx,xlab="x",ylab="y",col="black",scol="darkgrey"){
  plot(x,y,pch=4,cex=0.6,bty="l",col=col,xlab=xlab,ylab=ylab)
  arrows(x,y,x,y-sy,cex=0.6,pch=4,bty="l",col=scol,length=0.05,angle=90)
  arrows(x,y,x,y+sy,cex=0.6,pch=4,bty="l",col=scol,length=0.05,angle=90)
  arrows(x,y,x-sx,y,cex=0.6,pch=4,bty="l",col=scol,length=0.05,angle=90)
  arrows(x,y,x+sx,y,cex=0.6,pch=4,bty="l",col=scol,length=0.05,angle=90)
  points(x,y,cex=0.6,pch=4,col=col)
  grid()
}


draw <- function(x,y,xlab="x",ylab="y",col="black",scol="darkgrey"){
  plot(x,y,pch=4,cex=0.6,bty="l",col=col,xlab=xlab,ylab=ylab)
  grid()
}

roundfunc <- function(vals){
  x=vals[1]
  xerr=vals[2]
  n=0
  for(i in -20:20){
    a=round(xerr,i)*10^i
    if(a==1){
      n=i+1
      return(c(round(x,n),round(xerr,n)))
    }
    if(a==2){
      if(xerr*10^i<1.95){
        n=i+1
      } else {
        n=i
      }
      return(c(round(x,n),round(xerr,n)))
    }
    if(a>2){
      n=i
      return(c(round(x,n),round(xerr,n)))
    }
  }
  return(vals)
  
}