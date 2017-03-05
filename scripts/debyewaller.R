#library(Bessel)

Zinf=c(12.84,0.08)
Zmu=c(12.84,0.08)
Z=c()
Z[1]=(Zinf[1]-Zmu[1])/Zinf[1]
Z[2]=Zmu[1]/Zinf[1]*sqrt((Zinf[2]/Zinf[1])^2+(Zmu[2]/Zmu[1])^2)
i=complex(imaginary=1)
fQ=Z[1]*1/Re(1-exp(-TA[1]/2)*besselJ(i*TA[1]/2,0))