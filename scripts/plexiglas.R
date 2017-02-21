source("readFiles.R")
source("functions.R")
source("expfit.R")

par(mar=c(5,5,1,1))
empty = readTKA("plexiglas/leer")[[1]]
plexi = readTKA("plexiglas/plexi")[[1]]

empty_times=empty[1:2]
plexi_times=plexi[1:2]

empty[1:2,]=0
plexi[1:2,]=0

rate_e=sum(empty)/empty_times[1]
rate_p=sum(plexi)/plexi_times[1]
s_e=