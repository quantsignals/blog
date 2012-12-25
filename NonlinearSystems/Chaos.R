#load the library tseriesChaos and scatterplot3d for graphing
library(tseriesChaos)
library(scatterplot3d)

#generate and plot times series data for the Rössler system
rossler.ts <- sim.cont(rossler.syst, start=0, end=650, dt=0.1, start.x=c(0,0,0), parms=c(0.15, 0.2, 10))

close.screen(all = TRUE)    

par(mfrow=c(2,2))
plot(rossler.ts)

x <- window(rossler.ts, start=100)
xyz <- embedd(x, m=3, d=8)

scatterplot3d(xyz, type="l")

pc<-xyz[1000+which(sapply( 1000:(length(xyz[,1])-1) , function(i) ( (xyz[i+1,1]<xyz[i,1]) * (xyz[i,1]*xyz[i+1,1])  ) )<0),3]
plot(pc[1:(length(pc)-1)],pc[2:length(pc)],pch=16,cex=.4,xlab="current state", ylab="next state",main="Return Map")
abline(0,1)

plot(pc[1:(length(pc)-2)],pc[3:length(pc)],pch=16,cex=.4,xlab="previous state", ylab="next state",main="Return Map")
abline(0,1)
