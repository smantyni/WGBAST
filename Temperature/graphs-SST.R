############################
# Graphs for SST data and estimates
###############################

# Workflow:

# 1: Run data-temperature-17.R
# 2: Input data to BUGS (data-SST-Aug17.odc) and run model-SST.odc 
# 3: Run graphs-SST.R for figures


# The rest has not been tidied
meanTemp<-apply(TempST,c(2,3), mean, na.rm=T) 
sdTemp<-apply(TempST,c(2,3), sd, na.rm=T) 

cbind(1992:(1992+maxY-1),meanTemp)

# Lines:
ltyL=rep(1,4)#c(1,2,3,1)
colL=c(1,2,4,3)


res <- 6
name_figure <- "figure.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)

# in colour:
end<-26
par(mfrow=c(1,1))
plot(1992:(1992+end), meanTemp[,1], col=1, ylim=c(0,7), type="l",
     xlab="Year", ylab="Temperature (C)" , lwd=2, cex.lab=1.2, cex.axis=1.1)
segments(1992:(1992+end), meanTemp[,1]+sdTemp[,1],
         1992:(1992+end), meanTemp[,1]-sdTemp[,1], lwd=2)

for(m in 2:4){
  points(1992:(1992+end)+0.05*m, meanTemp[,m], col=colL[m],
         type="l", lwd=2, lty=ltyL[m])
  segments(1992:(1992+end)+0.05*m, meanTemp[,m]+sdTemp[,m],
           1992:(1992+end)+0.05*m, meanTemp[,m]-sdTemp[,m], col=colL[m], lwd=2)
}
legend("topleft", c("Jan","Feb","March","April"), col=colL,
       lty=ltyL, lwd=2)

dev.off()

# Lines:
lwdL=c(1,1,2,1)
ltyL=c(1,2,1,3)

# in grey:
end<-24
par(mfrow=c(1,1))
plot(1992:(1992+end), meanTemp[,1], col=1, ylim=c(0,7), type="l",
     xlab="Year", ylab="Temperature (C)" , lwd=1)
segments(1992:(1992+end), meanTemp[,1]+sdTemp[,1],
         1992:(1992+end), meanTemp[,1]-sdTemp[,1], lwd=2)

for(m in 2:4){
  points(1992:(1992+end)+0.05*m, meanTemp[,m], col=1,
         type="l", lwd=lwdL[m], lty=ltyL[m])#m)
  segments(1992:(1992+end)+0.05*m, meanTemp[,m]+sdTemp[,m],
           1992:(1992+end)+0.05*m, meanTemp[,m]-sdTemp[,m], col=1, lwd=2)
}
legend("topleft", c("Jan","Feb","March","April"), col=1,#col=c(1:4),
       lty=ltyL)#c(1:4), lwd=c(1,2,2,2))




# Expected April temperatures (median & 95% PI) from the model
april<-read.table("prg/temperature/ModelExpectedAprilTemp_Jan17.txt", header=T)
april

end<-26

res <- 6
name_figure <- "figure2.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)


#windows()
cbind(1992:(1992+end), april$med[1:(end+1)], april$low[1:(end+1)], april$high[1:(end+1)])

plot(1992:(1992+end), april$med[1:(end+1)], type="l", lwd=2, col="red", ylim=c(1,7),
     xlab="Year", ylab="Temperature (C)", main="SST in April", cex.lab=1.2, cex.axis=1.1)
points(1992:(1992+end), april$low[1:(end+1)], type="l", lwd=1, col="red")
points(1992:(1992+end), april$high[1:(end+1)], type="l", lwd=1, col="red")
m<-4
cbind(1992:(1992+(end-1)), meanTemp[1:end,m])
points(1992:(1992+(end-1)), meanTemp[1:end,m], col=m,type="l", lty=1, lwd=2)
segments(1992:(1992+(end-1)), meanTemp[,m]+sdTemp[,m],
         1992:(1992+(end-1)), meanTemp[,m]-sdTemp[,m], col=m, lwd=2)
legend("topleft", c("Model expected","Observed"), 
       col=c("red", "blue"), lty=c(1,1), lwd=c(2,2))

dev.off()

for(m in 1:3){
  #m<-1
  #cbind(1992:(1992+end), meanTemp[1:end,m])
  points(1992:(1992+end), meanTemp[1:end,m], col=m,type="l", lty=1, lwd=2)
  segments(1992:(1992+end), meanTemp[,m]+sdTemp[,m],
           1992:(1992+end), meanTemp[,m]-sdTemp[,m], col=m, lwd=2)
}
