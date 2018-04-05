library(coda)
library(xlsx)


################################################################################
#! #############################################################################
# Version of the estimation model
#Model<-"Orig"
Model<-"New_SR"

# Time
LastHistYear<-2017
LastPredYear<-2032
year<-c(1992:LastPredYear)
length(year)
Nyears<-length(year)
Nstocks<-16

# ===============================================================================
# Scenarios
#! Mps
choice<-"MED"   # corresponds to Mps during 2008-2011 period

# Maturation is the same in all scenarios
#! Effort 
EffScen<-1

PathScen<-"H:/FLR/WGBAST18/Scenarios/" # scenario results 

#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen",EffScen,".RData")

File
load(File)


# ===============================================================================


year<-c(1992:LastPredYear)


PFAW[,,1:16,1,1:3]


dim(PFAW)
PFAW2<-array(NA, dim=c(6,length(year),1000))
PFAW2_MSW<-array(NA, dim=c(length(year),1000))
PFAR2<-array(NA, dim=c(6,length(year),1000))
PFAR2_MSW<-array(NA, dim=c(length(year),1000))
PFA<-array(NA, dim=c(6,length(year),1000))
PFA_MSW<-array(NA, dim=c(length(year),1000))
PFAall<-array(NA, dim=c(length(year),1000))
PFAWall<-array(NA, dim=c(length(year),1000))
for(y in 1:length(year)){
for(s in 1:1000){
  PFAWall[y,s]<-sum(PFAW[1:6,y,1:Nstocks,1,s], na.rm=T)
  PFAall[y,s]<-sum(PFAW[1:6,y,1:Nstocks,1,s], na.rm=T)+sum(PFAR[1:6,y,1:4,1,s])
  PFA_MSW[y,s]<-sum(PFAW[2:6,y,1:Nstocks,1,s], na.rm=T)+sum(PFAR[2:6,y,1:4,1,s])
  PFAR2_MSW[y,s]<-sum(PFAR[2:6,y,1:4,1,s])
  PFAW2_MSW[y,s]<-sum(PFAW[2:6,y,1:Nstocks,1,s], na.rm=T)

for(a in 1:6){
  PFAW2[a,y,s]<-sum(PFAW[a,y,1:Nstocks,1,s], na.rm=T)
  PFAR2[a,y,s]<-sum(PFAR[a,y,1:4,1,s])
  PFA[a,y,s]<-sum(PFAW[a,y,1:Nstocks,1,s], na.rm=T)+sum(PFAR[a,y,1:4,1,s])
}
}}

# OBS! One year shift in the time-series! Take care that
# highest abundances hit the correct years!

windows()
par(mfrow=c(2,2))    
par(mar=c(3,5,3,1)+0.1)

med<-c();low<-c();high<-c()
for(y in 1:length(year)){
#y<-length(year)
  length(PFAW2[1,,1])
  med[y]<-summary(as.mcmc(PFAW2[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[3]
  low[y]<-summary(as.mcmc(PFAW2[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[1]
  high[y]<-summary(as.mcmc(PFAW2[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[5]
}
#cbind(year, med)
plot(year+1, med, pch=16, ylim=c(0,3000),
     main="1SW wild, scen 1",
     #main=paste(sep="","1SW wild, scen ",EffScen),
xlab="Year", ylab="Abundance (in 1000's)")
segments(year+1,low, year+1,high)

tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste(sep="","PFA_1SWwild_scen",EffScen,".xlsx"))

med<-c();low<-c();high<-c()
for(y in 1:length(year)){
#y<-2
  med[y]<-summary(as.mcmc(PFA[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[3]
  low[y]<-summary(as.mcmc(PFA[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[1]
  high[y]<-summary(as.mcmc(PFA[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[5]
}
plot(year+1, med, pch=16, ylim=c(0,3000), 
     main="1SW wild & reared, scen 1",
     #main=paste(sep="","1SW wild & reared, scen ",EffScen), 
xlab="Year", ylab="Abundance (in 1000's)")
segments(year+1,low, year+1,high)

tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste(sep="","PFA_1SWall_scen",EffScen,".xlsx"))


med<-c();low<-c();high<-c()
for(y in 1:length(year)){
  med[y]<-summary(as.mcmc(PFAW2_MSW[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[3]
  low[y]<-summary(as.mcmc(PFAW2_MSW[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[1]
  high[y]<-summary(as.mcmc(PFAW2_MSW[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[5]
}
plot(year+1, med, pch=16, ylim=c(0,3000), 
main="MSW wild, scen 1",
#main=paste(sep="","MSW wild, scen ",EffScen), 
xlab="Year", ylab="Abundance (in 1000's)")
segments(year+1,low, year+1,high)

tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste(sep="","PFA_MSWwild_scen",EffScen,".xlsx"))


med<-c();low<-c();high<-c()
for(y in 1:length(year)){
  med[y]<-summary(as.mcmc(PFA_MSW[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[3]
  low[y]<-summary(as.mcmc(PFA_MSW[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[1]
  high[y]<-summary(as.mcmc(PFA_MSW[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[5]
}
plot(year+1, med, pch=16, ylim=c(0,3000), 
     main="MSW wild & reared, scen 1", 
     #main=paste(sep="","MSW wild & reared, scen ",EffScen), 
xlab="Year", ylab="Abundance (in 1000's)")
segments(year+1,low, year+1,high)

tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste(sep="","PFA_MSWall_scen",EffScen,".xlsx"))



 # To check the stats:

# Total PFA: 
med<-c();low<-c();high<-c()
for(y in 1:length(year)){
  med[y]<-summary(as.mcmc(PFAall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[3]
  low[y]<-summary(as.mcmc(PFAall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[1]
  high[y]<-summary(as.mcmc(PFAall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[5]
}
 
tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste(sep="","PFA_Total_scen",EffScen,".xlsx"))

 
# Wild PFA: 
med<-c();low<-c();high<-c()
for(y in 1:length(year)){
  med[y]<-summary(as.mcmc(PFAWall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[3]
  low[y]<-summary(as.mcmc(PFAWall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[1]
  high[y]<-summary(as.mcmc(PFAWall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[5]
}
 
tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste(sep="","PFA_Totalwild_scen",EffScen,".xlsx"))

 
 
 
 

