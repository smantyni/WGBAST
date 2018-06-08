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
PathOut<-"H:/Biom/Scenarios/2018/prg/" # output

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

quants<-c(0.05,0.1,0.5,0.8,0.95)

windows()
par(mfrow=c(2,2))    
par(mar=c(3,5,3,1)+0.1)

med<-c();low<-c();high<-c()
for(y in 1:length(year)){
#y<-length(year)
  length(PFAW2[1,,1])
  med[y]<-summary(as.mcmc(PFAW2[1,y,]), quantiles=quants)$quantiles[3]
  low[y]<-summary(as.mcmc(PFAW2[1,y,]), quantiles=quants)$quantiles[1]
  high[y]<-summary(as.mcmc(PFAW2[1,y,]), quantiles=quants)$quantiles[5]
}
#cbind(year, med)
plot(year+1, med, pch=16, ylim=c(0,3000),
     main="1SW wild, scen 1",
     #main=paste(sep="","1SW wild, scen ",EffScen),
xlab="Year", ylab="Abundance (in 1000's)")
segments(year+1,low, year+1,high)

tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste0(PathOut,"PFA_1SWwild_scen",EffScen,".xlsx"))



med<-c();low<-c();high<-c()
for(y in 1:length(year)){
#y<-2
  med[y]<-summary(as.mcmc(PFA[1,y,]), quantiles=quants)$quantiles[3]
  low[y]<-summary(as.mcmc(PFA[1,y,]), quantiles=quants)$quantiles[1]
  high[y]<-summary(as.mcmc(PFA[1,y,]), quantiles=quants)$quantiles[5]
}
plot(year+1, med, pch=16, ylim=c(0,3000), 
     main="1SW wild & reared, scen 1",
     #main=paste0(PathOut,"1SW wild & reared, scen ",EffScen), 
xlab="Year", ylab="Abundance (in 1000's)")
segments(year+1,low, year+1,high)

tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste0(PathOut,"PFA_1SWall_scen",EffScen,".xlsx"))


med<-c();low<-c();high<-c()
for(y in 1:length(year)){
  med[y]<-summary(as.mcmc(PFAW2_MSW[y,]), quantiles=quants)$quantiles[3]
  low[y]<-summary(as.mcmc(PFAW2_MSW[y,]), quantiles=quants)$quantiles[1]
  high[y]<-summary(as.mcmc(PFAW2_MSW[y,]), quantiles=quants)$quantiles[5]
}
plot(year+1, med, pch=16, ylim=c(0,3000), 
main="MSW wild, scen 1",
#main=paste(sep="","MSW wild, scen ",EffScen), 
xlab="Year", ylab="Abundance (in 1000's)")
segments(year+1,low, year+1,high)

tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste0(PathOut,"PFA_MSWwild_scen",EffScen,".xlsx"))


med<-c();low<-c();high<-c()
for(y in 1:length(year)){
  med[y]<-summary(as.mcmc(PFA_MSW[y,]), quantiles=quants)$quantiles[3]
  low[y]<-summary(as.mcmc(PFA_MSW[y,]), quantiles=quants)$quantiles[1]
  high[y]<-summary(as.mcmc(PFA_MSW[y,]), quantiles=quants)$quantiles[5]
}
plot(year+1, med, pch=16, ylim=c(0,3000), 
     main="MSW wild & reared, scen 1", 
     #main=paste(sep="","MSW wild & reared, scen ",EffScen), 
xlab="Year", ylab="Abundance (in 1000's)")
segments(year+1,low, year+1,high)

tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste0(PathOut,"PFA_MSWall_scen",EffScen,".xlsx"))



 # To check the stats:

# Total PFA: 
med<-c();low<-c();high<-c()
for(y in 1:length(year)){
  med[y]<-summary(as.mcmc(PFAall[y,]), quantiles=quants)$quantiles[3]
  low[y]<-summary(as.mcmc(PFAall[y,]), quantiles=quants)$quantiles[1]
  high[y]<-summary(as.mcmc(PFAall[y,]), quantiles=quants)$quantiles[5]
}
 
tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste0(PathOut,"PFA_Total_scen",EffScen,".xlsx"))

 
# Wild PFA: 
med<-c();low<-c();high<-c()
for(y in 1:length(year)){
  med[y]<-summary(as.mcmc(PFAWall[y,]), quantiles=quants)$quantiles[3]
  low[y]<-summary(as.mcmc(PFAWall[y,]), quantiles=quants)$quantiles[1]
  high[y]<-summary(as.mcmc(PFAWall[y,]), quantiles=quants)$quantiles[5]
}
 
tx<-cbind(year+1,med, low, high)
write.xlsx(tx, paste0(PathOut,"PFA_Totalwild_scen",EffScen,".xlsx"))

 
# PFA:s per origin and age
 
# Wild: 

quants<-c(0.05,0.5,0.95)

W1<-array(NA, dim=c(length(year),length(quants)))
W2<-array(NA, dim=c(length(year),length(quants)))
W3<-array(NA, dim=c(length(year),length(quants)))
W4<-array(NA, dim=c(length(year),length(quants)))
W5<-array(NA, dim=c(length(year),length(quants)))
W6<-array(NA, dim=c(length(year),length(quants)))

for(y in 1:length(year)){
  W1[y,]<-summary(as.mcmc(PFAW2[1,y,]), quantiles=quants)$quantiles
  W2[y,]<-summary(as.mcmc(PFAW2[2,y,]), quantiles=quants)$quantiles
  W3[y,]<-summary(as.mcmc(PFAW2[3,y,]), quantiles=quants)$quantiles
  W4[y,]<-summary(as.mcmc(PFAW2[4,y,]), quantiles=quants)$quantiles
  W5[y,]<-summary(as.mcmc(PFAW2[5,y,]), quantiles=quants)$quantiles
  W6[y,]<-summary(as.mcmc(PFAW2[6,y,]), quantiles=quants)$quantiles
}
write.xlsx(W1, paste0(PathOut,"PFA_W_age1_scen",EffScen,".xlsx"))
write.xlsx(W2, paste0(PathOut,"PFA_W_age2_scen",EffScen,".xlsx"))
write.xlsx(W3, paste0(PathOut,"PFA_W_age3_scen",EffScen,".xlsx"))
write.xlsx(W4, paste0(PathOut,"PFA_W_age4_scen",EffScen,".xlsx"))
write.xlsx(W5, paste0(PathOut,"PFA_W_age5_scen",EffScen,".xlsx"))
write.xlsx(W6, paste0(PathOut,"PFA_W_age6_scen",EffScen,".xlsx"))


R1<-array(NA, dim=c(length(year),length(quants)))
R2<-array(NA, dim=c(length(year),length(quants)))
R3<-array(NA, dim=c(length(year),length(quants)))
R4<-array(NA, dim=c(length(year),length(quants)))
R5<-array(NA, dim=c(length(year),length(quants)))
R6<-array(NA, dim=c(length(year),length(quants)))

for(y in 1:length(year)){
  R1[y,]<-summary(as.mcmc(PFAR2[1,y,]), quantiles=quants)$quantiles
  R2[y,]<-summary(as.mcmc(PFAR2[2,y,]), quantiles=quants)$quantiles
  R3[y,]<-summary(as.mcmc(PFAR2[3,y,]), quantiles=quants)$quantiles
  R4[y,]<-summary(as.mcmc(PFAR2[4,y,]), quantiles=quants)$quantiles
  R5[y,]<-summary(as.mcmc(PFAR2[5,y,]), quantiles=quants)$quantiles
  R6[y,]<-summary(as.mcmc(PFAR2[6,y,]), quantiles=quants)$quantiles
}
write.xlsx(R1, paste0(PathOut,"PFA_R_age1_scen",EffScen,".xlsx"))
write.xlsx(R2, paste0(PathOut,"PFA_R_age2_scen",EffScen,".xlsx"))
write.xlsx(R3, paste0(PathOut,"PFA_R_age3_scen",EffScen,".xlsx"))
write.xlsx(R4, paste0(PathOut,"PFA_R_age4_scen",EffScen,".xlsx"))
write.xlsx(R5, paste0(PathOut,"PFA_R_age5_scen",EffScen,".xlsx"))
write.xlsx(R6, paste0(PathOut,"PFA_R_age6_scen",EffScen,".xlsx"))



 

