# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# Smolt_SpawnerGraphs.R  from Polina (9/2008)
# 
# Makes the graphs of the smolt and spawner amounts for different rivers. 
#
# Changes by Henni.
# - graphs without FLCore 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~               
rm(list=ls(all=TRUE))

PathScen<-"H:/FLR/WGBAST18/Scenarios/" # scenario results 


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
EffScen<-5

#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen",EffScen,".RData")

File
load(File)


# ===============================================================================


library(coda)

year<-c(1992:2032)
cbind(c(1992:2032),c(1:length(year)))

stats<-function(dat){
Mean1<-NULL; Median1<-NULL; Low1<-NULL; High1<-NULL;

for(y in 1:(dim(dat)[1])){
    temp1<-as.mcmc(dat[y,])
    sum_temp1<-summary(temp1, quantiles=c(0.05,0.1,0.5,0.8,0.95))
    Mean1[y]<-mean(temp1)
    Low1[y]<-sum_temp1$quantiles[1]
    Median1[y]<-sum_temp1$quantiles[3]
    High1[y]<-sum_temp1$quantiles[5]
}
result<- cbind(Median1,Low1,High1)
return(result)
}

#! #############################################################################
################################################################################

river<-c("Torne","Simo","Kalix","Råne","Pite","Åby","Byske","Rickleån",
           "Sävarån","Ume/Vindel","Öre","Lögde","Ljungan","Mörrumsån","Emån","Kåge")
maxSmolt<-c(
4000,90,1500,160,80,
40,300,20,30,600,
100,90,7,200,25,120)
maxSpawner<-c(
400,10,200,17,12,
5,35,2,2.5,25,
12,10,0.7,50,4,10)

if(EffScen==5){
maxSmolt<-c(
4000,100,1500,200,100,
50,300,27,35,600,
150,130,8,200,40,100)
maxSpawner<-c(
800,25,300,30,20,
8,70,4,6,40,
25,15,1.2,80,8,20)
}

#partition the graphing device into two rows and two columns
windows(8,10, record=T)
#par(mfrow=c(3,2))
par(mfrow=c(4,2))
par(mar=c(2.5,4,4,1))
      
for(r in 1:Nstocks){    

if(r<16){
  med<-stats(SmoltW[r,,])[,1]
  low<-stats(SmoltW[r,,])[,2]
  high<-stats(SmoltW[r,,])[,3]
  plot(year,med, pch=19, ylab="1000's of salmon", ylim=c(0,maxSmolt[r]),
  main=paste(sep="",river[r]," smolts"))
  segments(year, low, year, high)
  
  med<-stats(SpawnerW[r,,])[,1]
  low<-stats(SpawnerW[r,,])[,2]
  high<-stats(SpawnerW[r,,])[,3]
  plot(year,med, pch=19, ylab="1000's of salmon", ylim=c(0,maxSpawner[r]),
  main=paste(sep="",river[r]," spawners"))
  segments(year, low, year, high)
}
if(r==16){
#r<-16
tmp1<-SmoltW[r,22:length(year),]
  med<-stats(tmp1)[,1]
  low<-stats(tmp1)[,2]
  high<-stats(tmp1)[,3]
  plot(year[22:length(year)],med, pch=19, ylab="1000's of salmon", ylim=c(0,maxSmolt[r]),
  main=paste(sep="",river[r]," smolts"), xlim=c(1992,2032))
  segments(year[22:length(year)], low, year[22:length(year)], high)

tmp2<-SpawnerW[r,22:length(year),]  
  med<-stats(tmp2)[,1]
  low<-stats(tmp2)[,2]
  high<-stats(tmp2)[,3]
  plot(year[22:length(year)],med, pch=19, ylab="1000's of salmon", ylim=c(0,maxSpawner[r]),
  main=paste(sep="",river[r]," spawners"), xlim=c(1992,2032))
  segments(year[22:length(year)], low, year[22:length(year)], high)
}
}

tmp<-0
if(tmp==1){
# Only smolts (needed in advice)
par(mfrow=c(4,2))
par(mar=c(2.5,4,4,1))

for(r in 1:15){    
  med<-stats(SmoltW[r,,])[,1]
  low<-stats(SmoltW[r,,])[,2]
  high<-stats(SmoltW[r,,])[,3]
  plot(year,med, pch=19, ylab="1000's of salmon", ylim=c(0,maxSmolt[r]),
  main=paste(sep="",river[r]," smolts"), xlim=c(1992,2022.4))
  segments(year, low, year, high)
}
for(r in 16:16){    
  med<-stats(SmoltW[r,22:length(year),])[,1]
  low<-stats(SmoltW[r,22:length(year),])[,2]
  high<-stats(SmoltW[r,22:length(year),])[,3]
  plot(year[22:length(year)],med, pch=19, ylab="1000's of salmon", ylim=c(0,maxSmolt[r]),
       main=paste(sep="",river[r]," smolts"), xlim=c(1992,2022.4))
  segments(year[22:length(year)], low, year[22:length(year)], high)
}
}
