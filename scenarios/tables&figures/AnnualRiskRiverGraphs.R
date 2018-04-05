# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# Prob_Spawner_increase.R  from Polina (9/2008)
# 
# Makes the graphs of the smolt and spawner amounts for different rivers. 
#
# Changes by Henni.
# - graphs without FLCore 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~               
rm(list=ls(all=TRUE))

#setwd("H:/Biom/Scenarios/2017/rpt/Figures")

library("xlsx")


################################################################################
#! #############################################################################
# Version of the estimation model
Model<-"New_SR_long" # 50years forward 
LastPredYear<-2067
#Model<-"New_SR" # 15 years forward
#LastPredYear<-2032

#! Mps
choice<-"MED"

nrScen<-6

#! Set the last year for historic part and the last year for predictions:
LastHistYear<-2017    
yBreak<-length(c(1992:LastHistYear))

Nstocks<-16

PathScen<-"H:/FLR/WGBAST18/Scenarios/" # scenario results 
PathOut<-"H:/Biom/Scenarios/2018/prg/" # output

#! #############################################################################
################################################################################


#Number of simulations in PerformStats file
sim<- 1000

#Number of years in future projections
years<-c(1992,LastPredYear)
years<-c(years[],years[2]-years[1]+1) 
Years<-c(years[1]:years[2])

#Introduce the different names for the different salmon stocks
RiverNames<-c("Tornionjoki","Simojoki","Kalixälven","Råneälven"
,"Piteälven","Åbyälven","Byskeälven","Rickleån","Sävåran"
,"Vindelälven","Öreälven","Lögdeälven","Ljungan","Mörrumsån"
,"Emån", "Kågeälven")


#Store risk values
RecRisk<-array(NA, dim=c(Nstocks,nrScen,years[3]))

for(scen in 1:nrScen){ # number of scenarios

  #scen<-1
  EffScen<-scen
  
  #Load the file containing stats
  if(scen==1){File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen1.RData")}
  if(scen==2){File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen2.RData")}
  if(scen==3){File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen3.RData")}
  if(scen==4){File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen4.RData")}
  if(scen==5){File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen6.RData")} # Note! These two are
  if(scen==6){File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen5.RData")} # other way round
  
  File
  load(File)
  
  target_R0<-array(NA, dim=c(1000,Nstocks))
  for(r in 1:Nstocks){
    for(i in 1:1000){
      target_R0[i,r]<-mean(R0[(yBreak-4):yBreak,r,i]) # average R0 over last 5 years
    }
  }

  #Calculate the chance of some statistic (vector) being above some reference point
  risk<-function(x,ref) {sum(ifelse(x>ref,1,0))/length(x)}

  #Proportion of simulations in which the recruitment is above 75% carrying capacity
  #by year
  for(y in 1:years[3]){
    for(r in 1:Nstocks){
      RecRisk[r,scen,y]<-risk(SmoltW[r,y,],0.75*target_R0[,r])
    }
  } 
  
}

dim(RecRisk)
cbind(t(RecRisk[1,,]), c(1992:LastPredYear))

FirstYear50<-array(NA, dim=c(Nstocks, nrScen))
for(r in 1:Nstocks){
  for(s in 1:nrScen){
    tmp<-0
    y<-yBreak+2 # go through future years
    while(tmp==0){
      if(y==years[3]){tmp<-1}
      if(RecRisk[r,s,y]>0.499){FirstYear50[r,s]<-y+1991; tmp<-1}else{y<-y+1}
    }
  }
}   
rownames(FirstYear50)<-RiverNames
colnames(FirstYear50)<-c(1:6);FirstYear50
write.xlsx(FirstYear50, file=paste0(PathOut,"FirstYear50.xlsx"))

FirstYear70<-array(NA, dim=c(Nstocks, nrScen))
for(r in 1:Nstocks){
  for(s in 1:nrScen){
    tmp<-0
    y<-yBreak+2 # go through future years
    while(tmp==0){
      if(y==years[3]){tmp<-1}
      if(RecRisk[r,s,y]>0.699){FirstYear70[r,s]<-y+1991; tmp<-1}else{y<-y+1}
    }
  }
}   
rownames(FirstYear70)<-RiverNames
colnames(FirstYear70)<-c(1:6);FirstYear70
write.xlsx(FirstYear70, file=paste0(PathOut,"FirstYear70.xlsx"))


#######################

ScenNames<-c("1","2","3","4","5","6")
ScenLty=c(1:6)
ScenPch = c(19,22,3,25,8,5)

v1<-2022
v2<-2024

#
windows(record=T)
par(mfrow=c(4,1))
#par(mfrow=c(3,1))
par(mar=c(2.5,4,3,3))

for(r in 1:Nstocks){
  Risk<-cbind(RecRisk[r,1,],RecRisk[r,2,],RecRisk[r,3,],
  RecRisk[r,4,],RecRisk[r,5,],RecRisk[r,6,])
  plot(Years, Risk[,1], type = "n", ylim=c(0,1), xlab = "Year", 
  ylab = "Probability of meeting 75% CC obj.", main = RiverNames[r])
  for(i in 1:nrScen){
    points(Years, Risk[,i], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  legend(x="topleft", bty ="n", ScenNames, lty=ScenLty, pch=ScenPch)
  abline(v=v1); abline(v=v2)
}

windows()
par(mfrow=c(3,1))
par(mar=c(2.5,4,3,3))

for(r in 13:15){
  Risk<-cbind(RecRisk[r,1,],RecRisk[r,2,],RecRisk[r,3,],
  RecRisk[r,4,],RecRisk[r,5,])
  plot(Years, Risk[,1], type = "n", ylim=c(0,1), xlab = "Year", 
  ylab = "Probability of meeting 75% CC obj.", main = RiverNames[r])
  for(i in 1:nrScen){
    points(Years, Risk[,i], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  legend(x="topleft", bty ="n", ScenNames, lty=ScenLty, pch=ScenPch)
  abline(v=v1); abline(v=v2)
}




