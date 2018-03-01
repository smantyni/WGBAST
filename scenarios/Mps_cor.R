# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Medians and cv's of (instantaneous) post smolt mortality 
# for autokorrelation scenarios. 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

#Years<-c(1987:2014)

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# POST-SMOLT-MORTALITY, input (Wild smolts)

MpsW<-array(NA, dim=c(length(chains[,"MpsW[1]"][[1]]),length(Years)))
for(y in 1:length(Years)){   
  MpsW[,y]<-chains[,str_c("MpsW[",y,"]")][[1]]
}

MpsW[1:10,]
dim(MpsW)
lMpsW<-log(MpsW)

lMpsW_cor<-cor(lMpsW)
round(lMpsW_cor,3)

library(coda)

MpsMed<-vector()
MpsHigh<-vector()
MpsLow<-vector()
CV<-vector()
for(i in 1:(length(Years))){
  SummaryMps<-summary(as.mcmc(MpsW[,i]))
  MpsMed[i]<-SummaryMps$quantiles[3]
  CV[i]<-SummaryMps$statistics[2]/SummaryMps$statistics[1]
  MpsHigh[i]<-SummaryMps$quantiles[5]
  MpsLow[i]<-SummaryMps$quantiles[1]

}
# Input these to Mps_logitAR1.odc-model (Scenarios/prg/Mps/ directory)
cbind(MpsMed,CV, MpsLow, MpsHigh, Years)




# Reared
MpsR<-array(NA, dim=c(length(chains[,"MpsR[1]"][[1]]),length(Years)))
for(y in 1:length(Years)){   
  MpsR[,y]<-chains[,str_c("MpsR[",y,"]")][[1]]
}

lMpsR<-log(MpsR)

lMpsR_cor<-cor(lMpsR)
round(lMpsR_cor,3)

MpsMedR<-vector()
CVR<-vector()
for(i in 1:(length(Years))){
  SummaryMpsR<-summary(as.mcmc(MpsR[,i]))
  MpsMedR[i]<-SummaryMpsR$quantiles[3]
  CVR[i]<-SummaryMpsR$statistics[2]/SummaryMpsR$statistics[1]
}
cbind(MpsMedR,CVR)
