# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Statistics for salmon returning to Bothnian Bay
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


rm(list=ls(all=TRUE))
library(coda)
library(xlsx)

PathScen<-"H:/FLR/WGBAST18/Scenarios/" # scenario results 
PathOut<-"H:/Biom/Scenarios/2018/prg/" # output



################################################################################
#! #############################################################################
# Version of the estimation model
Model<-"New_SR"

# Time
LastHistYear<-2017
LastPredYear<-2032
year<-c(1992:LastPredYear)
length(year)
Nyears<-length(year)
Nstocks<-16


# Scenarios
#! Mps
choice<-"MED"  

#! Effort 
EffScen<-5 

#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen",EffScen,".RData")

File
load(File)
#! #############################################################################
################################################################################

dim(Migr_AU13tot)

mu<-c();sd<-c()
q5<-c(); q50<-c(); q95<-c()

for(i in 1:Nyears){
  tmp<-summary(as.mcmc(Migr_AU13tot[i,]), quantiles=c(0.05,0.5,0.95))
  mu[i]<-tmp$statistics[1]
  sd[i]<-tmp$statistics[2]
  q5[i]<-tmp$quantiles[1]
  q50[i]<-tmp$quantiles[2]
  q95[i]<-tmp$quantiles[3]
}

res<-cbind(year,mu, sd, q5, q50, q95)
write.xlsx(res,paste0(PathOut,"migratingAU13.xlsx"))

