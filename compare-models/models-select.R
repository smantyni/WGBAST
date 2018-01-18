library(rjags)
library(runjags)
library(tidyverse)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)
library(gridExtra)
library(coda)

source("functions/tidy-functions.r")


Rivername<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran","Ume","Ore","Logde","Ljungan","Eman","Morrum","Kage")
nstocks<-16


# Model 1: BUGS model
# =================
folder1<-"H:/FLR/WGBAST15/NoExpertsNewFec"; model<-"BUGS15"
Years<-c(1987:2016)
length(Years)
Years2<-c(1992:2015)
length(Years2)
#LastYear<-2015


# Model 2:
# =================
#load("all_stocks_Simo_2017-11-21.RData")   #2015 data
#load("output/spawners_JAGS_2112.RData") 
#dsub[,"NspWtot[6,1]"]


# Read runjags results from file
folder2<-"H:/Biom/FullLifeHistoryModel/2017/prg/output/JAGS/"; model<-"JAGS15"
run1<-results.jags(str_c(folder2,"logN_Simo_500k"))

#summary(run1, var="R0")
#summary(run1, var="alpha")
#summary(run1, var="survMps")

#plot(run1, var = "R0")

chains<-as.mcmc.list(run1)

#summary(chains[,"R0[1]"])

#saveRDS(chains, file="chains_logN_Simo.rds")





