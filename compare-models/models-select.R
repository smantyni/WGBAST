library(rjags)
library(runjags)
library(tidyverse)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)
library(gridExtra)
library(coda)

# setwd("C:/R/WGBAST/")
source("functions/tidy-functions_2chains.r")


nstocks<-16

pathData<-"C:/Users/412hpulkkin/Dropbox/WGBAST/JAGS/data_2018/"
#pathData<-"C:/R/JAGS17/data_2016/"

Rivername<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
             "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage")
Rivername_long<-read.table(str_c(pathData, "rivernames.txt"))[,1]

# Model 1: BUGS model
# =================
#folder1<-"H:/FLR/WGBAST15/NoExpertsNewFec"; model<-"BUGS15"
folder1<-"H:/FLR/WGBAST17/FLHM2016"; model<-"BUGS16"
YearsB<-c(1987:2016)
length(YearsB)
Years2B<-c(1992:2016)
length(Years2B)


# Model 2:
# =================
Years<-c(1987:2017)
length(Years)
Years2<-c(1992:2017)
length(Years2)


#load("all_stocks_Simo_2017-11-21.RData")   #2015 data
#load("output/spawners_JAGS_2112.RData") 
#dsub[,"NspWtot[6,1]"]


# Read runjags results from file
#folder2<-"H:/Biom/FullLifeHistoryModel/2017/prg/output/JAGS/"; model<-"JAGS16"
#load(str_c(folder2,"FLHM_mov_average_Mps_2018-01-22.RData"))

#load(file="H:/FLR/WGBAST18/WGBAST_JAGS_comparison_2.RData")
#chains<-as.mcmc.list(run)
#chains<-window(chains,start=1000000)

#load(file="H:/FLR/WGBAST18/comp_1_20022018.RData")

#load(file="H:/FLR/WGBAST18/WGBAST_JAGS_SRorig.RData"); SRnew<-"no"
#load(file="H:/FLR/WGBAST18/WGBAST_JAGS_new_SR.RData"); SRnew<-"yes"

#load(file="H:/FLR/WGBAST18/new_SR_long2018-03-19.RData"); SRnew<-"yes"
#traceplot(chains[,"K[1]"])

load(file="H:/FLR/WGBAST18/new_SR_HRR2018-03-22.RData"); SRnew<-"yes"
#traceplot(chains[,"K[1]"])

#chains<-as.mcmc.list(run1)



