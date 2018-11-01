## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:   Combine information from different countries together in R

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
library(tidyverse)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)
library(gridExtra)

source("02-catch-effort/WGBAST_DB_functions.r")


min_year<-2000
max_year<-2017
years<-min_year:max_year
NumYears<-length(years)

pathIn<-"H:/Biom/FullLifeHistoryModel/2018/"

df<-read_xlsx(str_c(pathIn, "dat/orig/catch&effort/WGBAST_Catch_2018_Turku_20022018.xlsx"),
                   range="A1:R9113", # NOTE!!!! UPDATE!!!!
              sheet="Catch data", col_names = T, guess_max = 8000, na=c("",".", "NaN"))

df<-df%>%
  filter(YEAR>2000)%>%
  mutate(NUMB=parse_double(NUMB))%>%
  select(SPECIES, COUNTRY, YEAR, TIME_PERIOD, TP_TYPE, sub_div2, FISHERY, F_TYPE, GEAR, NUMB, EFFORT, everything())
 
df%>%count(TP_TYPE) 

tmp<-df%>%group_by(FISHERY)%>%
  count(GEAR)
#View(tmp)
# https://datacollection.jrc.ec.europa.eu/wordef/gear-type
# GND: driftnets (previously DN, banned since 2008)
# LLD: longlines
# FYK: fyke nets (previously TN)
# MIS: miscellaneous (previously OT)
# GNS: gillnet (stationary) (previously GN)
# AN: angling

df2<-df%>%filter((SPECIES=="SAL" | SPECIES=="TRS"), SUB_DIV!=32, F_TYPE!="DISC", F_TYPE!="SEAL")
#3640

MON<-df2%>%filter(TP_TYPE=="MON")%>%mutate(HYR=ifelse(TIME_PERIOD<7,1,2))
QTR<-df2%>%filter(TP_TYPE=="QTR")%>%mutate(HYR=ifelse(TIME_PERIOD<3,1,2))
HYR<-df2%>%filter(TP_TYPE=="HYR")%>%mutate(HYR=ifelse(TIME_PERIOD<2,1,2))
YR<-df2%>%filter(TP_TYPE=="YR")%>%mutate(HYR="NA")%>%mutate(HYR=parse_double(HYR))

df2<-full_join(MON, QTR)%>%
  full_join(HYR)%>%
  full_join(YR)%>%
  select(SPECIES, COUNTRY, YEAR, TIME_PERIOD, TP_TYPE, HYR, sub_div2, FISHERY, F_TYPE, GEAR, NUMB, EFFORT, everything())

salmon<-df2%>%filter(SPECIES=="SAL")



source("02-catch-effort/WGBAST_DB_Germany.r")
source("02-catch-effort/WGBAST_DB_Latvia.r")
source("02-catch-effort/WGBAST_DB_Denmark.r")
source("02-catch-effort/WGBAST_DB_Finland.r")
#source("02-catch-effort/WGBAST_DB_Finland_CoastalCPUE.r")
source("02-catch-effort/WGBAST_DB_Sweden.r")
source("02-catch-effort/WGBAST_DB_CPUEoffshore.r")

source("02-catch-effort/WGBAST_DB_Poland.r")
#source("02-catch-effort/WGBAST_DB_Poland_CPUEothers.r")
#source("02-catch-effort/WGBAST_DB_Poland_new.r")

# ==============================================================================

#############
## Catch ##
#############

##################
# OLL catch
##################

OLL<-full_join(Ger_OLL, Den_OLL)%>%
  full_join(Fin_OLL)%>%
  full_join(Swe_OLL)%>%
  full_join(PolC_OLL)
#View(OLL)

# Check that there's no missing catches:
OLL%>%filter(is.na(Catch==T))

OLL%>%select(-Effort)%>%
  mutate(Myear=ifelse(HYR==2, YEAR, YEAR-1))%>% # Model year!
  ungroup()%>%
  group_by(Myear)%>%
  summarise(Catch=sum(Catch)) # if there were NA's you'd see it here

##################
# CTN catch
##################

full_join(Swe_CTN,
  select(Fin_CTN, -CPUE))%>%
  select(-Effort)%>%
  ungroup()%>%
  group_by(YEAR)%>%
  summarise(Catch=sum(Catch))


##################
# COT catch
##################

full_join(Swe_COT,
          select(Fin_COT, -CPUE))%>%
  select(-Effort)%>%
  ungroup()%>%
  group_by(YEAR)%>%
  summarise(Catch=sum(Catch))

##################
# River catch
##################

full_join(Swe_R,Fin_R)%>%
  summarise(Catch=sum(Catch))



################################################################################
                         #############
                         ## Efforts ##
                         #############


##################
# OLL effort
##################

# Ignore that Ger and Swe (2012->) effort data are missing. Catches are quite low.

OLL<-full_join(Ger_OLL, Den_OLL)%>%
  full_join(Fin_OLL)%>%
  full_join(Swe_OLL)%>%
  full_join(PolE_OLL)
#View(Swe_OLL)

OLL%>%select(-Catch)%>%
  mutate(Myear=ifelse(HYR==2, YEAR, YEAR-1))%>% # Model year!
  ungroup()%>%
  group_by(Myear)%>%
  summarise(Effort=round(sum(Effort, na.rm=T))) 


##################
# CTN effort
##################

FinE30<-select(Fin_CTN30, YEAR, HYR, Effort)%>%
  summarise(Effort=sum(Effort))
FinE31<-select(Fin_CTN31, YEAR, HYR, Effort)%>%
  summarise(Effort=sum(Effort))
SweE30<-select(Swe_CTN30, YEAR, HYR, Effort)%>%
  summarise(Effort=sum(Effort))
SweE31<-select(Swe_CTN31, YEAR, HYR, Effort)%>%
  summarise(Effort=sum(Effort))

CTN_AU1<-FinE30+FinE31+0.45*SweE31
CTN_AU2<-FinE30+0.55*SweE31
CTN_AU3<-FinE30+SweE30




E_CTN_AU1<-c()
E_CTN_AU2<-c()
E_CTN_AU3<-c()
for(i in 1:13){ #2012
  E_CTN_AU1[i]<-FinE_CTN30x[i,2]+FinE_CTN30x[i,3]+
                FinE_CTN31x[i,2]+FinE_CTN31x[i,3]+
          0.45*(SweE_CTN31x[i,2]+SweE_CTN31x[i,3])

  E_CTN_AU2[i]<-FinE_CTN30x[i,2]+FinE_CTN30x[i,3]+
          0.55*(SweE_CTN31x[i,2]+SweE_CTN31x[i,3])

  E_CTN_AU3[i]<-FinE_CTN30x[i,2]+FinE_CTN30x[i,3]+
                SweE_CTN30x[i,2]+SweE_CTN30x[i,3]

}
# 2013-> account for real swedish data
for(i in 14:length(years)){
  E_CTN_AU1[i]<-FinE_CTN30x[i,2]+FinE_CTN30x[i,3]+
                FinE_CTN31x[i,2]+FinE_CTN31x[i,3]+
          0.45*(SweE_CTN31x_real[i,2]+SweE_CTN31x_real[i,3])

  E_CTN_AU2[i]<-FinE_CTN30x[i,2]+FinE_CTN30x[i,3]+
          0.55*(SweE_CTN31x_real[i,2]+SweE_CTN31x_real[i,3])

  E_CTN_AU3[i]<-FinE_CTN30x[i,2]+FinE_CTN30x[i,3]+
                SweE_CTN30x_real[i,2]+SweE_CTN30x_real[i,3]

}
round(cbind(years, E_CTN_AU1),0)
round(cbind(years, E_CTN_AU2),0)
round(cbind(years, E_CTN_AU3),0)

# Country specific
E_CTN_FI30<-c()
E_CTN_FI31<-c()
E_CTN_SE30<-c()
E_CTN_SE31<-c()
for(i in 1:length(years)){ #2012
  E_CTN_FI30[i]<-FinE_CTN30x[i,2]+FinE_CTN30x[i,3]
  E_CTN_FI31[i]<-FinE_CTN31x[i,2]+FinE_CTN31x[i,3]
}  
for(i in 1:13){ #2012
  E_CTN_SE30[i]<-SweE_CTN30x[i,2]+SweE_CTN30x[i,3]
  E_CTN_SE31[i]<-SweE_CTN31x[i,2]+SweE_CTN31x[i,3]
}
for(i in 14:length(years)){
  E_CTN_SE30[i]<-SweE_CTN30x_real[i,2]+SweE_CTN30x_real[i,3]
  E_CTN_SE31[i]<-SweE_CTN31x_real[i,2]+SweE_CTN31x_real[i,3]
}
cbind(E_CTN_FI30/1000,E_CTN_SE30/1000,E_CTN_FI31/1000,E_CTN_SE31/1000)


##################
# COT effort
##################

E_COT_AU1<-c()
for(i in 1:length(years)){
  E_COT_AU1[i]<-FinE_COT30x[i,2]+FinE_COT30x[i,3]+
                FinE_COT31x[i,2]+FinE_COT31x[i,3]+
          0.45*(SweE_COT31x[i,2]+SweE_COT31x[i,3])

}
round(cbind(years, E_COT_AU1),0)

E_COT_AU2<-c()
for(i in 1:length(years)){
  E_COT_AU2[i]<-FinE_COT30x[i,2]+FinE_COT30x[i,3]+
          0.55*(SweE_COT31x[i,2]+SweE_COT31x[i,3])

}
round(cbind(years, E_COT_AU2),0)

E_COT_AU3<-c()
for(i in 1:length(years)){
  E_COT_AU3[i]<-FinE_COT30x[i,2]+FinE_COT30x[i,3]+
                SweE_COT30x[i,2]+SweE_COT30x[i,3]

}
round(cbind(years, E_COT_AU3),0)

# Country specific
E_COT_FI30<-c()
E_COT_FI31<-c()
E_COT_SE30<-c()
E_COT_SE31<-c()
for(i in 1:length(years)){ 
  E_COT_FI30[i]<-FinE_COT30x[i,2]+FinE_COT30x[i,3]
  E_COT_FI31[i]<-FinE_COT31x[i,2]+FinE_COT31x[i,3]
  E_COT_SE30[i]<-SweE_COT30x[i,2]+SweE_COT30x[i,3]
  E_COT_SE31[i]<-SweE_COT31x[i,2]+SweE_COT31x[i,3]
}  
cbind(E_COT_FI30/100000,E_COT_SE30/100000,E_COT_FI31/100000,E_COT_SE31/100000)



# Offshore driftnetting, in case needed

##################
# ODN effort
##################
for(i in 1:length(PolE_ODNx)){
  if(is.na(PolE_ODNx[i])==T){PolE_ODNx[i]<-0}
}
PolE_ODNx

E_ODN_tot<-c()
for(i in 1:(length(years)-1)){
  E_ODN_tot[i]<-FinE_ODNx[i,3]+FinE_ODNx[i+1,2]+SweE_ODNx[i,3]+SweE_ODNx[i+1,2]+
              DenE_ODNx[i,3]+DenE_ODNx[i+1,2]+LatE_ODNx[i,3]+LatE_ODNx[i+1,2]+
              PolE_ODNx[i,3]+PolE_ODNx[i+1,2]
}
cbind(years[1:(length(years)-1)], E_ODN_tot)

##################
# ODN catch
##################
for(i in 1:length(PolC_ODNx)){
  if(is.na(PolC_ODNx[i])==T){PolC_ODNx[i]<-0}
}
PolC_ODNx

C_ODN_tot<-c()

for(i in 1:(length(years)-1)){
  C_ODN_tot[i]<-FinC_ODNx[i,3]+FinC_ODNx[i+1,2]+SweC_ODNx[i,3]+SweC_ODNx[i+1,2]+
              DenC_ODNx[i,3]+DenC_ODNx[i+1,2]+LatC_ODNx[i,3]+LatC_ODNx[i+1,2]+
              PolC_ODNx[i,3]+PolC_ODNx[i+1,2]
}
cbind(years[1:(length(years)-1)], C_ODN_tot)
