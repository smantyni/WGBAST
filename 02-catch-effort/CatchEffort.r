## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:   Combine information from different countries

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
library(tidyverse)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)
library(gridExtra)


min_year<-2000
max_year<-2018
years<-min_year:max_year
NumYears<-length(years)

pathIn<-"H:/Biom/FullLifeHistoryModel/2019/"

df<-read_xlsx(str_c(pathIn, "dat/orig/catch&effort/WGBAST_Catch_2019_St_Petersburg_15022018.xlsx"),
              range="A1:Q12996", # Update!
              sheet="Catch data", col_names = T, guess_max = 8000, na=c("",".", "NaN"))%>%
  filter(YEAR>2008)%>% # Include results only 2009 onwards, catch DB has only updates from those years 
  mutate(NUMB=parse_double(NUMB))%>%
  select(SPECIES, COUNTRY, YEAR, TIME_PERIOD, TP_TYPE, sub_div2, FISHERY, F_TYPE, GEAR, NUMB, EFFORT, everything())%>%
  mutate(TP_TYPE=ifelse(TP_TYPE=="QRT", "QTR", TP_TYPE))

(tmpx<-df%>%filter(SPECIES=="SAL",F_TYPE=="RECR", FISHERY=="R", COUNTRY=="LV", YEAR==2018))
tmpx%>%distinct()

View(tmpx)

df<-df%>%distinct() #Remove duplicate rows

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

df%>%filter(is.na(SUB_DIV)==T) 
# There's RECR AN without SUBDIV specified! Will be removed in next unless NA's replaced.
df<-df%>%mutate(SUB_DIV=ifelse(is.na(SUB_DIV)==T, 1, SUB_DIV))
# 8326

df2<-df%>%filter(SUB_DIV!=32, F_TYPE!="DISC", F_TYPE!="SEAL",F_TYPE!="ALV",F_TYPE!="BMS",F_TYPE!="BROODSTOCK")
# 8326

MON<-df2%>%filter(TP_TYPE=="MON")%>%mutate(HYR=ifelse(TIME_PERIOD<7,1,2))
QTR<-df2%>%filter(TP_TYPE=="QTR")%>%mutate(HYR=ifelse(TIME_PERIOD<3,1,2))
HYR<-df2%>%filter(TP_TYPE=="HYR")%>%mutate(HYR=ifelse(TIME_PERIOD<2,1,2))
YR<-df2%>%filter(TP_TYPE=="YR")%>%mutate(HYR="NA")%>%mutate(HYR=parse_double(HYR))

df2<-full_join(MON, QTR)%>%
  full_join(HYR)%>%
  full_join(YR)%>%
  select(SPECIES, COUNTRY, YEAR, TIME_PERIOD, TP_TYPE, HYR, sub_div2, FISHERY, F_TYPE, GEAR, NUMB, EFFORT, everything())
# 8326

salmon<-df2%>%filter(SPECIES=="SAL")
#3546

source("02-catch-effort/WGBAST_DB_Germany.r")
source("02-catch-effort/WGBAST_DB_Latvia.r")
source("02-catch-effort/WGBAST_DB_Denmark.r")
source("02-catch-effort/WGBAST_DB_Finland.r")
source("02-catch-effort/WGBAST_DB_Sweden.r")
source("02-catch-effort/WGBAST_DB_CPUEoffshore.r")

# Choose method to calculate PL catch!
# ===================

# Current WGBAST version: 
# 2000-2008 estimated catch based on PL effort and 0.75*CPUE[other countries] 
# 2009-> reported SAL+TRS catch *0.97 
# ===================
 pl<-1;Feff<-0.75 

# Full misreporting version; estimated catch based on PL effort and 1*CPUE[other countries]
# ===================
#pl<-2;Feff<-1 

source("02-catch-effort/WGBAST_DB_Poland.r")

# ==============================================================================

#############
## Catch ##
#############

# choose above method for estimating PL catch

##################
# ODN catch
##################


ifelse(pl==1,
       PolC_ODNx<-full_join(
         filter(PolC_ODN_rep, YEAR>2008),
         filter(PolC_ODN_est, YEAR<2009)%>%
           select(YEAR, HYR, Catch)),
       
       PolC_ODNx<-PolC_ODN_est%>%
         select(YEAR, HYR, Catch)
)

ODN<-full_join(Lat_ODN, Den_ODN)%>%
  full_join(Fin_ODN)%>%
  full_join(Swe_ODN)%>%
  full_join(PolC_ODNx)

ODN%>%select(-Effort)%>%
  mutate(Myear=ifelse(HYR==2, YEAR, YEAR-1))%>% # Model year!
  ungroup()%>%
  group_by(Myear)%>%
  summarise(Catch=sum(Catch)) # if there were NA's you'd see it here

# ignore minor 2014 catch
# This produces a bit strange results, especially a huge catch for 2001 in both methods. Check,
# but in the mean time just leave ODN as it is in the Catch&Effort file.

##################
# OLL catch
##################


ifelse(pl==1,
       PolC_OLLx<-full_join(
         filter(PolC_OLL_rep, YEAR>2008),
         full_join(PolC_OLL_est,PolC_OT_rep)%>%
           filter(YEAR<2009)%>%
           select(YEAR, HYR, Catch)),
       
        PolC_OLLx<-full_join(PolC_OLL_est,PolC_OT_rep)%>%
         group_by(YEAR, HYR)%>%
         summarise(Catch=sum(Catch))
       )

OLL<-full_join(Ger_OLL, Den_OLL)%>%
  full_join(Fin_OLL)%>%
  full_join(Swe_OLL)%>%
  full_join(PolC_OLLx)
#View(OLL)

# Check that there's no missing catches:
OLL%>%filter(is.na(Catch==T))

(OLL_C<-OLL%>%select(-Effort)%>%
  mutate(Myear=ifelse(HYR==2, YEAR, YEAR-1))%>% # Model year!
  ungroup()%>%
  group_by(Myear)%>%
  summarise(Catch=sum(Catch))) # if there were NA's you'd see it here

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
FinC_OTtot%>%
  mutate(Catch=Catch_COT)%>%
  full_join(Swe_COT)%>%
  select(-Effort, -CPUE, -Catch_COT)%>%
  ungroup()%>%
  group_by(YEAR)%>%
  summarise(Catch=sum(Catch, na.rm=T))

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
#View(OLL)

(OLL_E<-OLL%>%select(-Catch)%>%
  mutate(Myear=ifelse(HYR==2, YEAR, YEAR-1))%>% # Model year!
  ungroup()%>%
  group_by(Myear)%>%
  summarise(Effort=round(sum(Effort, na.rm=T))) )


##################
# CTN effort
##################

FinE30_CTN<-select(Fin_CTN30, YEAR, HYR, Effort)%>%
  summarise(Effort=sum(Effort))

FinE31_CTN<-select(Fin_CTN31, YEAR, HYR, Effort)%>%
  summarise(Effort=sum(Effort))

# 2013-> use reported Swe FYK effort
SweE30_CTN<-select(Swe_CTN30, YEAR, HYR, Effort)%>%
  ungroup()%>%group_by(YEAR)%>%
  summarise(Effort=sum(Effort))%>%
  full_join(SweE_CTN_rep30)%>%
  mutate(Effort2=ifelse(YEAR>2012,Effort_rep,Effort))%>%
  mutate(Effort=Effort2)%>%
  select(YEAR,Effort)

SweE31_CTN<-select(Swe_CTN31, YEAR, HYR, Effort)%>%
  ungroup()%>%group_by(YEAR)%>%
  summarise(Effort=sum(Effort))%>%
  full_join(SweE_CTN_rep31)%>%
  mutate(Effort2=ifelse(YEAR>2012,Effort_rep,Effort))%>%
  mutate(Effort=Effort2)%>%
  select(YEAR,Effort)

CTN_AU1<-round(FinE30_CTN[,2]+FinE31_CTN[,2]+0.45*SweE31_CTN[,2])
CTN_AU2<-round(FinE30_CTN[,2]+0.55*SweE31_CTN[,2])
CTN_AU3<-round(FinE30_CTN[,2]+SweE30_CTN[,2])

yearx<-SweE31_CTN$YEAR
cbind(yearx,CTN_AU1, CTN_AU2, CTN_AU3)


##################
# COT effort
##################

FinE30_COT<-select(Fin_COT30, YEAR, HYR, Effort)%>%
  summarise(Effort=sum(Effort))

FinE31_COT<-select(Fin_COT31, YEAR, HYR, Effort)%>%
  summarise(Effort=sum(Effort))

SweE30_COT<-select(Swe_COT30, YEAR, HYR, Effort)%>%
  summarise(Effort=sum(Effort))%>%
  filter(YEAR>2002)

SweE31_COT<-select(Swe_COT31, YEAR, HYR, Effort)%>%
  summarise(Effort=sum(Effort))%>%
  filter(YEAR>2002)

yearx<-SweE31_COT$YEAR

COT_AU1<-round(FinE30_COT[,2]+FinE31_COT[,2]+0.45*SweE31_COT[,2])
COT_AU2<-round(FinE30_COT[,2]+0.55*SweE31_COT[,2])
COT_AU3<-round(FinE30_COT[,2]+SweE30_COT[,2])

cbind(yearx,COT_AU1, COT_AU2, COT_AU3)

#
# RECR Trolling catch
#########################

salmon%>%
  filter(YEAR==2018, F_TYPE=="RECR", FISHERY=="O", GEAR=="AN")%>%
  summarise(Catch=sum(NUMB))
  
salmon%>%
  filter(YEAR==2018,F_TYPE=="RECR", FISHERY=="C")%>%
  summarise(Catch=sum(NUMB))
