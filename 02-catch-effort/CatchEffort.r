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

source("catch-effort/WGBAST_DB_functions.r")


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
#tmp<-df%>%
#  mutate(TP_TYPE=factor(TP_TYPE, levels=))

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

salmon<-filter(df, SPECIES=="SAL", SUB_DIV!=32, F_TYPE!="DISC", F_TYPE!="SEAL")


source("catch-effort/WGBAST_DB_Germany.r")
source("catch-effort/WGBAST_DB_Latvia.r")
source("catch-effort/WGBAST_DB_Denmark.r")
source("catch-effort/WGBAST_DB_Finland.r")
source("catch-effort/WGBAST_DB_Finland_CoastalCPUE.r")
source("catch-effort/WGBAST_DB_Sweden.r")
source("catch-effort/WGBAST_DB_Poland_CPUEothers.r")
source("catch-effort/WGBAST_DB_Poland_new.r")

# ==============================================================================

                         #############
                         ## Catches ##
                         #############

##################
# OLL catch
##################
# OLL series stats from 2003, add NA's for years 2000-2002
PolC_OLLxx<-rbind(array(c(2000:2002, rep(NA,6)),dim=c(3,3)),PolC_OLLx)
for(i in 1:length(PolC_OLLxx)){
  if(is.na(PolC_OLLxx[i])==T){PolC_OLLxx[i]<-0}
}

# 20.2.2017: Polish data not yet available for 2016, assume the same catch as in 2015
#PolC_OLLxx
#PolC_OLLxx[17,2:3]<-PolC_OLLxx[16,2:3]


C_OLL_tot<-c()
for(i in 1:(length(years)-1)){
  C_OLL_tot[i]<-FinC_OLLx[i,3]+FinC_OLLx[i+1,2]+SweC_OLLx[i,3]+SweC_OLLx[i+1,2]+GerCx[i,3]+GerCx[i+1,2]+
              DenC_OLLx[i,3]+DenC_OLLx[i+1,2]+PolC_OLLxx[i,3]+PolC_OLLxx[i+1,2]
}
round(cbind(years[1:(length(years)-1)], C_OLL_tot),0)


##################
# CTN catch
##################

C_CTN_tot<-c()
for(i in 1:length(years)){
  C_CTN_tot[i]<-FinC_CTNx[i,2]+FinC_CTNx[i,3]+SweC_CTNx[i,2]+SweC_CTNx[i,3]
}
round(cbind(years, C_CTN_tot),0)

##################
# COT catch
##################

C_COT_tot<-c()
for(i in 1:length(years)){
  C_COT_tot[i]<-FinC_CNAandOTx[i,2]+FinC_CNAandOTx[i,3]+SweC_COTx[i,2]+SweC_COTx[i,3]
}
round(cbind(years, C_COT_tot),0)

##################
# River catch
##################

cbind(years, FinC_riverx[,2]+SweC_riverx[,2])


################################################################################
                         #############
                         ## Efforts ##
                         #############

##################
# OLL effort
##################
# OLL series stats from 2003, add NA's for years 2000-2002
PolE_OLLxx<-rbind(array(c(2000:2002, rep(NA,6)),dim=c(3,3)),PolE_OLLx)
for(i in 1:length(PolE_OLLxx)){
  if(is.na(PolE_OLLxx[i])==T){PolE_OLLxx[i]<-0}
}

# 22.2.2017: Polish effort not available for 2016, use 2015 cpue and 2016 catch
#PolE_OLLxx
#CPUE_PL_1<-mean(PolC_OLLxx[12:16,2]/PolE_OLLxx[12:16,2])
#CPUE_PL_2<-mean(PolC_OLLxx[12:16,3]/PolE_OLLxx[12:16,3])

#PolE_OLLxx[17,2]<-PolC_OLLxx[17,2]/CPUE_PL_1
#PolE_OLLxx[17,3]<-PolC_OLLxx[17,3]/CPUE_PL_2
PolE_OLLxx

E_OLL_tot<-c()
E_OLL_tot_DK<-c()
E_OLL_tot_PL<-c()
for(i in 1:(length(years)-1)){
  E_OLL_tot[i]<-FinE_OLLx[i,3]+FinE_OLLx[i+1,2]+SweE_OLLx[i,3]+SweE_OLLx[i+1,2]+
              DenE_OLLx[i,3]+DenE_OLLx[i+1,2]+PolE_OLLxx[i,3]+PolE_OLLxx[i+1,2]
  E_OLL_tot_DK[i]<-DenE_OLLx[i,3]+DenE_OLLx[i+1,2]
  E_OLL_tot_PL[i]<-PolE_OLLxx[i,3]+PolE_OLLxx[i+1,2]
}
cbind(years[1:(length(years)-1)], E_OLL_tot)
cbind(years[1:(length(years)-1)], E_OLL_tot_DK/100000) # scenarios
cbind(years[1:(length(years)-1)], E_OLL_tot_PL/100000)


##################
# CTN effort
##################

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
