## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Finland

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


finland<-filter(salmon, COUNTRY=="FI")

finland%>%
  group_by(FISHERY)%>%
  count(TP_TYPE)
# YR catch is RECR MIS at coast & river

finland%>%
  group_by(FISHERY)%>%
  count(GEAR)

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
################################################################################
#  Offshore fishery                                                                  
################################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~

################################################################################
#  Offshore driftnetting:                                                                  
################################################################################

Fin_ODN<-finland%>%
  filter(FISHERY=="S", GEAR=="GND")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))

#                    
# fin_offs<-filter(finland, FISHERY=="S")
# FinODN<-subset(fin_offs, GEAR=="GND")
# 
# 
# ##############
# # Effort
# ##############
# FinE_ODN<-Effort_HYR(FinODN)
# FinE1_ODN<-FinE_ODN[,2]; FinE2_ODN<-FinE_ODN[,3]
# 
# FinE_ODN<-round(GatherHalfYears(FinE1_ODN,FinE2_ODN,NumYears),0)
# FinE_ODN
# 
# FinE_ODNx<-cbind(years,FinE1_ODN,FinE2_ODN) 
# 
# ##############
# # Catches
# ##############
# FinC_ODN<-Catch_HYR(FinODN)
# FinC1_ODN<-FinC_ODN[,2]; FinC2_ODN<-FinC_ODN[,3]
# FinC1_ODN
# FinC2_ODN
# FinC_ODN
# 
# FinC_ODN<-round(GatherHalfYears(FinC1_ODN,FinC2_ODN,NumYears),0)
# FinC_ODN
# 
# FinC_ODNx<-cbind(years,FinC1_ODN,FinC2_ODN) 
# 
# ################################################################################
#  Offshore longlining:
################################################################################


Fin_OLL<-finland%>%
  filter(FISHERY=="S", GEAR=="LLD")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))
#View(Fin_OLL)
# 
# FinOLL<-subset(fin_offs, GEAR=="LLD")
# FinOLL%>%count(TP_TYPE)
# 
# dim(FinOLL)[1]
# 
# ##############
# # Effort
# ##############
# FinE_OLL<-Effort_HYR(FinOLL)
# FinE1_OLL<-FinE_OLL[,2]; FinE2_OLL<-FinE_OLL[,3]
# 
# FinE_OLL<-round(GatherHalfYears(FinE1_OLL,FinE2_OLL,NumYears),0)
# FinE_OLL
# 
# FinE_OLLx<-cbind(years,FinE1_OLL,FinE2_OLL) 
# 
# ##############
# # Catches
# ##############
# FinC_OLL<-Catch_HYR(FinOLL)
# FinC1_OLL<-FinC_OLL[,2]; FinC2_OLL<-FinC_OLL[,3]
# 
# FinC_OLL<-round(GatherHalfYears(FinC1_OLL,FinC2_OLL,NumYears),0)
# FinC_OLL
# 
# FinC_OLLx<-cbind(years,FinC1_OLL,FinC2_OLL) 

################################################################################
#  Other offshore gear (OT)
################################################################################
# These will be added to commercial coastal other gear

Fin_OOT<-finland%>%
  filter(FISHERY=="S", GEAR=="MIS")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_OOT=round(sum(NUMB, na.rm=T)),
            Effort_OOT=round(sum(EFFORT, na.rm=T)))
#View(Fin_OOT)
# 
# FinOffsOT<-subset(fin_offs, GEAR=="MIS")
# FinOffsOT%>%count(TP_TYPE)
# 
# ##############
# # Effort
# ##############
# FinE_OffsOT<-Effort_HYR(FinOffsOT)
# FinE1_OffsOT<-FinE_OffsOT[,2]; FinE2_OffsOT<-FinE_OffsOT[,3]
# FinE1_OffsOT
# FinE2_OffsOT
# FinE_OffsOT
# 
# ##############
# # Catches
# ##############
# FinC_OffsOT<-Catch_HYR(FinOffsOT)
# FinC1_OffsOT<-FinC_OffsOT[,2]; FinC2_OffsOT<-FinC_OffsOT[,3]
# FinC1_OffsOT
# FinC2_OffsOT
# FinC_OffsOT

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
################################################################################
#  Coastal fishery                                                                  
################################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~

finland%>%
  filter(FISHERY=="C")%>%
  count(GEAR)

# Groups in coastal catch and effort for Finland are
# CDN (GND), CTN (FYK) and Coastal OT (MIS). 
# What to do with LLD=14?
# Because NA's RECR => put those with GEAR=MIS (Tapani)

finland%>%
  filter(FISHERY=="C")%>%
  group_by(GEAR, F_TYPE)%>%
  count(TP_TYPE)

# YR data is at RECR MIS
# -> need to combine HYR (COMM) & YR (RECR) MIS

################################################################################
#  Coastal driftnetting:                                                                  
################################################################################


Fin_CDN<-finland%>%
  filter(FISHERY=="C", GEAR=="GND")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))

# 
# 
#  fin_coast<-subset(finland, FISHERY=="C")
#  FinCDN<-subset(fin_coast, GEAR=="GND")
# FinCDN%>%count(TP_TYPE)
# # Only HYR data!
# 
# ##############
# # Effort
# ##############
# FinE_CDN<-Effort_HYR(FinCDN)
# FinE1_CDN<-FinE_CDN[,2]; FinE2_CDN<-FinE_CDN[,3]
# FinE1_CDN
# FinE2_CDN
# FinE_CDN
# 
# FinE_CDN<-round(GatherHalfYears(FinE1_CDN,FinE2_CDN,NumYears),0)
# FinE_CDN
# 
# ##############
# # Catches
# ##############
# FinC_CDN<-Catch_HYR(FinCDN)
# FinC1_CDN<-FinC_CDN[,2]; FinC2_CDN<-FinC_CDN[,3]
# FinC1_CDN
# FinC2_CDN
# FinC_CDN
# 
# FinC_CDN<-round(GatherHalfYears(FinC1_CDN,FinC2_CDN,NumYears),0)
# FinC_CDN

################################################################################
#  Coastal trapnetting:
################################################################################

Fin_CTN<-finland%>%
  filter(FISHERY=="C", GEAR=="FYK")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))
#View(Fin_CTN)

# 
# FinCTN<-subset(fin_coast, GEAR=="FYK")
# 
# FinCTN%>%count(TP_TYPE)
# # Only HYR data!
# 
# ##############
# # Effort
# ##############
# FinE_CTN<-Effort_HYR(FinCTN)
# FinE1_CTN<-FinE_CTN[,2]; FinE2_CTN<-FinE_CTN[,3]
# 
# FinE_CTN<-round(GatherHalfYears(FinE1_CTN,FinE2_CTN,NumYears),0)
# FinE_CTN
# 
# FinE_CTNx<-cbind(years, FinE1_CTN,FinE2_CTN)
# 
# ####################
# # Catches
# ####################
# FinC_CTN<-Catch_HYR(FinCTN)
# FinC1_CTN<-FinC_CTN[,2]; FinC2_CTN<-FinC_CTN[,3]
# 
# FinC_CTN<-round(GatherHalfYears(FinC1_CTN,FinC2_CTN,NumYears),0)
# FinC_CTN
# 
# FinC_CTNx<-cbind(years, FinC1_CTN,FinC2_CTN)

################################################################################
#  Other coastal gear (OT)
################################################################################

finland%>%
  filter(FISHERY=="C", GEAR=="MIS"| GEAR=="LLD")%>%
  group_by(GEAR)%>%
  count(TP_TYPE)
# MIS : HYR & YR


Fin_COT<-finland%>%
  filter(FISHERY=="C", GEAR=="MIS"| GEAR=="LLD", TP_TYPE=="HYR")%>% # take HYR here and include YR later 
  group_by(YEAR, HYR)%>%
  summarise(Catch_COT=round(sum(NUMB, na.rm=T)),
            Effort_COT=round(sum(EFFORT, na.rm=T)))
#View(Fin_COT)


# tmp<-finland%>%
#   filter(FISHERY=="C", GEAR=="MIS"| GEAR=="LLD", TP_TYPE=="YR")
# View(tmp)


# FinCoastOT<-subset(fin_coast, GEAR=="MIS" | GEAR=="LLD")
# FinCoastOT<-filter(fin_coast, GEAR=="MIS" | GEAR=="LLD", TP_TYPE=="HYR")# take HYR here and include YR later 
# 
# ##############
# # Effort
# ##############
# FinE_CoastOT<-Effort_HYR(FinCoastOT)
# FinE1_CoastOT<-FinE_CoastOT[,2]; FinE2_CoastOT<-FinE_CoastOT[,3]
# 
# FinE_CoastOT<-round(GatherHalfYears(FinE1_CoastOT,FinE2_CoastOT,NumYears),0)
# FinE_CoastOT
# 
# ####################
# # Catches
# ####################
# FinC_CoastOT<-Catch_HYR(FinCoastOT)
# FinC1_CoastOT<-FinC_CoastOT[,2]; FinC2_CoastOT<-FinC_CoastOT[,3]
# 
# FinC_CoastOT<-round(GatherHalfYears(FinC1_CoastOT,FinC2_CoastOT,NumYears),0)
# FinC_CoastOT
# 

###########################################
# Combine coastal and offshore other gear
###########################################
Fin_OTtot<-full_join(Fin_OOT, Fin_COT)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_COT_tot=sum(Catch_OOT,Catch_COT, na.rm=T),
         Effort_COT_tot=sum(Effort_OOT,Effort_COT, na.rm=T))%>%
  mutate(Catch_COT=Catch_COT_tot,
         Effort_COT=Effort_COT_tot)%>%
  select(YEAR, HYR, Catch_COT, Effort_COT)
#View(Fin_OTtot)

# FinE1_OTtot<-FinE1_CoastOT+FinE1_OffsOT
# FinE2_OTtot<-FinE2_CoastOT+FinE2_OffsOT
# 
# FinE_OTtot<-round(GatherHalfYears(FinE1_OTtot,FinE2_OTtot,NumYears),0)
# FinE_OTtot

# FinC1_OTtot<-FinC1_CoastOT+FinC1_OffsOT
# FinC2_OTtot<-FinC2_CoastOT+FinC2_OffsOT
# 
# FinC_OTtot<-round(GatherHalfYears(FinC1_OTtot,FinC2_OTtot,NumYears),0)
# FinC_OTtot

################################################################################
#  Coastal gear = MIS, TP_TYPE= YR
# THIS WAS PREVIOUSLY COASTAL GEAR==NA!!!
################################################################################

finland%>%
  filter(FISHERY=="C", GEAR=="MIS"| GEAR=="LLD", TP_TYPE=="YR")%>%
  group_by(GEAR)%>%
  count(TP_TYPE)

Fin_COT_YR<-finland%>%
  filter(FISHERY=="C", GEAR=="MIS"| GEAR=="LLD", TP_TYPE=="YR")%>% 
  group_by(YEAR, HYR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T))) # only catch data


# 
# fin_coast%>%count(TP_TYPE)
# filter(fin_coast, TP_TYPE=="YR")
# # This is done only for the catch (effort isn't available) 
# 
# FinCoastNA<-filter(fin_coast, TP_TYPE=="YR")
# FinCoastNA%>%count(TP_TYPE)
# # only YR data (16)
# 
# ####################
# # Catches
# ####################
# Catch<-vector()
# 
# for(y in min_year:max_year){
# 	temp<-0
# 	for(i in 1:dim(FinCoastNA)[1]){
#     if (FinCoastNA$YEAR[i]==y){
# 		  if(is.na(FinCoastNA$NUMB[i])==F){temp<-FinCoastNA$NUMB[i]+temp}
#     }
# 	}
# 	Catch[(y-min_year+1)]<-temp
# }
# FinC_CNA<-Catch
# cbind(years,Catch)

################################################################################
# Combine OT catches and NA catches

tmp1<-Fin_OTtot%>%
  select(-Effort_COT)%>%
  ungroup()%>%
  group_by(YEAR, add=T)%>%
  mutate(sumC=sum(Catch_COT))%>%
  mutate(p=Catch_COT/sumC)

tmp2<-Fin_COT_YR%>%
  mutate(Catch_COT_YR=Catch_YR)%>%
  select(YEAR, Catch_COT_YR)
  
FinC_OTtot<-full_join(tmp1, tmp2, by="YEAR")%>%
  mutate(Catch_COT_YR2=Catch_COT_YR*p)%>%
  mutate(Catch_COT_HYR=Catch_COT)%>%
  mutate(Catch_COT=round(Catch_COT_HYR+Catch_COT_YR2))%>%
  select(YEAR, HYR, Catch_COT)
#View(FinC_OTtot)
  
# FinC1_OTtot
# FinC2_OTtot
# 
# propC1_COT<-FinC1_OTtot/(FinC1_OTtot+FinC2_OTtot)
# propC2_COT<-FinC2_OTtot/(FinC1_OTtot+FinC2_OTtot)
# 
# FinC1_CNA<-FinC_CNA*propC1_COT
# FinC2_CNA<-FinC_CNA*propC2_COT
# 
# FinC1_CNAandOT<-FinC1_CNA+FinC1_OTtot
# FinC2_CNAandOT<-FinC2_CNA+FinC2_OTtot
# 
# FinC_CNAandOT<-round(GatherHalfYears(FinC1_CNAandOT,FinC2_CNAandOT,NumYears),0)
# FinC_CNAandOT
# 
# FinC_CNAandOTx<-round(cbind(years,FinC1_CNAandOT,FinC2_CNAandOT),0)


################################################################################
# CPUE's
################################################################################
# Calculate Finnish gillnet and trapnet efforts for area 30 and area 31.
# Note that in this case area 30 needs to contain all the other subdivisions than 31,
# so that all catches and effort will be dealt (those are minor outside 30/31, but
# yet need to be included).

# include only those lines of data where both catch and effort data are known (Tapani)


finland%>%
  filter(FISHERY=="C")%>%
  group_by(GEAR)%>%
  count(SUB_DIV)

Fin_coast_cpue<-finland%>%
  filter(FISHERY=="C", is.na(NUMB)==F, is.na(EFFORT)==F)
  
Fin_coast_cpue%>%
  group_by(GEAR)%>%
  count(SUB_DIV)


#TN
########################
# SD 30 & 31

Fin_CTN<-Fin_coast_cpue%>%
  filter(GEAR=="FYK")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)

Fin_CTN30<-Fin_coast_cpue%>%
  filter(GEAR=="FYK", SUB_DIV!=31)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)

Fin_CTN31<-Fin_coast_cpue%>%
  filter(GEAR=="FYK", SUB_DIV==31)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)



# GN
########################
# SD 30 & 31

Fin_COT<-Fin_coast_cpue%>%
  filter(GEAR=="MIS")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)

Fin_COT30<-Fin_coast_cpue%>%
  filter(GEAR=="MIS", SUB_DIV!=31)%>%
  filter(is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)

Fin_COT31<-Fin_coast_cpue%>%
  filter(GEAR=="MIS", SUB_DIV==31)%>%
  filter(is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)

# # ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
################################################################################
# River fishery                                                                  
################################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~


Fin_R<-finland%>%
  filter(FISHERY=="R")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)))%>%
  select(-HYR)
Fin_R        

# fin_river<-subset(finland, FISHERY=="R")
# summary(fin_river)
# #attach(fin_river)
# 
# summary(fin_river$GEAR)
# # AN  GND  GNS  LLD  LLS   OT   TN NA's 
# #  19    0    0    0    0    0    0    9 
# # All catches to the same!
#  
# summary(fin_river$TP_TYPE)
# # Only YR data! This goes as it is, no need to divide half yearly.
# 
# ####################
# # Catch
# ####################
# Catch<-vector()
# 
# for(y in min_year:max_year){
# 	temp<-0
# 
# 	for(i in 1:dim(fin_river)[1]){
#     if (fin_river$YEAR[i]==y){
# 		  if(is.na(fin_river$NUMB[i])==F){temp<-fin_river$NUMB[i]+temp}
#     }
# 	}
# 	Catch[(y-min_year+1)]<-temp
# }
# FinC_river<-Catch
# FinC_riverx<-round(cbind(years,Catch),0)
# 
# FinC_riverx
# 
# 
