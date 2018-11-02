## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Sweden

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


sweden<-filter(salmon, COUNTRY=="SE")

sweden%>%
  group_by(FISHERY)%>%
  count(TP_TYPE)
# YR data in all fisheries

################################################################################
#  Offshore driftnetting:                                                                  
################################################################################

sweden%>%
  filter(FISHERY=="S")%>%
  group_by(GEAR)%>%
  count(TP_TYPE)
# YR MIS & AN is minor -> goes to OLL

Swe_ODN<-sweden%>%
  filter(FISHERY=="S", GEAR=="GND")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))




# 
# swe_offs<-filter(sweden, FISHERY=="S")
# 
# SweODN_catch<-filter(swe_offs, GEAR=="GND") # | GEAR=="OT")
# SweODN_eff<-filter(swe_offs, GEAR=="GND")
# 
# ##############
# # Effort
# ##############
# SweODN_eff%>%count(TP_TYPE)
# # only HYR
# 
# SweE_ODN<-Effort_HYR(SweODN_eff)
# SweE1_ODN<-SweE_ODN[,2]; SweE2_ODN<-SweE_ODN[,3]
# 
# SweE_ODN<-round(GatherHalfYears(SweE1_ODN,SweE2_ODN,NumYears),0)
# SweE_ODN
# 
# SweE_ODNx<-cbind(years,SweE1_ODN,SweE2_ODN) 
# 
# ##############
# # Catches
# ##############
# 
# SweC_ODN<-Catch_HYR(SweODN_catch)
# SweC1_ODN<-SweC_ODN[,2]; SweC2_ODN<-SweC_ODN[,3]
# 
# SweC_ODN<-round(GatherHalfYears(SweC1_ODN,SweC2_ODN,NumYears),0)
# SweC_ODN
# 
# SweC_ODNx<-cbind(years,SweC1_ODN,SweC2_ODN) 

################################################################################
#  Offshore longlining:
################################################################################
# 2008-> Pool all sea catch with longline, divide it for half years based on LL proportions

sweden%>%
  filter(FISHERY=="S" , (GEAR=="LLD"| GEAR=="MIS" | GEAR=="AN"), TP_TYPE=="YR", is.na(EFFORT)==F, EFFORT!=0)

sweden%>%
  filter(FISHERY=="C" , SUB_DIV<30,
         TP_TYPE=="YR", is.na(EFFORT)==F, EFFORT!=0)

#Effort
SweE_OLL<-sweden%>%
  filter(FISHERY=="S", GEAR=="LLD")%>%
  group_by(YEAR, HYR)%>%
  summarise(Effort=round(sum(EFFORT, na.rm=T)))


# Catch

Swe_sea<-sweden%>%
  filter((FISHERY=="S" & (GEAR=="LLD"| GEAR=="MIS" | GEAR=="AN")) | 
        (FISHERY=="C" & SUB_DIV<30))
Swe_sea%>%count(TP_TYPE)
filter(Swe_sea, YEAR>2008)%>%group_by(SUB_DIV)%>%count(GEAR)

tmp1<-Swe_sea%>%
  filter(TP_TYPE=="HYR")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_sea%>%
  filter(TP_TYPE=="YR")%>%
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))
  
SweC_OLL<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_YR2=Catch_YR*p)%>%
  mutate(Catch=round(sum(Catch_HYR,Catch_YR2, na.rm=T)))%>%
  select(YEAR, HYR, Catch)

Swe_OLL<-full_join(SweC_OLL, SweE_OLL)
#View(Swe_OLL)


# 
# 
# SweOLL<-filter(swe_offs, GEAR=="LLD"| GEAR=="MIS" | GEAR=="AN")
# 
# SweOLL_Call<-rbind(
# subset(sweden, FISHERY=="S" & (GEAR=="LLD"| GEAR=="MIS" | GEAR=="AN")),
# subset(sweden, FISHERY=="C"& (SUB_DIV==29| SUB_DIV==28| SUB_DIV==27| SUB_DIV==26| SUB_DIV==25| SUB_DIV==24| SUB_DIV==23| SUB_DIV==22)))
# 
# 
# sweden%>%count(SUB_DIV)
# 
# SweOLL%>%count(TP_TYPE)
# #View(filter(SweOLL, TP_TYPE=="YR"))
# # YR: RECR AN & MIS
# 
# dim(SweOLL)[1]
# 
# ##############
# # Effort
# ##############
# filter(SweOLL, is.na(EFFORT)==F)%>%count(GEAR)
# # A tibble: 3 x 2
# #GEAR     n
# #<chr> <int>
# #1    AN     8
# #2   LLD   142
# #3   MIS    92
# 
# # addition 2018, surely only LLD effort belongs here?
# SweOLL_E<-filter(SweOLL, is.na(EFFORT)==F, GEAR=="LLD")
# 
# 
# SweE_OLL<-Effort_HYR(SweOLL_E)
# 
# SweE1_OLL<-SweE_OLL[,2]; SweE2_OLL<-SweE_OLL[,3]
# 
# SweE_OLL<-round(GatherHalfYears(SweE1_OLL,SweE2_OLL,NumYears),0)
# SweE_OLL
# 
# SweE1_OLL+SweE2_OLL 
# 
# SweE_OLLx<-cbind(years,SweE1_OLL,SweE2_OLL) 
# 
# ####################
# # Catches
# ####################
# SweC_OLL<-Catch_HYRandYR(SweOLL_Call)
# SweC1_OLL<-SweC_OLL[,2]; SweC2_OLL<-SweC_OLL[,3]
# 
# SweC_OLL<-round(GatherHalfYears(SweC1_OLL,SweC2_OLL,NumYears),0)
# SweC_OLL
# # Take these only for years 2008-> since before that OT is put to ODN!!
# 
# SweC_OLLx<-cbind(years,SweC1_OLL,SweC2_OLL) 
# 

################################################################################
################################################################################
################################################################################
# Coastal fisheries


sweden%>%
  filter(FISHERY=="C")%>%
  group_by(GEAR)%>%
  group_by(SUB_DIV)%>%
  count(TP_TYPE)

sweden%>%
  filter(FISHERY=="C" , SUB_DIV>29,
         TP_TYPE=="YR", is.na(EFFORT)==F, EFFORT!=0)
# 4 rows of RECR FYK with effort
# But coastal swe effort is largely missing anyway

#View(sweden%>%
#       filter(FISHERY=="C" , SUB_DIV>29))


################################################################################
#  Coastal TN & GN catches
################################################################################

Swe_coast<-sweden%>%filter(FISHERY=="C", SUB_DIV>29)

Swe_coast%>%
  group_by(F_TYPE)%>%
  count(GEAR)

Swe_coast%>%
  group_by(F_TYPE, GEAR)%>%
  count(TP_TYPE)
# All COMM is HYR, RECR is all YR


#  TN
tmp1<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="HYR")%>% # All COMM!
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="YR")%>% # All RECR!
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

SweC_CTN<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_RECR=round(Catch_YR*p))%>%
  mutate(Catch_COMM=Catch_HYR)%>%
  mutate(Catch_TOT=round(sum(Catch_COMM,Catch_RECR, na.rm=T)))%>%
  select(YEAR, HYR, Catch_TOT, Catch_RECR, Catch_COMM)

# OT
tmp1<-Swe_coast%>%
  filter(GEAR!="FYK",TP_TYPE=="HYR")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="YR")%>%
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

SweC_COT<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_YR2=Catch_YR*p)%>%
  mutate(Catch=round(sum(Catch_HYR,Catch_YR2, na.rm=T)))%>%
  select(YEAR, HYR, Catch)



####################
# Effort
####################

# 2013-> account for real swedish data
SweE_CTN_rep<-filter(Swe_coast, YEAR>2012)

filter(SweE_CTN_rep, is.na(EFFORT)==T)%>%
  group_by(YEAR)%>%
  count(GEAR)

filter(SweE_CTN_rep, is.na(EFFORT)==F)%>%
  group_by(YEAR)%>%
  count(GEAR)

# Small amount (=small catch) of RECR FYK EFFORT missing in 2015 -> ignore
filter(SweE_CTN_rep, YEAR==2015, GEAR=="FYK")

SweE_CTN_rep<-SweE_CTN_rep%>%
  filter(GEAR=="FYK", is.na(EFFORT)==F)%>%
    group_by(YEAR, SUB_DIV)%>%
    summarise(Effort_rep=round(sum(EFFORT)))

SweE_CTN_rep30<-filter(SweE_CTN_rep, SUB_DIV==30)%>%select(-SUB_DIV)
SweE_CTN_rep31<-filter(SweE_CTN_rep, SUB_DIV==31)%>%select(-SUB_DIV)

# 
# View(SweC_CGN)
# 
# Swe_coast<-subset(sweden, FISHERY=="C")
# Swe_coast%>%count(GEAR)
# # A tibble: 3 x 2
# #GEAR     n
# #<chr> <int>
# #1    AN     2
# #2   FYK   280
# #3   MIS   137
# 
# select(filter(Swe_coast, GEAR=="AN"), NUMB, EFFORT, everything())
# # Combine AN and offshore longline (where does this happen?)
# 
# Swe_coast%>%count(TP_TYPE)
# # A tibble: 2 x 2
# #TP_TYPE     n
# #<chr> <int>
# #1     HYR   373
# #2      YR    46
# 
# # meneeks nää jonnekkin?
# filter(Swe_coast, TP_TYPE=="YR")%>%count(GEAR)
# filter(Swe_coast, TP_TYPE=="YR")%>%count(YEAR)
# 
# ################################################################################
# #  Coastal trapnetting:
# ################################################################################
# SweCTN<-filter(Swe_coast, GEAR=="FYK")
# SweCTN_COMM<-filter(Swe_coast, GEAR=="FYK", F_TYPE=="COMM")
# SweCTN_RECR<-filter(Swe_coast, GEAR=="FYK", F_TYPE=="RECR")
# 
# SweCTN%>%count(TP_TYPE)
# # Both HYR and YR data!
# 
# dim(SweCTN)[1]
#               
# ####################
# # Catches
# ####################
# # COMM & RECR together
# SweC_CTN<-Catch_HYRandYR(SweCTN)
# SweC1_CTN<-SweC_CTN[,2]; SweC2_CTN<-SweC_CTN[,3]
# 
# SweC_CTN<-round(GatherHalfYears(SweC1_CTN,SweC2_CTN,NumYears),0)
# SweC_CTN
# 
# SweC_CTNx<-round(cbind(years,SweC1_CTN,SweC2_CTN),0)
#          
# # only COMM 
# SweC_CTN_COMM<-Catch_HYRandYR(SweCTN_COMM)
# SweC1_CTN_COMM<-SweC_CTN_COMM[,2]; SweC2_CTN_COMM<-SweC_CTN_COMM[,3]
# 
# SweC_CTN_COMM<-round(GatherHalfYears(SweC1_CTN_COMM,SweC2_CTN_COMM,NumYears),0)
# SweC_CTN_COMM
# 
# # only RECR
# 
# SweC_CTN_RECR<- SweC_CTN-SweC_CTN_COMM
# SweC_CTN_RECR
# 
# SweC1_CTN_RECR<- SweC1_CTN-SweC1_CTN_COMM
# SweC2_CTN_RECR<- SweC2_CTN-SweC2_CTN_COMM
# SweC1_CTN_RECR
# SweC2_CTN_RECR
# 
# 
# cbind(SweC_CTN_RECR,SweC_CTN,SweC_CTN_COMM,SweC_CTN_RECR+SweC_CTN_COMM)
# 
# ####################
# # Effort
# ####################
# # This would be used if REAL swedish data was available
# filter(SweCTN, is.na(EFFORT)==F & EFFORT>0)%>%count(TP_TYPE)
# 
# #View(filter(SweCTN, is.na(EFFORT==F), TP_TYPE=="YR"))
# # both HYR & YR, nevermind now if this is not used. Otherwise YR data should be accounted.
# 
# SweE_CTN<-Effort_HYR(SweCTN)
# SweE1_CTN<-SweE_CTN[,2]; SweE2_CTN<-SweE_CTN[,3]
# 
# SweE_CTN<-round(GatherHalfYears(SweE1_CTN,SweE2_CTN,NumYears),0)
# SweE_CTN
# 
# SweE_CTNx_real<-round(cbind(years,SweE1_CTN,SweE2_CTN),0)
# 
# ################################################################################
# #  Coastal other gear:
# ################################################################################
# SweCOT<-subset(Swe_coast, GEAR=="MIS")
# SweCOT%>%count(TP_TYPE)
# # Both HYR and YR data!
# 
# dim(SweCOT)[1]
#               
# ####################
# # Catches
# ####################
# SweC_COT<-Catch_HYRandYR(SweCOT)
# SweC1_COT<-SweC_COT[,2]; SweC2_COT<-SweC_COT[,3]
# 
# SweC_COT<-round(GatherHalfYears(SweC1_COT,SweC2_COT,NumYears),0)
# SweC_COT
# 
# SweC_COTx<-round(cbind(years,SweC1_COT,SweC2_COT),0)
# 
# ####################
# # Effort
# ####################
# subset(SweCOT, is.na(EFFORT)==F)
# # all existing effort data zeros -> Use Finnish CPUE to
# # calculate effort


################################################################################
# Effort for trapnet and other gear
################################################################################
# 3.3.2010: Lars: Commercial Swedish trapnet cpue is 20% smaller than
# the Finnish commercial trapnet cpue, and recreational swedish 
# trapnet cpue is 20% smaller than the swedish commercial trapnet cpue.

# TN
Fin_CTN_CPUE<-Fin_CTN%>%select(YEAR, HYR, CPUE)
SweC_CTN_COMM<-SweC_CTN%>%select(YEAR, HYR, Catch_COMM)
SweC_CTN_RECR<-SweC_CTN%>%select(YEAR, HYR, Catch_RECR)

Swe_CTN_COMM<-full_join(SweC_CTN_COMM, Fin_CTN_CPUE, by=c("YEAR", "HYR"))%>%
  mutate(Effort_COMM=round(Catch_COMM/(CPUE*0.8)))

Swe_CTN_RECR<-full_join(Swe_CTN_COMM, SweC_CTN_RECR)%>%
  mutate(Effort_RECR=round(Catch_RECR/(Catch_COMM/Effort_COMM*0.8)))

SweE_CTN<-full_join(select(Swe_CTN_COMM, YEAR, HYR, Effort_COMM),
                    select(Swe_CTN_RECR, YEAR, HYR, Effort_RECR))%>%
  mutate(Effort=Effort_COMM+Effort_RECR)%>%
  select(YEAR, HYR, Effort)

Swe_CTN<-full_join(SweC_CTN, SweE_CTN)%>%
  mutate(Catch=Catch_TOT)%>%
  select(YEAR, HYR, Catch, Effort)

# GN
Fin_COT_CPUE<-Fin_COT%>%select(YEAR, HYR, CPUE)

Swe_COT<-full_join(Fin_COT_CPUE,SweC_COT)%>%
  mutate(Effort=Catch/CPUE)


# 
# 
# SweE1_CTN_COMM<-SweC1_CTN_COMM/(FinCTN1_CPUE*0.8)
# SweE1_CTN_RECR<-SweC1_CTN_RECR/(SweC1_CTN_COMM/SweE1_CTN_COMM *0.8)
# 
# SweE2_CTN_COMM<-SweC2_CTN_COMM/(FinCTN2_CPUE*0.8)
# SweE2_CTN_RECR<-SweC2_CTN_RECR/(SweC2_CTN_COMM/SweE2_CTN_COMM *0.8)
# 
# SweE1_CTN<-SweE1_CTN_COMM+SweE1_CTN_RECR
# SweE2_CTN<-SweE2_CTN_COMM+SweE2_CTN_RECR
# 
# SweE1_COT<-SweC1_COT/FinCOT1_CPUE
# SweE2_COT<-SweC2_COT/FinCOT2_CPUE
# 
# #cbind(SweE1_COT,SweC1_COT,FinCOT1_CPUE)
# #cbind(SweE2_COT,SweC2_COT,FinCOT2_CPUE)
# 
# SweE_CTN<-round(GatherHalfYears(SweE1_CTN,SweE2_CTN,NumYears),0)
# SweE_CTN
# 
# SweE_CTNx<-round(cbind(years,SweE1_CTN,SweE2_CTN),0)
# 
# SweE_COT<-round(GatherHalfYears(SweE1_COT,SweE2_COT,NumYears),0)
# SweE_COT
# 
# SweE_COTx<-round(cbind(years,SweE1_COT,SweE2_COT),0)

################################################################################
# Calculate Swedish gillnet and trapnet efforts for area 30 and area 31.

Swe_coast%>%group_by(F_TYPE, TP_TYPE, GEAR)%>%
  count(SUB_DIV)


#  TN 30
tmp1<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="HYR", SUB_DIV==30)%>% # All COMM!
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="YR", SUB_DIV==30)%>% # All RECR!
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

SweC_CTN30<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_RECR=round(Catch_YR*p))%>%
  mutate(Catch_COMM=Catch_HYR)%>%
  mutate(Catch_TOT=round(sum(Catch_COMM,Catch_RECR, na.rm=T)))%>%
  select(YEAR, HYR, Catch_TOT, Catch_RECR, Catch_COMM)

# TN 31
tmp1<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="HYR", SUB_DIV==31)%>% # All COMM!
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="YR", SUB_DIV==31)%>% # All RECR!
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

SweC_CTN31<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_RECR=round(Catch_YR*p))%>%
  mutate(Catch_COMM=Catch_HYR)%>%
  mutate(Catch_TOT=round(sum(Catch_COMM,Catch_RECR, na.rm=T)))%>%
  select(YEAR, HYR, Catch_TOT, Catch_RECR, Catch_COMM)


# GN 30
tmp1<-Swe_coast%>%
  filter(GEAR!="FYK",TP_TYPE=="HYR", SUB_DIV==30)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="YR", SUB_DIV==30)%>%
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

SweC_COT30<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_YR2=Catch_YR*p)%>%
  mutate(Catch=round(sum(Catch_HYR,Catch_YR2, na.rm=T)))%>%
  select(YEAR, HYR, Catch)

#View(SweC_COT31)

# GN 31
tmp1<-Swe_coast%>%
  filter(GEAR!="FYK",TP_TYPE=="HYR", SUB_DIV==31)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="YR", SUB_DIV==31)%>%
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

SweC_COT31<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_YR2=Catch_YR*p)%>%
  mutate(Catch=round(sum(Catch_HYR,Catch_YR2, na.rm=T)))%>%
  select(YEAR, HYR, Catch)


# Calculate Swe effort with Swe catch & Fin CPUE

# TN 30
Fin_CTN30_CPUE<-Fin_CTN30%>%select(YEAR, HYR, CPUE)
SweC_CTN30_COMM<-SweC_CTN30%>%select(YEAR, HYR, Catch_COMM)
SweC_CTN30_RECR<-SweC_CTN30%>%select(YEAR, HYR, Catch_RECR)

Swe_CTN30_COMM<-full_join(SweC_CTN30_COMM, Fin_CTN30_CPUE, by=c("YEAR", "HYR"))%>%
  mutate(Effort_COMM=round(Catch_COMM/(CPUE*0.8)))

Swe_CTN30_RECR<-full_join(Swe_CTN30_COMM, SweC_CTN30_RECR)%>%
  mutate(Effort_RECR=round(Catch_RECR/(Catch_COMM/Effort_COMM*0.8)))

SweE_CTN30<-full_join(select(Swe_CTN30_COMM, YEAR, HYR, Effort_COMM),
                    select(Swe_CTN30_RECR, YEAR, HYR, Effort_RECR))%>%
  mutate(Effort=Effort_COMM+Effort_RECR)%>%select(YEAR, HYR, Effort)

Swe_CTN30<-full_join(SweC_CTN30, SweE_CTN30)%>%
  mutate(Catch=Catch_TOT)%>%select(YEAR, HYR, Catch, Effort)

# TN 31
Fin_CTN31_CPUE<-Fin_CTN31%>%select(YEAR, HYR, CPUE)
SweC_CTN31_COMM<-SweC_CTN31%>%select(YEAR, HYR, Catch_COMM)
SweC_CTN31_RECR<-SweC_CTN31%>%select(YEAR, HYR, Catch_RECR)

Swe_CTN31_COMM<-full_join(SweC_CTN31_COMM, Fin_CTN31_CPUE, by=c("YEAR", "HYR"))%>%
  mutate(Effort_COMM=round(Catch_COMM/(CPUE*0.8)))

Swe_CTN31_RECR<-full_join(Swe_CTN31_COMM, SweC_CTN31_RECR)%>%
  mutate(Effort_RECR=round(Catch_RECR/(Catch_COMM/Effort_COMM*0.8)))

SweE_CTN31<-full_join(select(Swe_CTN31_COMM, YEAR, HYR, Effort_COMM),
                      select(Swe_CTN31_RECR, YEAR, HYR, Effort_RECR))%>%
  mutate(Effort=Effort_COMM+Effort_RECR)%>%select(YEAR, HYR, Effort)

Swe_CTN31<-full_join(SweC_CTN31, SweE_CTN31)%>%
  mutate(Catch=Catch_TOT)%>%select(YEAR, HYR, Catch, Effort)


# GN 30
Fin_COT30_CPUE<-Fin_COT30%>%select(YEAR, HYR, CPUE)
Swe_COT30<-full_join(Fin_COT30_CPUE,SweC_COT30)%>%
  mutate(Effort=ifelse(is.na(Catch)==T, 0, round(Catch/CPUE)))
#View(Swe_COT30)

# GN 31
Fin_COT31_CPUE<-Fin_COT31%>%select(YEAR, HYR, CPUE)
Swe_COT31<-full_join(Fin_COT31_CPUE,SweC_COT31)%>%
  mutate(Effort=ifelse(is.na(Catch)==T, 0, round(Catch/CPUE)))




# SweCTN<-filter(Swe_coast, GEAR=="FYK")
# SweC_CTN_COMM%>%count(SUB_DIV)
# 
# 
# SweCTN%>%count(TP_TYPE)
# # Both HYR and YR data!
# 
# ########################
# # Coastal trapnet
# ########################
# # Area 30
# SweCTN30<-subset(Swe_coast, GEAR=="FYK" & SUB_DIV==30 )
# SweCTN30_COMM<-subset(Swe_coast, GEAR=="FYK" & SUB_DIV==30 & F_TYPE=="COMM" )
# SweCTN30_RECR<-subset(Swe_coast, GEAR=="FYK" & SUB_DIV==30 & F_TYPE=="RECR" )
# dim(SweCTN30)[1]
# 
# ##############
# # Catch
# ##############
# SweC_CTN30_<-Catch_HYRandYR(SweCTN30)
# SweC1_CTN30<-SweC_CTN30_[,2]; SweC2_CTN30<-SweC_CTN30_[,3]
# 
# SweC_CTN30<-round(GatherHalfYears(SweC1_CTN30,SweC2_CTN30,NumYears),0)
# SweC_CTN30
# 
# SweC_CTN30x<-round(cbind(years,SweC1_CTN30,SweC2_CTN30),0)
# 
# 
# # COMM
# SweC_CTN30_COMM<-Catch_HYRandYR(SweCTN30_COMM)
# SweC1_CTN30_COMM<-SweC_CTN30_COMM[,2]; SweC2_CTN30_COMM<-SweC_CTN30_COMM[,3]
# 
# SweC_CTN30_COMM<-round(GatherHalfYears(SweC1_CTN30_COMM,SweC2_CTN30_COMM,NumYears),0)
# SweC_CTN30_COMM
# 
# # RECR
# SweC_CTN30_RECR<-SweC_CTN30-SweC_CTN30_COMM
# SweC1_CTN30_RECR<-SweC1_CTN30-SweC1_CTN30_COMM
# SweC2_CTN30_RECR<-SweC2_CTN30-SweC2_CTN30_COMM
# 
# ##############
# # Effort
# ##############
# SweE_CTN30_<-Effort_HYR(SweCTN30)
# SweE1_CTN30<-SweE_CTN30_[,2]; SweE2_CTN30<-SweE_CTN30_[,3]
# 
# SweE_CTN30<-round(GatherHalfYears(SweE1_CTN30,SweE2_CTN30,NumYears),0)
# SweE_CTN30
# 
# SweE_CTN30x_real<-round(cbind(years,SweE1_CTN30,SweE2_CTN30),0)
#                         
# ########################
# # Area 31
# SweCTN31<-subset(Swe_coast, GEAR=="FYK" & SUB_DIV==31 )
# SweCTN31_COMM<-subset(Swe_coast, GEAR=="FYK" & SUB_DIV==31 & F_TYPE=="COMM")
# SweCTN31_RECR<-subset(Swe_coast, GEAR=="FYK" & SUB_DIV==31 & F_TYPE=="RECR")
# dim(SweCTN31)[1]
# 
# ##############
# # Catch
# ##############
# SweC_CTN31_<-Catch_HYRandYR(SweCTN31)
# SweC1_CTN31<-SweC_CTN31_[,2]; SweC2_CTN31<-SweC_CTN31_[,3]
# 
# SweC_CTN31<-round(GatherHalfYears(SweC1_CTN31,SweC2_CTN31,NumYears),0)
# SweC_CTN31
# 
# SweC_CTN31x<-round(cbind(years,SweC1_CTN31,SweC2_CTN31),0)
# 
# # COMM
# SweC_CTN31_COMM<-Catch_HYRandYR(SweCTN31_COMM)
# SweC1_CTN31_COMM<-SweC_CTN31_COMM[,2]; SweC2_CTN31_COMM<-SweC_CTN31_COMM[,3]
# 
# SweC_CTN31_COMM<-round(GatherHalfYears(SweC1_CTN31_COMM,SweC2_CTN31_COMM,NumYears),0)
# SweC_CTN31_COMM
# 
# # RECR
# 
# SweC_CTN31_RECR<-SweC_CTN31-SweC_CTN31_COMM
# SweC1_CTN31_RECR<-SweC1_CTN31-SweC1_CTN31_COMM
# SweC2_CTN31_RECR<-SweC2_CTN31-SweC2_CTN31_COMM
# 
# 
# ##############
# # Effort
# ##############
# SweE_CTN31_<-Effort_HYR(SweCTN31)
# SweE1_CTN31<-SweE_CTN31_[,2]; SweE2_CTN31<-SweE_CTN31_[,3]
# 
# SweE_CTN31<-round(GatherHalfYears(SweE1_CTN31,SweE2_CTN31,NumYears),0)
# SweE_CTN31
# 
# SweE_CTN31x_real<-round(cbind(years,SweE1_CTN31,SweE2_CTN31),0)
# 
#                                                                            
# ########################
# # Coastal other gear
# ########################
# # Area 30
# 
# SweCOT30<-subset(Swe_coast, GEAR=="MIS" & SUB_DIV!=31 )
# dim(SweCOT30)[1]
# 
# ##############
# # Catch
# ##############
# SweC_COT30_<-Catch_HYRandYR(SweCOT30)
# SweC1_COT30<-SweC_COT30_[,2]; SweC2_COT30<-SweC_COT30_[,3]
# 
# SweC_COT30<-round(GatherHalfYears(SweC1_COT30,SweC2_COT30,NumYears),0)
# SweC_COT30
# 
# SweC_COT30x<-round(cbind(years,SweC1_COT30,SweC2_COT30),0)
#                       
# ########################
# # Area 31
# SweCOT31<-subset(Swe_coast, GEAR=="MIS" & SUB_DIV==31 )
# dim(SweCOT31)[1]
# 
# ##############
# # Catch
# ##############
# SweC_COT31_<-Catch_HYRandYR(SweCOT31)
# SweC1_COT31<-SweC_COT31_[,2]; SweC2_COT31<-SweC_COT31_[,3]
# 
# SweC_COT31<-round(GatherHalfYears(SweC1_COT31,SweC2_COT31,NumYears),0)
# SweC_COT31
# 
# SweC_COT31x<-round(cbind(years,SweC1_COT31,SweC2_COT31),0)
# 
# #cbind(FinCTN30_CPUE1,FinCTN30_CPUE2)                               
# 
# # Finally, calculate corresponding efforts based on Finnish CPUE's
# SweE1_CTN30_COMM<-SweC1_CTN30_COMM/(FinCTN30_CPUE1*0.8)
# SweE2_CTN30_COMM<-SweC2_CTN30_COMM/(FinCTN30_CPUE2*0.8)
# SweE1_CTN31_COMM<-SweC1_CTN31_COMM/(FinCTN31_CPUE1*0.8)
# SweE2_CTN31_COMM<-SweC2_CTN31_COMM/(FinCTN31_CPUE2*0.8)
# 
# SweE1_CTN30_RECR<-SweC1_CTN30_RECR/(SweC1_CTN30_COMM/SweE1_CTN30_COMM*0.8)
# SweE2_CTN30_RECR<-SweC2_CTN30_RECR/(SweC2_CTN30_COMM/SweE2_CTN30_COMM*0.8)
# SweE1_CTN31_RECR<-SweC1_CTN31_RECR/(SweC1_CTN31_COMM/SweE1_CTN31_COMM*0.8)
# SweE2_CTN31_RECR<-SweC2_CTN31_RECR/(SweC2_CTN31_COMM/SweE2_CTN31_COMM*0.8)
# 
# #cbind(SweE1_CTN30,SweC1_CTN30_COMM,FinCTN30_CPUE1)
# #cbind(SweE2_CTN30,SweC2_CTN30_COMM,FinCTN30_CPUE2)
# 
# 
# SweE1_CTN30<-SweE1_CTN30_COMM+SweE1_CTN30_RECR
# SweE2_CTN30<-SweE2_CTN30_COMM+SweE2_CTN30_RECR
# 
# SweE1_CTN31<-SweE1_CTN31_COMM+SweE1_CTN31_RECR
# SweE2_CTN31<-SweE2_CTN31_COMM+SweE2_CTN31_RECR
# 
# cbind(SweE1_CTN31,SweE1_CTN31_COMM,SweE1_CTN31_RECR)
# 
# 
# SweE_CTN30<-round(GatherHalfYears(SweE1_CTN30,SweE2_CTN30,NumYears),0)
# SweE_CTN30
# SweE_CTN30x<-round(cbind(years,SweE1_CTN30,SweE2_CTN30),0)
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 
# SweE_CTN31<-round(GatherHalfYears(SweE1_CTN31,SweE2_CTN31,NumYears),0)
# SweE_CTN31
# SweE_CTN31x<-round(cbind(years,SweE1_CTN31,SweE2_CTN31),0)
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 
# 
# #SweE1_COT30<-SweC1_COT30/FinCOT30_CPUE1
# #SweE2_COT30<-SweC2_COT30/FinCOT30_CPUE2
# SweE1_COT30<-SweC1_COT30/FinCOT1_CPUE
# SweE2_COT30<-SweC2_COT30/FinCOT2_CPUE
# 
# #SweE1_COT31<-SweC1_COT31/FinCOT31_CPUE1
# #SweE2_COT31<-SweC2_COT31/FinCOT31_CPUE2
# SweE1_COT31<-SweC1_COT31/FinCOT1_CPUE
# SweE2_COT31<-SweC2_COT31/FinCOT2_CPUE
# 
# SweE_COT30<-round(GatherHalfYears(SweE1_COT30,SweE2_COT30,NumYears),0)
# SweE_COT30
# SweE_COT30x<-round(cbind(years,SweE1_COT30,SweE2_COT30),0)
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 
# SweE_COT31<-round(GatherHalfYears(SweE1_COT31,SweE2_COT31,NumYears),0)
# SweE_COT31
# SweE_COT31x<-round(cbind(years,SweE1_COT31,SweE2_COT31),0)
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

################################################################################
# River


Swe_R<-sweden%>%
  filter(FISHERY=="R")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)))%>%
  select(-HYR)
Swe_R      

# 
# 
# summary(sweden)
# Swe_river<-subset(sweden, FISHERY=="R")
# summary(Swe_river)
# #attach(Swe_river)
# # Catch
# Catch<-vector()
# for(y in min_year:max_year){
# 	temp<-0
# 	for(i in 1:dim(Swe_river)[1]){
#     if (Swe_river$YEAR[i]==y){
# 		  if(is.na(Swe_river$NUMB[i])==F){temp<-Swe_river$NUMB[i]+temp}
#     }
# 	}
# 	Catch[(y-min_year+1)]<-temp
# }
# SweC_river<-Catch
# SweC_riverx<-cbind(years,Catch)
# SweC_riverx
# 
