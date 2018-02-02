## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Sweden

# R-file:		   WGBAST_DB_Sweden.r

# input: 		   WGBAST_DB09.txt
# output:  	

# R ver:	  	  2.8.0

# programmed:		2009 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


salmon<-subset(dat_all, SPECIES=="SAL" & SUB_DIV!=32 & F_TYPE!="DISC" & F_TYPE!="SEAL")
summary(salmon)

sweden<-subset(salmon, COUNTRY=="SE")
summary(sweden)

summary(sweden$GEAR)
# No NA's!!!!
summary(sweden$FISHERY)
summary(sweden$TIME_PERIOD)
summary(sweden$TP_TYPE)
# only HYR and YR data

swe_offs<-subset(sweden, FISHERY=="S")
summary(swe_offs)
summary(swe_offs$GEAR)
# AN GND GNS LLD LLS  OT  TN 
#  7 117   0 100   0 158   0 

summary(sweden$SUB_DIV)

################################################################################
#  Offshore driftnetting:                                                                  
################################################################################
# Put OT to driftnet fishery 
# V. 2008 -> put OT to LL when DN is gone.
SweODN_catch<-subset(swe_offs, GEAR=="GND") # | GEAR=="OT")
SweODN_eff<-subset(swe_offs, GEAR=="GND")

##############
# Effort
##############
summary(SweODN_eff)
summary(SweODN_eff$TP_TYPE)
# only HYR

dim(summary(SweODN_eff))[1]
SweE_ODN<-Effort_HYR(SweODN_eff)
SweE1_ODN<-SweE_ODN[,2]; SweE2_ODN<-SweE_ODN[,3]

SweE_ODN<-round(GatherHalfYears(SweE1_ODN,SweE2_ODN,NumYears),0)
SweE_ODN

SweE_ODNx<-cbind(years,SweE1_ODN,SweE2_ODN) 

##############
# Catches
##############
summary(SweODN_catch)

dim(summary(SweODN_catch))[1]
SweC_ODN<-Catch_HYR(SweODN_catch)
SweC1_ODN<-SweC_ODN[,2]; SweC2_ODN<-SweC_ODN[,3]

SweC_ODN<-round(GatherHalfYears(SweC1_ODN,SweC2_ODN,NumYears),0)
SweC_ODN

SweC_ODNx<-cbind(years,SweC1_ODN,SweC2_ODN) 

################################################################################
#  Offshore longlining:
################################################################################
# Put angling with longline, divide it for half years based on longlining proportions

SweOLL<-subset(swe_offs, GEAR=="LLD"| GEAR=="OT" | GEAR=="AN")

SweOLL_Call<-rbind(
subset(sweden, FISHERY=="S" & (GEAR=="LLD"| GEAR=="OT" | GEAR=="AN")),
subset(sweden, FISHERY=="C"& (SUB_DIV==29| SUB_DIV==28| SUB_DIV==27| SUB_DIV==26| SUB_DIV==25| SUB_DIV==24| SUB_DIV==23| SUB_DIV==22)))

summary(SweOLL)
summary(SweOLL$TP_TYPE)
test<-subset(SweOLL, TP_TYPE=="YR")
test

dim(SweOLL)[1]

##############
# Effort
##############
SweE_OLL<-Effort_HYR(SweOLL)

SweE1_OLL<-SweE_OLL[,2]; SweE2_OLL<-SweE_OLL[,3]

SweE_OLL<-round(GatherHalfYears(SweE1_OLL,SweE2_OLL,NumYears),0)
SweE_OLL

SweE1_OLL+SweE2_OLL 

SweE_OLLx<-cbind(years,SweE1_OLL,SweE2_OLL) 

####################
# Catches
####################
SweC_OLL<-Catch_HYRandYR(SweOLL_Call)
SweC1_OLL<-SweC_OLL[,2]; SweC2_OLL<-SweC_OLL[,3]

SweC_OLL<-round(GatherHalfYears(SweC1_OLL,SweC2_OLL,NumYears),0)
SweC_OLL
# Take these only for years 2008-> since before that OT is put to ODN!!

SweC_OLLx<-cbind(years,SweC1_OLL,SweC2_OLL) 


################################################################################
################################################################################
################################################################################
# Coastal fisheries

Swe_coast<-subset(sweden, FISHERY=="C")
summary(Swe_coast)
summary(Swe_coast$GEAR)
# AN FPO GND GNS GTR LLD LLS MIS  OT OTB SDN  TN 
#  2   0   0   0   0   0   0   0  32   0   0  83 
# Combine AN and offshore longline 
summary(Swe_coast$TP_TYPE)
# HYR MON QTR  YR 
#  94   0   0  23 

################################################################################
#  Coastal trapnetting:
################################################################################
SweCTN<-subset(Swe_coast, GEAR=="TN")
SweCTN_COMM<-subset(Swe_coast, GEAR=="TN" & F_TYPE=="COMM")
SweCTN_RECR<-subset(Swe_coast, GEAR=="TN" & F_TYPE=="RECR")
summary(SweCTN)
summary(SweCTN_COMM)
summary(SweCTN_RECR)
#attach(SweCTN)

summary(SweCTN$TP_TYPE)
# Both HYR and YR data!

dim(SweCTN)[1]
              
####################
# Catches
####################
# COMM & RECR together
SweC_CTN<-Catch_HYRandYR(SweCTN)
SweC1_CTN<-SweC_CTN[,2]; SweC2_CTN<-SweC_CTN[,3]

SweC_CTN<-round(GatherHalfYears(SweC1_CTN,SweC2_CTN,NumYears),0)
SweC_CTN

SweC_CTNx<-round(cbind(years,SweC1_CTN,SweC2_CTN),0)
         
# only COMM 
SweC_CTN_COMM<-Catch_HYRandYR(SweCTN_COMM)
SweC1_CTN_COMM<-SweC_CTN_COMM[,2]; SweC2_CTN_COMM<-SweC_CTN_COMM[,3]

SweC_CTN_COMM<-round(GatherHalfYears(SweC1_CTN_COMM,SweC2_CTN_COMM,NumYears),0)
SweC_CTN_COMM

# only RECR

SweC_CTN_RECR<- SweC_CTN-SweC_CTN_COMM
SweC_CTN_RECR

SweC1_CTN_RECR<- SweC1_CTN-SweC1_CTN_COMM
SweC2_CTN_RECR<- SweC2_CTN-SweC2_CTN_COMM
SweC1_CTN_RECR
SweC2_CTN_RECR


cbind(SweC_CTN_RECR,SweC_CTN,SweC_CTN_COMM,SweC_CTN_RECR+SweC_CTN_COMM)

####################
# Effort
####################
# This would be used if REAL swedish data was available
subset(SweCTN, is.na(EFFORT)==F & EFFORT>0)
# all existing effort data HYR

SweE_CTN<-Effort_HYR(SweCTN)
SweE1_CTN<-SweE_CTN[,2]; SweE2_CTN<-SweE_CTN[,3]

SweE_CTN<-round(GatherHalfYears(SweE1_CTN,SweE2_CTN,NumYears),0)
SweE_CTN

SweE_CTNx_real<-round(cbind(years,SweE1_CTN,SweE2_CTN),0)

################################################################################
#  Coastal other gear:
################################################################################
SweCOT<-subset(Swe_coast, GEAR=="OT")
summary(SweCOT)
#attach(SweCOT)

summary(SweCOT$TP_TYPE)
# Both HYR and YR data!

dim(SweCOT)[1]
              
####################
# Catches
####################
SweC_COT<-Catch_HYRandYR(SweCOT)
SweC1_COT<-SweC_COT[,2]; SweC2_COT<-SweC_COT[,3]

SweC_COT<-round(GatherHalfYears(SweC1_COT,SweC2_COT,NumYears),0)
SweC_COT

SweC_COTx<-round(cbind(years,SweC1_COT,SweC2_COT),0)

####################
# Effort
####################
subset(SweCOT, is.na(EFFORT)==F)
# all existing effort data zeros -> Use Finnish CPUE to
# calculate effort


################################################################################
# Effort for trapnet and other gear. Run first WGBAST_DB_Finland_CoastalCPUE.r
################################################################################
# 3.3.2010: Lars: Commercial Swedish trapnet cpue is 20% smaller than
# the Finnish commercial trapnet cpue, and recreational swedish 
# trapnet cpue is 20% smaller than the swedish commercial trapnet cpue.

SweE1_CTN_COMM<-SweC1_CTN_COMM/(FinCTN1_CPUE*0.8)
SweE1_CTN_RECR<-SweC1_CTN_RECR/(SweC1_CTN_COMM/SweE1_CTN_COMM *0.8)

SweE2_CTN_COMM<-SweC2_CTN_COMM/(FinCTN2_CPUE*0.8)
SweE2_CTN_RECR<-SweC2_CTN_RECR/(SweC2_CTN_COMM/SweE2_CTN_COMM *0.8)

SweE1_CTN<-SweE1_CTN_COMM+SweE1_CTN_RECR
SweE2_CTN<-SweE2_CTN_COMM+SweE2_CTN_RECR

SweE1_COT<-SweC1_COT/FinCOT1_CPUE
SweE2_COT<-SweC2_COT/FinCOT2_CPUE

#cbind(SweE1_COT,SweC1_COT,FinCOT1_CPUE)
#cbind(SweE2_COT,SweC2_COT,FinCOT2_CPUE)

SweE_CTN<-round(GatherHalfYears(SweE1_CTN,SweE2_CTN,NumYears),0)
SweE_CTN

SweE_CTNx<-round(cbind(years,SweE1_CTN,SweE2_CTN),0)

SweE_COT<-round(GatherHalfYears(SweE1_COT,SweE2_COT,NumYears),0)
SweE_COT

SweE_COTx<-round(cbind(years,SweE1_COT,SweE2_COT),0)

################################################################################
# Calculate Swedish gillnet and trapnet efforts for area 30 and area 31.
# Note that in this case area 30 needs to contain all the other subdivisions than 31,
# so that all catches and effort will be dealt (those are minor outside 30/31, but
# need to be taken into account).

summary(sweden)
summary(SweCTN$TP_TYPE)
# Both HYR and YR data!

########################
# Coastal trapnet
########################
# Area 30
SweCTN30<-subset(Swe_coast, GEAR=="TN" & SUB_DIV==30 )
SweCTN30_COMM<-subset(Swe_coast, GEAR=="TN" & SUB_DIV==30 & F_TYPE=="COMM" )
SweCTN30_RECR<-subset(Swe_coast, GEAR=="TN" & SUB_DIV==30 & F_TYPE=="RECR" )
summary(SweCTN30)
#attach(SweCTN30)

dim(SweCTN30)[1]

##############
# Catch
##############
SweC_CTN30_<-Catch_HYRandYR(SweCTN30)
SweC1_CTN30<-SweC_CTN30_[,2]; SweC2_CTN30<-SweC_CTN30_[,3]

SweC_CTN30<-round(GatherHalfYears(SweC1_CTN30,SweC2_CTN30,NumYears),0)
SweC_CTN30

SweC_CTN30x<-round(cbind(years,SweC1_CTN30,SweC2_CTN30),0)


# COMM
SweC_CTN30_COMM<-Catch_HYRandYR(SweCTN30_COMM)
SweC1_CTN30_COMM<-SweC_CTN30_COMM[,2]; SweC2_CTN30_COMM<-SweC_CTN30_COMM[,3]

SweC_CTN30_COMM<-round(GatherHalfYears(SweC1_CTN30_COMM,SweC2_CTN30_COMM,NumYears),0)
SweC_CTN30_COMM

# RECR
SweC_CTN30_RECR<-SweC_CTN30-SweC_CTN30_COMM
SweC1_CTN30_RECR<-SweC1_CTN30-SweC1_CTN30_COMM
SweC2_CTN30_RECR<-SweC2_CTN30-SweC2_CTN30_COMM

##############
# Effort
##############
SweE_CTN30_<-Effort_HYR(SweCTN30)
SweE1_CTN30<-SweE_CTN30_[,2]; SweE2_CTN30<-SweE_CTN30_[,3]

SweE_CTN30<-round(GatherHalfYears(SweE1_CTN30,SweE2_CTN30,NumYears),0)
SweE_CTN30

SweE_CTN30x_real<-round(cbind(years,SweE1_CTN30,SweE2_CTN30),0)
                        
########################
# Area 31
SweCTN31<-subset(Swe_coast, GEAR=="TN" & SUB_DIV==31 )
SweCTN31_COMM<-subset(Swe_coast, GEAR=="TN" & SUB_DIV==31 & F_TYPE=="COMM")
SweCTN31_RECR<-subset(Swe_coast, GEAR=="TN" & SUB_DIV==31 & F_TYPE=="RECR")
summary(SweCTN31)
#attach(SweCTN31)

dim(SweCTN31)[1]

##############
# Catch
##############
SweC_CTN31_<-Catch_HYRandYR(SweCTN31)
SweC1_CTN31<-SweC_CTN31_[,2]; SweC2_CTN31<-SweC_CTN31_[,3]

SweC_CTN31<-round(GatherHalfYears(SweC1_CTN31,SweC2_CTN31,NumYears),0)
SweC_CTN31

SweC_CTN31x<-round(cbind(years,SweC1_CTN31,SweC2_CTN31),0)

# COMM
SweC_CTN31_COMM<-Catch_HYRandYR(SweCTN31_COMM)
SweC1_CTN31_COMM<-SweC_CTN31_COMM[,2]; SweC2_CTN31_COMM<-SweC_CTN31_COMM[,3]

SweC_CTN31_COMM<-round(GatherHalfYears(SweC1_CTN31_COMM,SweC2_CTN31_COMM,NumYears),0)
SweC_CTN31_COMM

# RECR

SweC_CTN31_RECR<-SweC_CTN31-SweC_CTN31_COMM
SweC1_CTN31_RECR<-SweC1_CTN31-SweC1_CTN31_COMM
SweC2_CTN31_RECR<-SweC2_CTN31-SweC2_CTN31_COMM


##############
# Effort
##############
SweE_CTN31_<-Effort_HYR(SweCTN31)
SweE1_CTN31<-SweE_CTN31_[,2]; SweE2_CTN31<-SweE_CTN31_[,3]

SweE_CTN31<-round(GatherHalfYears(SweE1_CTN31,SweE2_CTN31,NumYears),0)
SweE_CTN31

SweE_CTN31x_real<-round(cbind(years,SweE1_CTN31,SweE2_CTN31),0)

                                                                           
########################
# Coastal other gear
########################
# Area 30

SweCOT30<-subset(Swe_coast, GEAR=="OT" & SUB_DIV!=31 )
summary(SweCOT30)
#attach(SweCOT30)

dim(SweCOT30)[1]

##############
# Catch
##############
SweC_COT30_<-Catch_HYRandYR(SweCOT30)
SweC1_COT30<-SweC_COT30_[,2]; SweC2_COT30<-SweC_COT30_[,3]

SweC_COT30<-round(GatherHalfYears(SweC1_COT30,SweC2_COT30,NumYears),0)
SweC_COT30

SweC_COT30x<-round(cbind(years,SweC1_COT30,SweC2_COT30),0)
                      
########################
# Area 31
SweCOT31<-subset(Swe_coast, GEAR=="OT" & SUB_DIV==31 )
summary(SweCOT31)
#attach(SweCOT31)

dim(SweCOT31)[1]

##############
# Catch
##############
SweC_COT31_<-Catch_HYRandYR(SweCOT31)
SweC1_COT31<-SweC_COT31_[,2]; SweC2_COT31<-SweC_COT31_[,3]

SweC_COT31<-round(GatherHalfYears(SweC1_COT31,SweC2_COT31,NumYears),0)
SweC_COT31

SweC_COT31x<-round(cbind(years,SweC1_COT31,SweC2_COT31),0)

#cbind(FinCTN30_CPUE1,FinCTN30_CPUE2)                               

# Finally, calculate corresponding efforts based on Finnish CPUE's
SweE1_CTN30_COMM<-SweC1_CTN30_COMM/(FinCTN30_CPUE1*0.8)
SweE2_CTN30_COMM<-SweC2_CTN30_COMM/(FinCTN30_CPUE2*0.8)
SweE1_CTN31_COMM<-SweC1_CTN31_COMM/(FinCTN31_CPUE1*0.8)
SweE2_CTN31_COMM<-SweC2_CTN31_COMM/(FinCTN31_CPUE2*0.8)

SweE1_CTN30_RECR<-SweC1_CTN30_RECR/(SweC1_CTN30_COMM/SweE1_CTN30_COMM*0.8)
SweE2_CTN30_RECR<-SweC2_CTN30_RECR/(SweC2_CTN30_COMM/SweE2_CTN30_COMM*0.8)
SweE1_CTN31_RECR<-SweC1_CTN31_RECR/(SweC1_CTN31_COMM/SweE1_CTN31_COMM*0.8)
SweE2_CTN31_RECR<-SweC2_CTN31_RECR/(SweC2_CTN31_COMM/SweE2_CTN31_COMM*0.8)

#cbind(SweE1_CTN30,SweC1_CTN30_COMM,FinCTN30_CPUE1)
#cbind(SweE2_CTN30,SweC2_CTN30_COMM,FinCTN30_CPUE2)


SweE1_CTN30<-SweE1_CTN30_COMM+SweE1_CTN30_RECR
SweE2_CTN30<-SweE2_CTN30_COMM+SweE2_CTN30_RECR

SweE1_CTN31<-SweE1_CTN31_COMM+SweE1_CTN31_RECR
SweE2_CTN31<-SweE2_CTN31_COMM+SweE2_CTN31_RECR

cbind(SweE1_CTN31,SweE1_CTN31_COMM,SweE1_CTN31_RECR)


SweE_CTN30<-round(GatherHalfYears(SweE1_CTN30,SweE2_CTN30,NumYears),0)
SweE_CTN30
SweE_CTN30x<-round(cbind(years,SweE1_CTN30,SweE2_CTN30),0)

SweE_CTN31<-round(GatherHalfYears(SweE1_CTN31,SweE2_CTN31,NumYears),0)
SweE_CTN31
SweE_CTN31x<-round(cbind(years,SweE1_CTN31,SweE2_CTN31),0)


#SweE1_COT30<-SweC1_COT30/FinCOT30_CPUE1
#SweE2_COT30<-SweC2_COT30/FinCOT30_CPUE2
SweE1_COT30<-SweC1_COT30/FinCOT1_CPUE
SweE2_COT30<-SweC2_COT30/FinCOT2_CPUE

#SweE1_COT31<-SweC1_COT31/FinCOT31_CPUE1
#SweE2_COT31<-SweC2_COT31/FinCOT31_CPUE2
SweE1_COT31<-SweC1_COT31/FinCOT1_CPUE
SweE2_COT31<-SweC2_COT31/FinCOT2_CPUE

SweE_COT30<-round(GatherHalfYears(SweE1_COT30,SweE2_COT30,NumYears),0)
SweE_COT30
SweE_COT30x<-round(cbind(years,SweE1_COT30,SweE2_COT30),0)

SweE_COT31<-round(GatherHalfYears(SweE1_COT31,SweE2_COT31,NumYears),0)
SweE_COT31
SweE_COT31x<-round(cbind(years,SweE1_COT31,SweE2_COT31),0)

################################################################################
# River
summary(sweden)
Swe_river<-subset(sweden, FISHERY=="R")
summary(Swe_river)
#attach(Swe_river)
# Catch
Catch<-vector()
for(y in min_year:max_year){
	temp<-0
	for(i in 1:dim(Swe_river)[1]){
    if (Swe_river$YEAR[i]==y){
		  if(is.na(Swe_river$NUMB[i])==F){temp<-Swe_river$NUMB[i]+temp}
    }
	}
	Catch[(y-min_year+1)]<-temp
}
SweC_river<-Catch
SweC_riverx<-cbind(years,Catch)
SweC_riverx

