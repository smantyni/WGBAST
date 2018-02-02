## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Finland

# R-file:		   WGBAST_DB_Finland.r

# input: 		   WGBAST_DB09.txt
# output:  	

# R ver:	  	  2.8.0

# programmed:		2009 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

#dat_all <- read.table(
#paste("data/der/catch&effort/WGBAST_DB",NumYears,".txt", sep=""), header=T)

summary(dat_all)

salmon<-subset(dat_all, SPECIES=="SAL" & SUB_DIV!=32 & F_TYPE!="DISC" & F_TYPE!="SEAL")
summary(salmon)

finland<-subset(salmon, COUNTRY=="FI")
summary(finland)

################################################################################
# Run WGBAST_DB_functions.R

################################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
################################################################################
#  Offshore fishery                                                                  
################################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
fin_offs<-subset(finland, FISHERY=="S")
summary(fin_offs)

summary(fin_offs$GEAR)
# AN GND GNS LLD LLS  OT  TN 
#  0 170   0  97   0  16   0
  # Catherine: For GEAR=OT, add effort and catch to
# commercial costal other gear!!!! 

summary(fin_offs$TP_TYPE)
# Only HYR data!
                                   
################################################################################
#  Offshore driftnetting:                                                                  
################################################################################

FinODN<-subset(fin_offs, GEAR=="GND")
#attach(FinODN)
summary(FinODN)

dim(FinODN)[1]

##############
# Effort
##############
FinE_ODN<-Effort_HYR(FinODN)
FinE1_ODN<-FinE_ODN[,2]; FinE2_ODN<-FinE_ODN[,3]

FinE_ODN<-round(GatherHalfYears(FinE1_ODN,FinE2_ODN,NumYears),0)
FinE_ODN

FinE_ODNx<-cbind(years,FinE1_ODN,FinE2_ODN) 

##############
# Catches
##############
FinC_ODN<-Catch_HYR(FinODN)
FinC1_ODN<-FinC_ODN[,2]; FinC2_ODN<-FinC_ODN[,3]
FinC1_ODN
FinC2_ODN
FinC_ODN

FinC_ODN<-round(GatherHalfYears(FinC1_ODN,FinC2_ODN,NumYears),0)
FinC_ODN

FinC_ODNx<-cbind(years,FinC1_ODN,FinC2_ODN) 

################################################################################
#  Offshore longlining:
################################################################################
FinOLL<-subset(fin_offs, GEAR=="LLD")
summary(FinOLL)
summary(FinOLL$TP_TYPE)

dim(FinOLL)[1]

##############
# Effort
##############
FinE_OLL<-Effort_HYR(FinOLL)
FinE1_OLL<-FinE_OLL[,2]; FinE2_OLL<-FinE_OLL[,3]

FinE_OLL<-round(GatherHalfYears(FinE1_OLL,FinE2_OLL,NumYears),0)
FinE_OLL

FinE_OLLx<-cbind(years,FinE1_OLL,FinE2_OLL) 

##############
# Catches
##############
FinC_OLL<-Catch_HYR(FinOLL)
FinC1_OLL<-FinC_OLL[,2]; FinC2_OLL<-FinC_OLL[,3]

FinC_OLL<-round(GatherHalfYears(FinC1_OLL,FinC2_OLL,NumYears),0)
FinC_OLL

FinC_OLLx<-cbind(years,FinC1_OLL,FinC2_OLL) 

################################################################################
#  Other offshore gear (OT)
################################################################################
# These will be added to commercial coastal other gear
FinOffsOT<-subset(fin_offs, GEAR=="OT")
#attach(FinOffsOT)
summary(FinOffsOT)
summary(FinOffsOT$TP_TYPE)

dim(FinOffsOT)[1]

##############
# Effort
##############
FinE_OffsOT<-Effort_HYR(FinOffsOT)
FinE1_OffsOT<-FinE_OffsOT[,2]; FinE2_OffsOT<-FinE_OffsOT[,3]
FinE1_OffsOT
FinE2_OffsOT
FinE_OffsOT

##############
# Catches
##############
FinC_OffsOT<-Catch_HYR(FinOffsOT)
FinC1_OffsOT<-FinC_OffsOT[,2]; FinC2_OffsOT<-FinC_OffsOT[,3]
FinC1_OffsOT
FinC2_OffsOT
FinC_OffsOT

################################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
################################################################################
#  Coastal fishery                                                                  
################################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~

fin_coast<-subset(finland, FISHERY=="C")
summary(fin_coast)
fin_coast$SUB_DIV

summary(fin_coast$GEAR)
# AN  GND  GNS  LLD  LLS   OT   TN NA's 
#   6   32    0    4    0   61   55   24 
 

# Groups in coastal catch and effort for Finland are
# CDN, CTN and Coastal OT. Corresponding groups here are
# GND, TN and OT
# what is left are: 
# AN=6, LLD=2, NA=18. What to do with those?
# Because NA's RECR => put those with GEAR=OT (Tapani)
test<-subset(fin_coast, GEAR=="AN")
summary(test)
test<-subset(fin_coast, is.na(GEAR)==T)
summary(test)
# Since AN only from 2000 and 2001, forget this!

summary(fin_coast$TP_TYPE)
summary(subset(fin_coast,TP_TYPE=="YR"))
# YR data is at gears AN and NA.
# If those will be taken along, then YR needs to be taken into account!

################################################################################
#  Coastal driftnetting:                                                                  
################################################################################
FinCDN<-subset(fin_coast, GEAR=="GND")
summary(FinCDN)
#attach(FinCDN)
summary(FinCDN$TP_TYPE)
# Only HYR data!

##############
# Effort
##############
FinE_CDN<-Effort_HYR(FinCDN)
FinE1_CDN<-FinE_CDN[,2]; FinE2_CDN<-FinE_CDN[,3]
FinE1_CDN
FinE2_CDN
FinE_CDN

FinE_CDN<-round(GatherHalfYears(FinE1_CDN,FinE2_CDN,NumYears),0)
FinE_CDN

##############
# Catches
##############
FinC_CDN<-Catch_HYR(FinCDN)
FinC1_CDN<-FinC_CDN[,2]; FinC2_CDN<-FinC_CDN[,3]
FinC1_CDN
FinC2_CDN
FinC_CDN

FinC_CDN<-round(GatherHalfYears(FinC1_CDN,FinC2_CDN,NumYears),0)
FinC_CDN

################################################################################
#  Coastal trapnetting:
################################################################################
FinCTN<-subset(fin_coast, GEAR=="TN")
summary(FinCTN)
#attach(FinCTN)

summary(FinCTN$TP_TYPE)
# Only HYR data!

dim(FinCTN)[1]

##############
# Effort
##############
FinE_CTN<-Effort_HYR(FinCTN)
FinE1_CTN<-FinE_CTN[,2]; FinE2_CTN<-FinE_CTN[,3]

FinE_CTN<-round(GatherHalfYears(FinE1_CTN,FinE2_CTN,NumYears),0)
FinE_CTN

FinE_CTNx<-cbind(years, FinE1_CTN,FinE2_CTN)

####################
# Catches
####################
FinC_CTN<-Catch_HYR(FinCTN)
FinC1_CTN<-FinC_CTN[,2]; FinC2_CTN<-FinC_CTN[,3]

FinC_CTN<-round(GatherHalfYears(FinC1_CTN,FinC2_CTN,NumYears),0)
FinC_CTN

FinC_CTNx<-cbind(years, FinC1_CTN,FinC2_CTN)

################################################################################
#  Other coastal gear (OT)
################################################################################
FinCoastOT<-subset(fin_coast, GEAR=="OT")
#attach(FinCoastOT)
summary(FinCoastOT)
summary(FinCoastOT$TP_TYPE)

dim(FinCoastOT)[1]  

##############
# Effort
##############
FinE_CoastOT<-Effort_HYR(FinCoastOT)
FinE1_CoastOT<-FinE_CoastOT[,2]; FinE2_CoastOT<-FinE_CoastOT[,3]

FinE_CoastOT<-round(GatherHalfYears(FinE1_CoastOT,FinE2_CoastOT,NumYears),0)
FinE_CoastOT

####################
# Catches
####################
FinC_CoastOT<-Catch_HYR(FinCoastOT)
FinC1_CoastOT<-FinC_CoastOT[,2]; FinC2_CoastOT<-FinC_CoastOT[,3]

FinC_CoastOT<-round(GatherHalfYears(FinC1_CoastOT,FinC2_CoastOT,NumYears),0)
FinC_CoastOT


###########################################
# Effort: Combine coastal and offshore other gear
###########################################
FinE1_OTtot<-FinE1_CoastOT+FinE1_OffsOT
FinE2_OTtot<-FinE2_CoastOT+FinE2_OffsOT

FinE_OTtot<-round(GatherHalfYears(FinE1_OTtot,FinE2_OTtot,NumYears),0)
FinE_OTtot

###########################################
# Catch: Combine coastal and offshore other gear
###########################################
FinC1_OTtot<-FinC1_CoastOT+FinC1_OffsOT
FinC2_OTtot<-FinC2_CoastOT+FinC2_OffsOT

FinC_OTtot<-round(GatherHalfYears(FinC1_OTtot,FinC2_OTtot,NumYears),0)
FinC_OTtot

################################################################################
#  Coastal gear = NA
################################################################################
# This is done only for the catch (effort isn't available) 
# Then divide by proportions to CDN, CTN and OT

# TAPANI: Because AN is RECR fishing, put that to OT 

FinCoastNA<-subset(fin_coast, is.na(GEAR)==T)
#attach(FinCoastNA)
summary(FinCoastNA)
summary(FinCoastNA$TP_TYPE)
# only YR data (18)

dim(FinCoastNA)[1]

####################
# Catches
####################
Catch<-vector()

for(y in min_year:max_year){
	temp<-0
	for(i in 1:dim(FinCoastNA)[1]){
    if (FinCoastNA$YEAR[i]==y){
		  if(is.na(FinCoastNA$NUMB[i])==F){temp<-FinCoastNA$NUMB[i]+temp}
    }
	}
	Catch[(y-min_year+1)]<-temp
}
FinC_CNA<-Catch
cbind(years,Catch)

################################################################################
# Combine OT catches and NA catches

# Gear OT (includes both coastal and offshore OT)
FinC1_OTtot
FinC2_OTtot

propC1_COT<-FinC1_OTtot/(FinC1_OTtot+FinC2_OTtot)
propC2_COT<-FinC2_OTtot/(FinC1_OTtot+FinC2_OTtot)

FinC1_CNA<-FinC_CNA*propC1_COT
FinC2_CNA<-FinC_CNA*propC2_COT

FinC1_CNAandOT<-FinC1_CNA+FinC1_OTtot
FinC2_CNAandOT<-FinC2_CNA+FinC2_OTtot

FinC_CNAandOT<-round(GatherHalfYears(FinC1_CNAandOT,FinC2_CNAandOT,NumYears),0)
FinC_CNAandOT

FinC_CNAandOTx<-round(cbind(years,FinC1_CNAandOT,FinC2_CNAandOT),0)


################################################################################
# Calculate Finnish gillnet and trapnet efforts for area 30 and area 31.
# Note that in this case area 30 needs to contain all the other subdivisions than 31,
# so that all catches and effort will be dealt (those are minor outside 30/31, but
# need to be taken into account).


summary(finland)
summary(FinCTN$TP_TYPE)
# Only HYR data!

########################
# Area 30
FinCTN30<-subset(fin_coast, GEAR=="TN" & SUB_DIV!=31 )
summary(FinCTN30)
#attach(FinCTN30)

dim(FinCTN30)[1]

##############
# Effort
##############
Effort1<-vector()
Effort2<-vector()

for(y in min_year:max_year){
	temp1<-0
	temp2<-0

	for(i in 1:dim(FinCTN30)[1]){
    if(FinCTN30$TP_TYPE[i]=="HYR"){
      if (FinCTN30$YEAR[i]==y && FinCTN30$TIME_PERIOD[i]==1){
			   if(is.na(FinCTN30$EFFORT[i])==F){temp1<-FinCTN30$EFFORT[i]+temp1}
		  }
		  if (FinCTN30$YEAR[i]==y && FinCTN30$TIME_PERIOD[i]==2){
			   if(is.na(FinCTN30$EFFORT[i])==F){temp2<-FinCTN30$EFFORT[i]+temp2}
		  }  
    }  
  }
  Effort1[(y-min_year+1)]<-temp1
	Effort2[(y-min_year+1)]<-temp2
}
FinE1_CTN30<-Effort1; FinE2_CTN30<-Effort2
Effort1<-round(Effort1,0); Effort2<-round(Effort2,0)
cbind(years,Effort1,Effort2)

FinE_CTN30<-round(GatherHalfYears(FinE1_CTN30,FinE2_CTN30,NumYears),0)
FinE_CTN30

FinE_CTN30x<-round(cbind(years,FinE1_CTN30,FinE2_CTN30),0)

                        
########################
# Area 31
FinCTN31<-subset(fin_coast, GEAR=="TN" & SUB_DIV==31 )
summary(FinCTN31)
#attach(FinCTN31)

dim(FinCTN31)[1]

##############
# Effort
##############
Effort1<-vector()
Effort2<-vector()

for(y in min_year:max_year){
	temp1<-0
	temp2<-0

	for(i in 1:dim(FinCTN31)[1]){
    if(FinCTN31$TP_TYPE[i]=="HYR"){
      if (FinCTN31$YEAR[i]==y && FinCTN31$TIME_PERIOD[i]==1){
			   if(is.na(FinCTN31$EFFORT[i])==F){temp1<-FinCTN31$EFFORT[i]+temp1}
		  }
		  if (FinCTN31$YEAR[i]==y && FinCTN31$TIME_PERIOD[i]==2){
			   if(is.na(FinCTN31$EFFORT[i])==F){temp2<-FinCTN31$EFFORT[i]+temp2}
		  }  
    }  
  }
  Effort1[(y-min_year+1)]<-temp1
	Effort2[(y-min_year+1)]<-temp2
}
FinE1_CTN31<-Effort1; FinE2_CTN31<-Effort2
Effort1<-round(Effort1,0); Effort2<-round(Effort2,0)
cbind(years,Effort1,Effort2)

FinE_CTN31<-round(GatherHalfYears(FinE1_CTN31,FinE2_CTN31,NumYears),0)
FinE_CTN31

FinE_CTN31x<-round(cbind(years,FinE1_CTN31,FinE2_CTN31),0)
                                                                            
# these are now the same!
cbind(FinE_CTN30+FinE_CTN31, FinE_CTN)
                                                                            
################################################################################
# Other coastal gear seems to be gillnet, right!!!???
                     
summary(subset(finland, GEAR=="OT"))
Fin_OT<-subset(finland, GEAR=="OT")

########################
# Area 30

FinOT30<-subset(Fin_OT, SUB_DIV!=31)
summary(FinOT30)

#attach(FinOT30)

dim(FinOT30)[1]

##############
# Effort
##############
Effort1<-vector()
Effort2<-vector()

for(y in min_year:max_year){
	temp1<-0
	temp2<-0

	for(i in 1:dim(FinOT30)[1]){
    if(FinOT30$TP_TYPE[i]=="HYR"){
      if (FinOT30$YEAR[i]==y && FinOT30$TIME_PERIOD[i]==1){
			   if(is.na(FinOT30$EFFORT[i])==F){temp1<-FinOT30$EFFORT[i]+temp1}
		  }
		  if (FinOT30$YEAR[i]==y && FinOT30$TIME_PERIOD[i]==2){
			   if(is.na(FinOT30$EFFORT[i])==F){temp2<-FinOT30$EFFORT[i]+temp2}
		  }  
    }  
  }
  Effort1[(y-min_year+1)]<-temp1
	Effort2[(y-min_year+1)]<-temp2
}
FinE1_OT30<-Effort1; FinE2_OT30<-Effort2
Effort1<-round(Effort1,0); Effort2<-round(Effort2,0)
cbind(years,Effort1,Effort2)

FinE_OT30<-round(GatherHalfYears(FinE1_OT30,FinE2_OT30,NumYears),0)
FinE_OT30

FinE_COT30x<-round(cbind(years,FinE1_OT30,FinE2_OT30),0)

########################
# Area 31

FinOT31<-subset(Fin_OT, SUB_DIV==31)
summary(FinOT31)

#attach(FinOT31)

dim(FinOT31)[1]

##############
# Effort
##############
Effort1<-vector()
Effort2<-vector()

for(y in min_year:max_year){
	temp1<-0
	temp2<-0

	for(i in 1:dim(FinOT31)[1]){
    if(FinOT31$TP_TYPE[i]=="HYR"){
      if (FinOT31$YEAR[i]==y && FinOT31$TIME_PERIOD[i]==1){
			   if(is.na(FinOT31$EFFORT[i])==F){temp1<-FinOT31$EFFORT[i]+temp1}
		  }
		  if (FinOT31$YEAR[i]==y && FinOT31$TIME_PERIOD[i]==2){
			   if(is.na(FinOT31$EFFORT[i])==F){temp2<-FinOT31$EFFORT[i]+temp2}
		  }  
    }  
  }
  Effort1[(y-min_year+1)]<-temp1
	Effort2[(y-min_year+1)]<-temp2
}
FinE1_OT31<-Effort1; FinE2_OT31<-Effort2
Effort1<-round(Effort1,0); Effort2<-round(Effort2,0)
cbind(years,Effort1,Effort2)

FinE_OT31<-round(GatherHalfYears(FinE1_OT31,FinE2_OT31,NumYears),0)
FinE_OT31

FinE_COT31x<-round(cbind(years,FinE1_OT31,FinE2_OT31),0)

# these are now the same!
cbind(FinE_OT31+FinE_OT30, FinE_OTtot)
                                                                      
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
################################################################################
# River fishery                                                                  
################################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
fin_river<-subset(finland, FISHERY=="R")
summary(fin_river)
#attach(fin_river)

summary(fin_river$GEAR)
# AN  GND  GNS  LLD  LLS   OT   TN NA's 
#  19    0    0    0    0    0    0    9 
# All catches to the same!
 
summary(fin_river$TP_TYPE)
# Only YR data! This goes as it is, no need to divide half yearly.

####################
# Catch
####################
Catch<-vector()

for(y in min_year:max_year){
	temp<-0

	for(i in 1:dim(fin_river)[1]){
    if (fin_river$YEAR[i]==y){
		  if(is.na(fin_river$NUMB[i])==F){temp<-fin_river$NUMB[i]+temp}
    }
	}
	Catch[(y-min_year+1)]<-temp
}
FinC_river<-Catch
FinC_riverx<-round(cbind(years,Catch),0)

FinC_riverx


