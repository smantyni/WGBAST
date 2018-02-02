## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate Finnish CPUE for coastal trapnet and coastal other gear
#              This will be used for calculating Swedish effort for those coastal gears

# R-file:		   WGBAST_DB_Finland_CoastalCPUE.r

# input: 		   WGBAST_DB09.txt
# output:  	

# R ver:	  	  2.8.0

# programmed:		2009 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

################################################################################

# include only those lines of data where both catch and effort data are known (Tapani)

#dat_all <- read.table(
#"C:/Biom/FullLifeHistoryModel/2013/data/der/catch&effort/WGBAST_DB13.txt", header=T)
#summary(dat_all)

salmon<-subset(dat_all, SPECIES=="SAL" & SUB_DIV!=32 & F_TYPE!="DISC" & F_TYPE!="SEAL")
summary(salmon)

finland<-subset(salmon, COUNTRY=="FI")
summary(finland)

fin_coast<-subset(finland, FISHERY=="C")
summary(fin_coast)

################################################################################

################################################################################
#  Coastal trapnetting:
################################################################################
FinCTN<-subset(fin_coast, GEAR=="TN")
summary(FinCTN)

summary(FinCTN$NUMB)
summary(FinCTN$EFFORT)
# check if there are NA's

#attach(FinCTN)
summary(FinCTN$TP_TYPE)
# Only HYR data!

dim(FinCTN)[1]

##############
# Effort
##############
Effort1<-vector()
Effort2<-vector()

for(y in min_year:max_year){
	temp1<-0
	temp2<-0

	for(i in 1:dim(FinCTN)[1]){
    if(FinCTN$TP_TYPE[i]=="HYR"){
      if (FinCTN$YEAR[i]==y && FinCTN$TIME_PERIOD[i]==1){
			   if(is.na(FinCTN$EFFORT[i])==F){temp1<-FinCTN$EFFORT[i]+temp1}
		  }
		  if (FinCTN$YEAR[i]==y && FinCTN$TIME_PERIOD[i]==2){
			   if(is.na(FinCTN$EFFORT[i])==F){temp2<-FinCTN$EFFORT[i]+temp2}
		  }
    }
  }
  Effort1[(y-min_year+1)]<-temp1
	Effort2[(y-min_year+1)]<-temp2
}
FinE1_CTN<-Effort1; FinE2_CTN<-Effort2
Effort1<-round(Effort1,0); Effort2<-round(Effort2,0)
cbind(years,Effort1,Effort2)

####################
# Catches
####################
Catch1<-vector()
Catch2<-vector()

for(y in min_year:max_year){
	temp1<-0
	temp2<-0

	for(i in 1:dim(FinCTN)[1]){
    if(FinCTN$TP_TYPE[i]=="HYR"){
      if (FinCTN$YEAR[i]==y && FinCTN$TIME_PERIOD[i]==1){
			 if(is.na(FinCTN$NUMB[i])==F){temp1<-FinCTN$NUMB[i]+temp1}
		  }
		  if (FinCTN$YEAR[i]==y && FinCTN$TIME_PERIOD[i]==2){
			 if(is.na(FinCTN$NUMB[i])==F){temp2<-FinCTN$NUMB[i]+temp2}
		  }
    }
	}
	Catch1[(y-min_year+1)]<-temp1
	Catch2[(y-min_year+1)]<-temp2
}
FinC1_CTN<-Catch1; FinC2_CTN<-Catch2
cbind(years,Catch1,Catch2)


################################################################################
#  Other coastal gear (OT)
################################################################################
FinCoastOT<-subset(fin_coast, GEAR=="OT")
summary(FinCoastOT)

summary(FinCoastOT$NUMB)
summary(FinCoastOT$EFFORT)
# Check NA's

FinCoastOT_noNA<-subset(FinCoastOT, GEAR=="OT" & is.na(EFFORT)==F)
summary(FinCoastOT_noNA)

#attach(FinCoastOT_noNA)
summary(FinCoastOT_noNA$TP_TYPE)


##############
# Effort
##############
Effort1<-vector()
Effort2<-vector()

for(y in min_year:max_year){
	temp1<-0
	temp2<-0

	for(i in 1:dim(FinCoastOT_noNA)[1]){
    if(FinCoastOT_noNA$TP_TYPE[i]=="HYR"){
      if (FinCoastOT_noNA$YEAR[i]==y && FinCoastOT_noNA$TIME_PERIOD[i]==1){
			   if(is.na(FinCoastOT_noNA$EFFORT[i])==F){temp1<-FinCoastOT_noNA$EFFORT[i]+temp1}
		  }
		  if (FinCoastOT_noNA$YEAR[i]==y && FinCoastOT_noNA$TIME_PERIOD[i]==2){
			   if(is.na(FinCoastOT_noNA$EFFORT[i])==F){temp2<-FinCoastOT_noNA$EFFORT[i]+temp2}
		  }
    }
  }
  Effort1[(y-min_year+1)]<-temp1
	Effort2[(y-min_year+1)]<-temp2
}
FinE1_CoastOT<-Effort1; FinE2_CoastOT<-Effort2
Effort1<-round(Effort1,0); Effort2<-round(Effort2,0)
cbind(years,Effort1,Effort2)

####################
# Catches
####################
Catch1<-vector()
Catch2<-vector()

for(y in min_year:max_year){
	temp1<-0
	temp2<-0

	for(i in 1:dim(FinCoastOT_noNA)[1]){
    if(FinCoastOT_noNA$TP_TYPE[i]=="HYR"){
      if (FinCoastOT_noNA$YEAR[i]==y && FinCoastOT_noNA$TIME_PERIOD[i]==1){
			 if(is.na(FinCoastOT_noNA$NUMB[i])==F){temp1<-FinCoastOT_noNA$NUMB[i]+temp1}
		  }
		  if (FinCoastOT_noNA$YEAR[i]==y && FinCoastOT_noNA$TIME_PERIOD[i]==2){
			 if(is.na(FinCoastOT_noNA$NUMB[i])==F){temp2<-FinCoastOT_noNA$NUMB[i]+temp2}
		  }
    }
	}
	Catch1[(y-min_year+1)]<-temp1
	Catch2[(y-min_year+1)]<-temp2
}
FinC1_CoastOT<-Catch1; FinC2_CoastOT<-Catch2
cbind(years,Catch1,Catch2)
################################################################################

FinCTN1_CPUE<-FinC1_CTN/FinE1_CTN
FinCTN2_CPUE<-FinC2_CTN/FinE2_CTN

FinCOT1_CPUE<-FinC1_CoastOT/FinE1_CoastOT
FinCOT2_CPUE<-FinC2_CoastOT/FinE2_CoastOT


# Then the same for sub divs 30 and 31... 

################################################################################
#  Coastal trapnetting, area 30
################################################################################
FinCTN$SUB_DIV
FinCTN30<-subset(fin_coast, GEAR=="TN" & SUB_DIV==30)
summary(FinCTN30)

summary(FinCTN30$NUMB)
summary(FinCTN30$EFFORT)
# check if there are NA's. None.


#attach(FinCTN30)
summary(FinCTN30$TP_TYPE)
# Only HYR data!

dim(FinCTN30)[1]

##############
# Effort
##############
FinE_CTN30_<-Effort_HYR(FinCTN30)
FinE1_CTN30<-FinE_CTN30_[,2];FinE2_CTN30<-FinE_CTN30_[,3]

FinE_CTN30<-round(GatherHalfYears(FinE1_CTN30,FinE2_CTN30,NumYears),0)
FinE_CTN30


####################
# Catches
####################
FinC_CTN30_<-Catch_HYR(FinCTN30)
FinC1_CTN30<-FinC_CTN30_[,2];FinC2_CTN30<-FinC_CTN30_[,3]

FinC_CTN30<-round(GatherHalfYears(FinC1_CTN30,FinC2_CTN30,NumYears),0)
FinC_CTN30

################################################################################
#  Coastal trapnetting, area 31
################################################################################
FinCTN31<-subset(fin_coast, GEAR=="TN" & SUB_DIV==31)
summary(FinCTN31)

summary(FinCTN31$NUMB)
summary(FinCTN31$EFFORT)
# check if there are NA's. None.

#attach(FinCTN31)
summary(FinCTN31$TP_TYPE)
# Only HYR data!

dim(FinCTN31)[1]

##############
# Effort
##############
FinE_CTN31_<-Effort_HYR(FinCTN31)
FinE1_CTN31<-FinE_CTN31_[,2];FinE2_CTN31<-FinE_CTN31_[,3]

FinE_CTN31<-round(GatherHalfYears(FinE1_CTN31,FinE2_CTN31,NumYears),0)
FinE_CTN31

####################
# Catches
####################
FinC_CTN31_<-Catch_HYR(FinCTN31)
FinC1_CTN31<-FinC_CTN31_[,2];FinC2_CTN31<-FinC_CTN31_[,3]

FinC_CTN31<-round(GatherHalfYears(FinC1_CTN31,FinC2_CTN31,NumYears),0)
FinC_CTN31


################################################################################
#  Coastal other gear, area 30
################################################################################
FinCoastOT$SUB_DIV
FinCOT30<-subset(fin_coast, GEAR=="OT" & SUB_DIV==30 & is.na(EFFORT)==F)
summary(FinCOT30)

summary(FinCOT30$NUMB)
summary(FinCOT30$EFFORT)
# check if there are NA's. None.


#attach(FinCOT30)
summary(FinCOT30$TP_TYPE)
# Only HYR data!

dim(FinCOT30)[1]

##############
# Effort
##############
FinE_COT30_<-Effort_HYR(FinCOT30)
FinE1_COT30<-FinE_COT30_[,2];FinE2_COT30<-FinE_COT30_[,3]

FinE_COT30<-round(GatherHalfYears(FinE1_COT30,FinE2_COT30,NumYears),0)
FinE_COT30


####################
# Catches
####################
FinC_COT30_<-Catch_HYR(FinCOT30)
FinC1_COT30<-FinC_COT30_[,2];FinC2_COT30<-FinC_COT30_[,3]

FinC_COT30<-round(GatherHalfYears(FinC1_COT30,FinC2_COT30,NumYears),0)
FinC_COT30

################################################################################
#  Coastal trapnetting, area 31
################################################################################
FinCOT31<-subset(fin_coast, GEAR=="OT" & SUB_DIV==31 & is.na(EFFORT)==F)
summary(FinCOT31)

summary(FinCOT31$NUMB)
summary(FinCOT31$EFFORT)
# check if there are NA's. None.

#attach(FinCOT31)
summary(FinCOT31$TP_TYPE)
# Only HYR data!

dim(FinCOT31)[1]

##############
# Effort
##############
FinE_COT31_<-Effort_HYR(FinCOT31)
FinE1_COT31<-FinE_COT31_[,2];FinE2_COT31<-FinE_COT31_[,3]

FinE_COT31<-round(GatherHalfYears(FinE1_COT31,FinE2_COT31,NumYears),0)
FinE_COT31

#FinE_COT31x<-round(cbind(years,FinE1_COT31,FinE2_COT31),0)

####################
# Catches
####################
FinC_COT31_<-Catch_HYR(FinCOT31)
FinC1_COT31<-FinC_COT31_[,2];FinC2_COT31<-FinC_COT31_[,3]

FinC_COT31<-round(GatherHalfYears(FinC1_COT31,FinC2_COT31,NumYears),0)
FinC_COT31

#FinC_COT31x<-round(cbind(years,FinC1_COT31,FinC2_COT31),0)


################################################################################
# Then CPUE's...

FinCTN30_CPUE1<-FinC1_CTN30/FinE1_CTN30
FinCTN30_CPUE2<-FinC2_CTN30/FinE2_CTN30

FinCTN31_CPUE1<-FinC1_CTN31/FinE1_CTN31
FinCTN31_CPUE2<-FinC2_CTN31/FinE2_CTN31

# Coastal other fishery. No real point in dividing CPUE between areas 30/31,
# use FinCOT1_CPUE and FinCOT2_CPUE instead.

#FinCOT30_CPUE1<-FinC1_COT30/FinE1_COT30
#FinCOT30_CPUE2<-FinC2_COT30/FinE2_COT30

#FinCOT31_CPUE1<-FinC1_COT31/FinE1_COT31
#FinCOT31_CPUE2<-FinC2_COT31/FinE2_COT31


