## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Denmark

# R-file:		   WGBAST_DB_Denmark.r

# input: 		   WGBAST_DB09.txt
# output:  	

# R ver:	  	  2.8.0

# programmed:		2009 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


salmon<-subset(dat_all, SPECIES=="SAL" & SUB_DIV!=32 & F_TYPE!="DISC" & F_TYPE!="SEAL")
summary(salmon)

denmark<-subset(salmon, COUNTRY=="DK")
summary(denmark)

# We are interested in the OLL and ODN in case of Denmark

summary(denmark$GEAR)
# AN FPO GND GNS GTR LLD LLS MIS  OT OTB SDN  TN 
#  8   0  88   0   0  76   0   0   0   0   0   0 
# AN  GND  GNS  LLD  LLS   OT   TN NA's 
#  10   88    0   97    0    0    0   35

summary(denmark$FISHERY)
#  C  CR   R   S  SC 
# 11   0   0 219   0 
   
coast_den<-subset(denmark, FISHERY=="C")
summary(coast_den$GEAR)
# AN  GND  GNS  LLD  LLS   OT   TN NA's 
#   9    0    0    0    0    0    0    2 
# Catherine: Coastal angling goes to offshore longline
# Same stuff for NA-fishery, since no other option left.. (02/10)
subset(denmark, is.na(GEAR)==T)
# But since this is before 2000, don't mind about it!

summary(as.factor(denmark$TIME_PERIOD))
summary(denmark$TP_TYPE)

summary(denmark)

################################################################################
#  Driftnetting:                                                                  
################################################################################

DenODN<-subset(denmark, GEAR=="GND")
#attach(DenODN)
summary(DenODN)
yearly<-subset(DenODN, TP_TYPE=="YR")
yearly
# Let's take into account only monthly and half yearly Dena 
#( in this case 2001-> is okay):
# count effort and catch for each year for half years

dim(DenODN)[1]

##############
# Effort
##############
DenE_ODN_<-Effort_MONandHYR(DenODN)
DenE1_ODN<-DenE_ODN_[,2]; DenE2_ODN<-DenE_ODN_[,3];

DenE_ODN<-round(GatherHalfYears(DenE1_ODN,DenE2_ODN,NumYears),0)
DenE_ODN

DenE_ODNx<-cbind(years,DenE1_ODN,DenE2_ODN) 

####################
# Catches
####################
DenC_ODN_<-Catch_MONandHYR(DenODN)
DenC1_ODN<-DenC_ODN_[,2]; DenC2_ODN<-DenC_ODN_[,3];

DenC_ODN<-round(GatherHalfYears(DenC1_ODN,DenC2_ODN,NumYears),0)
DenC_ODN

DenC_ODNx<-cbind(years,DenC1_ODN,DenC2_ODN) 
                                     
################################################################################
#  Longlining:
################################################################################
# Add to this also Coastal angling catch!!!!!!!!!
# Effortille ei kait tehdä mitään, kun noita lienee paha yhdistellä vai kuinka? (Tapsa!)

DenOLL_eff<-subset(denmark, GEAR=="LLD" | GEAR=="LL")
DenOLL_catch<-subset(denmark, GEAR=="LLD"| GEAR=="LL" | GEAR=="AN")
summary(DenOLL_catch)
summary(DenOLL_eff)
subset(DenOLL_eff, TP_TYPE=="YR") # YR data only for 2000 so skip this
summary(subset(DenOLL_catch, TP_TYPE=="YR")) # this needs to be taken into account
# data rows with yearly data 
#subset(DenOLL_eff, YEAR==2016)
##############
# Effort
##############
#attach(DenOLL_eff)
DenE_OLL_<-Effort_MONandHYR(DenOLL_eff)
DenE1_OLL<-DenE_OLL_[,2]; DenE2_OLL<-DenE_OLL_[,3];

DenE_OLL<-round(GatherHalfYears(DenE1_OLL,DenE2_OLL,NumYears),0)
DenE_OLL

DenE_OLLx<-cbind(years,DenE1_OLL,DenE2_OLL) 

####################
# Catches
####################
dat<-DenOLL_catch
summary(DenOLL_catch)
dim(DenOLL_catch)[1]

Catch1<-vector()
Catch2<-vector()
propC_HYR1<-vector()

for(y in min_year:max_year){
#	y<-max_year
  temp1<-0
	temp2<-0
	temp3<-0

	for(i in 1:dim(dat)[1]){
#i<-1
      if(dat$TP_TYPE[i]=="MON"){
		  if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]<7){
			 if(is.na(dat$NUMB[i])==F){temp1<-dat$NUMB[i]+temp1}
		  }
		  if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]>6){
			 if(is.na(dat$NUMB[i])==F){temp2<-dat$NUMB[i]+temp2}
		  }
    }
    if(dat$TP_TYPE[i]=="HYR"){
    	if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==1){
			 if(is.na(dat$NUMB[i])==F){temp1<-dat$NUMB[i]+temp1}
		  }
		  if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==2){
			 if(is.na(dat$NUMB[i])==F){temp2<-dat$NUMB[i]+temp2}
		  }
    }
	}

	# Count proportion of catch on first half year
	propC_HYR1[(y-min_year+1)]<-temp1/(temp1+temp2)
	
	# Count the sum of catch reported as yearly..
	for(i in 1:dim(dat)[1]){
    if(dat$TP_TYPE[i]=="YR" && dat$YEAR[i]==y){
     	        if(is.na(dat$NUMB[i])==F){temp3<-dat$NUMB[i]+temp3}
    }
  }
  # ... divide it based on the half yearly proportion and 
  # add it to the half yearly catches
  temp1<-temp1 + temp3 * propC_HYR1[(y-min_year+1)]
  temp2<-temp2 + temp3 * (1-propC_HYR1[(y-min_year+1)])

	Catch1[(y-min_year+1)]<-temp1
	Catch2[(y-min_year+1)]<-temp2
}
DenC1_OLL<-Catch1; DenC2_OLL<-Catch2
cbind(years,round(propC_HYR1,2))
cbind(years,round(Catch1,0),round(Catch2))

DenC_OLL<-round(GatherHalfYears(DenC1_OLL,DenC2_OLL,NumYears),0)
DenC_OLL

DenC_OLLx<-cbind(years,DenC1_OLL,DenC2_OLL) 

