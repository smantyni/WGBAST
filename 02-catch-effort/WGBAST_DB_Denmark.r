## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Denmark

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


denmark<-filter(salmon, COUNTRY=="DK")


# We are interested in the LLD and GND in case of Denmark

denmark%>%count(GEAR)
# A tibble: 6 x 2
#GEAR     n
#<chr> <int>
#1    AN    17
#2   FYK     3
#3   GND    83
#4   GNS     1
#5   LLD   189
#6   MIS     6

denmark%>%count(FISHERY)
# A tibble: 1 x 2
#FISHERY     n
#<chr> <int>
#  1       S   299


################################################################################
#  Driftnetting:                                                                  
################################################################################

DenODN<-subset(denmark, GEAR=="GND")
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

DenOLL_eff<-subset(denmark, GEAR=="LLD")
DenOLL_catch<-subset(denmark, GEAR!="GND") # catch from all gears except GND goes to LLD
summary(DenOLL_catch)
summary(DenOLL_eff)
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

