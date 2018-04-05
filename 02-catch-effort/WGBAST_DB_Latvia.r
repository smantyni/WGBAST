## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Latvia

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


salmon<-filter(df, SPECIES=="SAL", SUB_DIV!=32, F_TYPE!="DISC", F_TYPE!="SEAL")
#View(salmon)

latvia<-filter(salmon, COUNTRY=="LV", FISHERY=="S")
summary(latvia)

# test
filter(latvia, TP_TYPE=="YR", YEAR>2000)
# all data monthly after 2000!
filter(latvia, GEAR=="OT")
# not OT gear

################################################################################
#  Driftnetting:                                                                  
################################################################################
# We are interested only on the offshore driftnetting, icluding gears
# GND, GNS and OT

datDN<-as.data.frame(filter(latvia, GEAR=="GND" | GEAR=="OT" | GEAR=="GNS"))
summary(datDN)
#attach(datDN)

summary(datDN$GEAR)
summary(datDN$TIME_PERIOD)
summary(datDN$TP_TYPE)
# This is now completely monthly data!

dim(datDN)[1]
test<-subset(datDN, YEAR==2007)
test

################################################################################
# Run WGBAST_DB_functions.R

##############
# Effort
##############

#  Annetaan olla OT mukana, vaikka onkin ep?selko.

LatE_ODN_<-Effort_MON(datDN)
LatE1_ODN<-LatE_ODN_[,2]; LatE2_ODN<-LatE_ODN_[,3];

LatE_ODN<-round(GatherHalfYears(LatE1_ODN,LatE2_ODN,NumYears),0)
LatE_ODN

LatE_ODNx<-cbind(years,LatE1_ODN,LatE2_ODN) 

##############
# Catches
##############
LatC_ODN_<-Catch_MON(datDN)

LatC1_ODN<-LatC_ODN_[,2]; LatC2_ODN<-LatC_ODN_[,3];

LatC_ODN<-round(GatherHalfYears(LatC1_ODN,LatC2_ODN,NumYears),0)
LatC_ODN

LatC_ODNx<-cbind(years,LatC1_ODN,LatC2_ODN) 

