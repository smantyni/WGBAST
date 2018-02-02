## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Latvia

# R-file:		   WGBAST_DB_Latvia.r

# input: 		   WGBAST_DB09.txt
# output:  	

# R ver:	  	  2.8.0

# programmed:		2009 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


#dat_all <- read.table(
#"C:/Biom/FullLifeHistoryModel/2013/data/der/catch&effort/WGBAST_DB13.txt", header=T)
#summary(dat_all)

salmon<-subset(dat_all, SPECIES=="SAL" & SUB_DIV!=32 & F_TYPE!="DISC" & F_TYPE!="SEAL")
summary(salmon)

latvia<-subset(salmon, COUNTRY=="LV" & FISHERY=="S")
summary(latvia)
test<-subset(latvia, TP_TYPE=="YR")
test
# all data monthly after 2000!

test2<-subset(latvia, GEAR=="OT")
summary(test2)


################################################################################
#  Driftnetting:                                                                  
################################################################################
# We are interested only on the offshore driftnetting, icluding gears
# GND, GNS and OT

datDN<-subset(latvia, GEAR=="GND" | GEAR=="OT" | GEAR=="GNS")
summary(datDN)
#attach(datDN)

summary(datDN$GEAR)
summary(datDN$TIME_PERIOD)
summary(datDN$TP_TYPE)
# This is now complitely monthly data!

dim(datDN)[1]
test<-subset(datDN, YEAR==2007)
test

################################################################################
# Run WGBAST_DB_functions.R

##############
# Effort
##############

#  Annetaan olla OT mukana, vaikka onkin epäselko.

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
