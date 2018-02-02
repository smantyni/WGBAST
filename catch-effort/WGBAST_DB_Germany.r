## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches for Germany

# R-file:		   WGBAST_DB_Germany.r

# input: 		   WGBAST_CEDB17.txt
# output:  	

# R ver:	  	 3.3.2

# programmed:		2017 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

salmon<-subset(dat_all, SPECIES=="SAL" & SUB_DIV!=32 & F_TYPE!="DISC" & F_TYPE!="SEAL")
summary(salmon)

germany<-subset(salmon, COUNTRY=="DE")# & FISHERY=="S")
summary(germany)
test<-subset(germany, TP_TYPE=="YR")
test
# quarterly data after 2000


################################################################################
#  Offshore catches                                                                  
################################################################################
# We are interested only in the offshore catches, icluding gears
# LLD, GND and OT

datLL<-subset(germany, GEAR=="LLD" | GEAR=="OT" | GEAR=="GND")
summary(datLL)

summary(datLL$GEAR)
summary(datLL$TIME_PERIOD)
summary(datLL$TP_TYPE)
# Quarterly data only

dim(datLL)[1]
test<-subset(datLL, YEAR==2007)
test


##############
# Catches
##############
GerC<-Catch_QUART(datLL)

GerC1<-GerC[,2]; GerC2<-GerC[,3];

GerC<-round(GatherHalfYears(GerC1,GerC2,NumYears),0)
GerC

GerCx<-cbind(years,GerC1,GerC2) 
GerCx
