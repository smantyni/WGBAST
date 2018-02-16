## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches for Germany

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


germany<-filter(salmon, COUNTRY=="DE")

germany%>%
  count(TP_TYPE)
# quarterly data

germany%>%
  group_by(FISHERY)%>%
  count(GEAR)

filter(germany, FISHERY=="SC")
filter(germany, GEAR=="GND")
#View(tmp)

################################################################################
#  Offshore catches                                                                  
################################################################################
# All german fishery goes to LLD (no effort)

#datLL<-subset(germany, GEAR=="LLD" | GEAR=="OT" | GEAR=="GND")
datLL<-germany
summary(datLL)

summary(datLL$GEAR)
summary(datLL$TIME_PERIOD)
summary(datLL$TP_TYPE)
# Quarterly data only

dim(datLL)[1]
subset(datLL, YEAR==2007)


##############
# Catches
##############
GerC<-Catch_QUART(datLL)

GerC1<-GerC[,2]; GerC2<-GerC[,3];

GerC<-round(GatherHalfYears(GerC1,GerC2,NumYears),0)
GerC

GerCx<-cbind(years,GerC1,GerC2) 
GerCx
