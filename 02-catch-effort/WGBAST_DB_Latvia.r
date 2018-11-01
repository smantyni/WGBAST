## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Latvia

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


latvia<-filter(salmon, COUNTRY=="LV", FISHERY=="S")

latvia%>%count(TP_TYPE)

latvia%>%
  group_by(FISHERY)%>%
  count(GEAR)

################################################################################
#  Driftnetting:                                                                  
################################################################################
# Include all gears into offshore driftnetting

Lat_ODN<-latvia%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))

# View(LatE)
# View(LatC)
# 
# datDN<-as.data.frame(filter(latvia, GEAR=="GND" | GEAR=="MIS" | GEAR=="GNS"))
# summary(datDN)
# #attach(datDN)
# 
# summary(datDN$GEAR)
# summary(datDN$TIME_PERIOD)
# summary(datDN$TP_TYPE)
# # This is now completely monthly data!
# 
# dim(datDN)[1]
# test<-subset(datDN, YEAR==2007)
# test
# 
# ################################################################################
# # Run WGBAST_DB_functions.R
# 
# ##############
# # Effort
# ##############
# 
# #  Annetaan olla OT mukana, vaikka onkin ep?selko.
# 
# LatE_ODN_<-Effort_MON(datDN)
# LatE1_ODN<-LatE_ODN_[,2]; LatE2_ODN<-LatE_ODN_[,3];
# 
# LatE_ODN<-round(GatherHalfYears(LatE1_ODN,LatE2_ODN,NumYears),0)
# LatE_ODN
# 
# LatE_ODNx<-cbind(years,LatE1_ODN,LatE2_ODN) 
# 
# ##############
# # Catches
# ##############
# LatC_ODN_<-Catch_MON(datDN)
# 
# LatC1_ODN<-LatC_ODN_[,2]; LatC2_ODN<-LatC_ODN_[,3];
# 
# LatC_ODN<-round(GatherHalfYears(LatC1_ODN,LatC2_ODN,NumYears),0)
# LatC_ODN
# 
# LatC_ODNx<-cbind(years,LatC1_ODN,LatC2_ODN) 

