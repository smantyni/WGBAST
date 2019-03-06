## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Latvia

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


latvia<-filter(salmon, COUNTRY=="LV", FISHERY=="O")

latvia%>%count(TP_TYPE)
# Only MON/QTR

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

