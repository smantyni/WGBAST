## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Poland

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

#! #############################################################################
#! 13.2.2015:
#! Calculate both polish sal & trs effort and choose which ever is larger.
#!
#! #############################################################################
#! 19.2.2014:
#! We estimate PL catch for 2000-2008 with PL TRS effort and other countries
#! combined CPUE*0.75, and from 2009 onwards with PL reported SAL+TRS catch*0.97
#! #############################################################################


poland_catch<-subset(df, (SPECIES=="SAL" | SPECIES=="TRS") &  COUNTRY=="PL" & FISHERY=="S")
poland_catch_coast<-subset(df, SPECIES=="SAL" &  COUNTRY=="PL" & FISHERY=="C")

poland_eff_sal<-subset(df, SPECIES=="SAL" &  COUNTRY=="PL" & FISHERY=="S")
poland_eff_trs<-subset(df, SPECIES=="TRS" &  COUNTRY=="PL" & FISHERY=="S")

plsal<-poland_eff_sal%>%
  group_by(GEAR,YEAR)%>%
  summarise(Esal=sum(EFFORT), Csal=sum(NUMB))

pltrs<-poland_eff_trs%>%
  group_by(GEAR,YEAR)%>%
  summarise(Etrs=sum(EFFORT), Ctrs=sum(NUMB))

#View(full_join(plsal, pltrs, by=c("GEAR","YEAR")))


# Driftnetting
# ===================
PolC_ODN<-subset(poland_catch, GEAR=="GND")
dim(PolC_ODN)[1]

Catch<-vector()

for(y in min_year:max_year){
	temp1<-0

	for(i in 1:dim(PolC_ODN)[1]){
		if (PolC_ODN$YEAR[i]==y){
			if(is.na(PolC_ODN$NUMB[i])==F){temp1<-PolC_ODN$NUMB[i]+temp1}
		}
	}
	Catch[(y-min_year+1)]<-temp1
}

PolC1_ODN<-Catch*propC1_ODN
PolC2_ODN<-Catch*(1-propC1_ODN)
cbind(PolC1_ODN,PolC2_ODN)

PolC_ODN<-round(GatherHalfYears(PolC1_ODN,PolC2_ODN,NumYears),0)
PolC_ODN

# Coastal catch (incl. 22.2.17)
# ===================

PolC_coast<-poland_catch_coast
dim(PolC_coast)[1]
PolC_coast%>%group_by(YEAR)%>%count(TP_TYPE)
# HYR, YR & MON data (HYR in 01-02 so forget)

# YR data
Catch_yr<-vector()
for(y in 2003:2009){
  temp1<-0
  
  for(i in 1:dim(PolC_coast)[1]){
    if (PolC_coast$YEAR[i]==y){
      if(is.na(PolC_coast$NUMB[i])==F){temp1<-PolC_coast$NUMB[i]+temp1}
    }
    Catch_yr[(y-2003+1)]<-temp1
  }	
}

cbind(min_year:max_year,propC1_OLL)
PolC1_coast_yr<-Catch_yr*propC1_OLL[4:10]
PolC2_coast_yr<-Catch_yr*(1-propC1_OLL[4:10])
PolC1_coast_yr
PolC1_coast<-PolC1_coast_yr
PolC2_coast<-PolC2_coast_yr

# MON data, 2010!
PolC1_coast_mon<-vector()
PolC2_coast_mon<-vector()
dat<-PolC_coast

for(y in 2010:max_year){
  temp1<-0
  temp2<-0
  
  for(i in 1:dim(dat)[1]){
    
    if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]<7){
      if(is.na(dat$NUMB[i])==F){temp1<-dat$NUMB[i]+temp1}
    }
    if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]>6){
      if(is.na(dat$NUMB[i])==F){temp2<-dat$NUMB[i]+temp2}
    }
  }
  PolC1_coast_mon[(y-2010+1)]<-temp1
  PolC2_coast_mon[(y-2010+1)]<-temp2
}

#2010
PolC1_coast_mon<-round(PolC1_coast_mon,0) 
PolC2_coast_mon<-round(PolC2_coast_mon,0)
PolC1_coast_mon
PolC2_coast_mon

length(PolC1_coast_yr)
# Combine
PolC1_coast[8:(length(PolC1_coast_yr)+length(PolC1_coast_mon))]<-PolC1_coast_mon
PolC2_coast[8:(length(PolC1_coast_yr)+length(PolC1_coast_mon))]<-PolC2_coast_mon

cbind(PolC1_coast,PolC2_coast)

PolC_coast<-round(GatherHalfYears(PolC1_coast,PolC2_coast,length(PolC1_coast)),0)
PolC_coast



# Longline catch
# ===================
PolC_OLL<-filter(poland_catch, GEAR=="LLD"|GEAR=="MIS"| GEAR=="GNS")
#GEAR#subset(poland_catch, GEAR=="LLD" | GEAR=="MIS")

PolC_OLL%>%count(GEAR)

PolC_OLL%>%count(TP_TYPE)
# MON, HYR & YR data!

filter(PolC_OLL, YEAR>2009)%>%group_by(GEAR,YEAR)%>%count(TP_TYPE)
# MON DATA!

dim(PolC_OLL)[1]

# YR data 2003-2009!
Catch_yr<-vector()
for(y in 2003:2009){
  temp1<-0
  
  for(i in 1:dim(PolC_OLL)[1]){
    if (PolC_OLL$YEAR[i]==y){
      if(is.na(PolC_OLL$NUMB[i])==F){temp1<-PolC_OLL$NUMB[i]+temp1}
    }
    Catch_yr[(y-2003+1)]<-temp1
  }	
}

cbind(min_year:max_year,propC1_OLL)
PolC1_OLL_yr<-Catch_yr*propC1_OLL[4:10]
PolC2_OLL_yr<-Catch_yr*(1-propC1_OLL[4:10])
PolC1_OLL_yr
PolC1_OLL<-PolC1_OLL_yr
PolC2_OLL<-PolC2_OLL_yr

# MON data, 2010
PolC1_OLL_mon<-vector()
PolC2_OLL_mon<-vector()
dat<-PolC_OLL

for(y in 2010:(max_year)){ # NOTE!!!!!!!!!! CHECK IF THIS STILL HOLDS!!!!!
  temp1<-0
  temp2<-0
  
  for(i in 1:dim(dat)[1]){
    
    if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]<7){
      if(is.na(dat$NUMB[i])==F){temp1<-dat$NUMB[i]+temp1}
    }
    if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]>6){
      if(is.na(dat$NUMB[i])==F){temp2<-dat$NUMB[i]+temp2}
    }
  }
  PolC1_OLL_mon[(y-2010+1)]<-temp1
  PolC2_OLL_mon[(y-2010+1)]<-temp2
}

#2010-2016
PolC1_OLL_mon<-round(PolC1_OLL_mon,0) 
PolC2_OLL_mon<-round(PolC2_OLL_mon,0)
PolC1_OLL_mon
PolC2_OLL_mon




length(PolC1_OLL_yr)
# Combine
L1<-(length(PolC1_OLL_yr)+length(PolC1_OLL_mon))
PolC1_OLL[8:L1]<-PolC1_OLL_mon
PolC2_OLL[8:L1]<-PolC2_OLL_mon

#L2<-(length(PolC1_OLL_yr)+length(PolC1_OLL_mon)+length(PolC1_OLL_hyr))
#PolC1_OLL[(L1+1):L2]<-PolC1_OLL_hyr
#PolC2_OLL[(L1+1):L2]<-PolC2_OLL_hyr

cbind(PolC1_OLL,PolC2_OLL)

PolC_OLL<-round(GatherHalfYears(PolC1_OLL,PolC2_OLL,length(PolC1_OLL)),0)
PolC_OLL


# Effort
# ==============================================================================
# Driftnetting
# ===================
# Use trout effort for driftnetting (minor issue as DN ended in '08)
PolE_ODN<-subset(poland_eff_trs, GEAR=="GND")
summary(PolE_ODN)
dim(PolE_ODN)[1]

Effort<-vector()
for(y in min_year:max_year){
	temp1<-0

	for(i in 1:dim(PolE_ODN)[1]){
		if(PolE_ODN$YEAR[i]==y){
			if(is.na(PolE_ODN$EFFORT[i])==F){temp1<-PolE_ODN$EFFORT[i]+temp1}
		}
	}
	Effort[(y-min_year+1)]<-temp1
}

PolE1_ODN<-Effort*propE1_ODN
PolE2_ODN<-Effort*(1-propE1_ODN)
cbind(PolE1_ODN,PolE2_ODN)

PolE_ODNx<-round(cbind(years,PolE1_ODN,PolE2_ODN),0)


PolE_ODN<-round(GatherHalfYears(PolE1_ODN,PolE2_ODN,NumYears),0)
PolE_ODN




# Longline effort
# ========================================


PolE_OLL_S<-subset(poland_eff_sal, GEAR=="LLD")
PolE_OLL_T<-subset(poland_eff_trs, GEAR=="LLD")
dim(PolE_OLL_S)[1]
dim(PolE_OLL_T)[1]


PolE_OLL_S%>%count(TP_TYPE)
PolE_OLL_T%>%count(TP_TYPE)
# MON, HYR & YR data!

filter(PolE_OLL_S, YEAR>2009)%>%group_by(GEAR,YEAR)%>%count(TP_TYPE)
filter(PolE_OLL_T, YEAR>2009)%>%group_by(GEAR,YEAR)%>%count(TP_TYPE)
# YR data until 2009
# MON data 2010->

PolE1_OLL<-vector()
PolE2_OLL<-vector()

# YR data!
EffortS<-c()
EffortT<-c()
Effort<-c()
for(y in 2003:2009){
#y<-2003
	temp1<-0
	temp2<-0

 	for(i in 1:dim(PolE_OLL_S)[1]){
		  if(PolE_OLL_S$YEAR[i]==y){
		  	if(is.na(PolE_OLL_S$EFFORT[i])==F){temp1<-PolE_OLL_S$EFFORT[i]+temp1}
	   	}
	 EffortS[(y-2003+1)]<-temp1
	}	

 	for(i in 1:dim(PolE_OLL_T)[1]){
		  if(PolE_OLL_T$YEAR[i]==y){
		  	if(is.na(PolE_OLL_T$EFFORT[i])==F){temp2<-PolE_OLL_T$EFFORT[i]+temp2}
	   	}
	 EffortT[(y-2003+1)]<-temp2
	}	
  Effort[(y-2003+1)]<-max(EffortT[(y-2003+1)], EffortS[(y-2003+1)])
}

cbind(EffortT, EffortS, Effort)


cbind(min_year:max_year,propE1_OLL)
PolE1_OLL_yr<-Effort*propE1_OLL[4:10]
PolE2_OLL_yr<-Effort*(1-propE1_OLL[4:10])

# MON data!

PolE1_OLL_mon<-vector()
PolE2_OLL_mon<-vector()
dat1<-PolE_OLL_S
dat2<-PolE_OLL_T

for(y in 2010:(max_year)){
  #y<-(max_year-1)
	temp1S<-0
	temp2S<-0
  temp1T<-0
	temp2T<-0

  # salmon:
  for(i in 1:dim(dat1)[1]){
    if(dat1$YEAR[i]==y && dat1$TIME_PERIOD[i]<7){
			 if(is.na(dat1$EFFORT[i])==F){temp1S<-dat1$EFFORT[i]+temp1S}
		}
		if(dat1$YEAR[i]==y && dat1$TIME_PERIOD[i]>6){
	   	 if(is.na(dat1$EFFORT[i])==F){temp2S<-dat1$EFFORT[i]+temp2S}
    }
  }
  # trout:
  for(i in 1:dim(dat2)[1]){
    if(dat2$YEAR[i]==y && dat2$TIME_PERIOD[i]<7){
			 if(is.na(dat2$EFFORT[i])==F){temp1T<-dat2$EFFORT[i]+temp1T}
		}
		if(dat2$YEAR[i]==y && dat2$TIME_PERIOD[i]>6){
	   	 if(is.na(dat2$EFFORT[i])==F){temp2T<-dat2$EFFORT[i]+temp2T}
    }
  }
	 
	 PolE1_OLL_mon[(y-2010+1)]<-max(temp1S, temp1T)
	 PolE2_OLL_mon[(y-2010+1)]<-max(temp2S, temp2T)
}
PolE1_OLL_mon<-round(PolE1_OLL_mon,0) 
PolE2_OLL_mon<-round(PolE2_OLL_mon,0)
PolE1_OLL_mon
PolE2_OLL_mon


length(PolE1_OLL_yr)
# Combine
L1<-(length(PolE1_OLL_yr)+length(PolE1_OLL_mon))
PolE1_OLL[8:L1]<-PolE1_OLL_mon
PolE2_OLL[8:L1]<-PolE2_OLL_mon

#L2<-(length(PolE1_OLL_yr)+length(PolE1_OLL_mon)+length(PolE1_OLL_hyr))
#PolE1_OLL[(L1+1):L2]<-PolE1_OLL_hyr
#PolE2_OLL[(L1+1):L2]<-PolE2_OLL_hyr

round(cbind(PolE1_OLL,PolE2_OLL),0)



PolE_OLLx<-round(cbind(years[4:length(years)],PolE1_OLL,PolE2_OLL),0)

PolE_OLL<-round(GatherHalfYears(PolE1_OLL,PolE2_OLL,length(PolE1_OLL)),0)
PolE_OLL


################################################################################
# Now, if we want to calculate Polish catches by using Polish effort and CPUE 
# combined from other countries, we get:

PolC1_OLL_new<-PolE1_OLL*CPUE1_OLL[4:length(CPUE1_OLL)]*0.75 
PolC2_OLL_new<-PolE2_OLL*CPUE2_OLL[4:length(CPUE2_OLL)]*0.75

# to compare reported catch and catch generated from effort (and CPUE of others) 
round(cbind(years[4:length(years)],PolC1_OLL*0.97, PolC1_OLL_new, 
PolC2_OLL*0.97, PolC2_OLL_new),0)

#PolC_OLLx<-round(cbind(years[4:length(years)],PolC1_OLL_new,PolC2_OLL_new),0)

#PolC1_coastZ<-c(rep(0,6),PolC1_coast)
#PolC2_coastZ<-c(rep(0,6),PolC2_coast)

# This combines different methods!! 2009 is the change point in time
PolC_OLLx<-round(cbind(years[4:length(years)],
c(PolC1_OLL_new[1:6],PolC1_OLL[7:(length(years)-3)]*0.97)+PolC1_coast, # 1:6 = 2000-2008
c(PolC2_OLL_new[1:6],PolC2_OLL[7:(length(years)-3)]*0.97)+PolC2_coast)
,0)
PolC_OLLx



PolC_OLL_new<-GatherHalfYears(PolC1_OLL_new,PolC2_OLL_new,length(PolC1_OLL_new))

#!!!!!!!!!!!!!!!!!!!!!!!!!!!
#round(PolC_OLL_new*0.75,0)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!

round(PolC_OLL_new,0)

PolC1_ODN_new<-PolE1_ODN*CPUE1_ODN*0.75
PolC2_ODN_new<-PolE2_ODN*CPUE2_ODN*0.75

PolC_ODNx<-round(cbind(years,PolC1_ODN_new,PolC2_ODN_new),0)

PolC_ODN_new<-round(GatherHalfYears(PolC1_ODN_new,PolC2_ODN_new,NumYears),0)
PolC_ODN_new

# compare with the reported:

length(years)
cbind(PolC_OLL_new*0.75, PolC_OLL, PolC_OLL_new-PolC_OLL, 
(PolC_OLL_new-PolC_OLL)/PolC_OLL)

cbind(PolC_ODN_new, PolC_ODN, (PolC_ODN_new-PolC_ODN)/PolC_ODN)
sum((PolC_ODN_new-PolC_ODN)[7:16,])




