## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:   Combine effort data for scenarios

# c:		2015 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


#Run first CatchEffort.r
source("02-catch-effort/CatchEffort.r")

yearX<-2017 # Assessment year -1

# =======================================================================
# Longlining:
# =====================

# Copy last observed effort at HYR==1 for assessment year for scenarios

# Denmark
DenE_OLL<-Den_OLL%>%select(-Catch)
tmp<-filter(DenE_OLL, YEAR==yearX, HYR==1)%>% # Change!
  ungroup()%>%
  mutate(YEAR=YEAR+1) # Assessement year

full_join(tmp,DenE_OLL)%>%
  mutate(Myear=ifelse(HYR==2, YEAR, YEAR-1))%>% # Model year
  ungroup()%>%
  group_by(Myear)%>%
  summarise(Effort=sum(Effort)) # if there were NA's you'd see it here

# Poland
tmp<-filter(PolE_OLL, YEAR==yearX, HYR==1)%>% # Change!
  ungroup()%>%
  mutate(YEAR=YEAR+1) # Assessement year

full_join(tmp,PolE_OLL)%>%
  mutate(Myear=ifelse(HYR==2, YEAR, YEAR-1))%>% # Model year
  ungroup()%>%
  group_by(Myear)%>%
  summarise(Effort=sum(Effort)) # if there were NA's you'd see it here

# Trolling
(OLL_CPUE<-full_join(OLL_E,OLL_C)%>%
  mutate(CPUE=Catch/Effort)%>%
  filter(Myear<yearX))

OLL_CPUE%>%
  filter(Myear<yearX& Myear>(yearX-5))%>% 
  summarise(CPUE=sum(Catch)/sum(Effort))%>%# Average over 2013-2016
  mutate(YEAR=yearX)

# Plug the above value into Catch&Effort.xlsx F75

  
# =======================================================================
# Trapnetting & Gillnetting:
# =====================

# This goes to EffortCTN_ICES.txt
############################
cbind(
FinE30_CTN%>%mutate(Fin30=Effort/1000)%>%select(-Effort),
SweE30_CTN%>%mutate(Swe30=Effort/1000)%>%select(-YEAR, -Effort),
FinE31_CTN%>%mutate(Fin31=Effort/1000)%>%select(-YEAR, -Effort),
SweE31_CTN%>%mutate(Swe31=Effort/1000)%>%select(-YEAR, -Effort))


# This goes to EffortCGN_ICES.txt
############################
cbind(
  FinE30_COT%>%mutate(Fin30=Effort/100000)%>%select(-Effort),
  SweE30_COT%>%mutate(Swe30=Effort/100000)%>%select(-YEAR, -Effort),
  FinE31_COT%>%mutate(Fin31=Effort/100000)%>%select(-YEAR, -Effort),
  SweE31_COT%>%mutate(Swe31=Effort/100000)%>%select(-YEAR, -Effort))


# =======================================================================
# Note! Kate (or Adam or someone else) will ask OLL efforts from all countries
# for a certain graph in the report. Print the following (half year
# specific numbers) for them. 2014 Finnish longline effort is to be ignored
# (Tapsa will give details on this if needed). 
PolE_OLL
Fin_OLL%>%select(-Catch)
DenE_OLL
SweE_OLL
# =======================================================================


