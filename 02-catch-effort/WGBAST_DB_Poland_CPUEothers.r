## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Poland based on CPUE's of Finland,
#              Sweden, Denmark and Latvia

# R-file:		   WGBAST_DB_Poland_CPUEothers.r

# input: 		   WGBAST_DB09.txt
# output:  	

# R ver:	  	 2.8.0

# programmed:	 2009 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


# Calculate proportions of effort and catch for 1. HYR and 2. HYR
# from combined FIN, SWE, DEN and LAT data for different years and for ODN and OLL.
# Then Divide Polish catch and effort for each HYR based on those.

# Calculate combined CPUE from FIN, SWE, DEN and LAT data for ODN and OLL (only those data
# lines taken into account where both information are known)
# With this we can calculate new Polish catch based on other countries CPUE and Polish effort


# Latvia
# ============
latvia<-filter(salmon, COUNTRY=="LV", FISHERY=="S")
summary(latvia)
# Driftnetting
LatDN<-filter(latvia, GEAR=="GND" | GEAR=="GNS")
LatDN%>%count(TP_TYPE) # monthly Data

LatE_ODN<-Effort_MON(LatDN)

LatE1_ODN<-LatE_ODN[,2]; LatE2_ODN<-LatE_ODN[,3]
LatC_ODN<-Catch_MON(LatDN)
LatC1_ODN<-LatC_ODN[,2]; LatC2_ODN<-LatC_ODN[,3]

# Denmark
# ============
denmark<-subset(salmon, COUNTRY=="DK" & FISHERY=="S")
denmark%>%count(TP_TYPE)

# Driftnetting
DenDN<-subset(denmark, GEAR=="GND")
DenDN%>%count(TP_TYPE) # HYR and MON data
filter(DenDN, is.na(EFFORT)==T) # noNA's in NUMB or EFFORT
filter(DenDN, is.na(NUMB)==T) # noNA's in NUMB or EFFORT

DenE_ODN<-Effort_MONandHYR(DenDN)
DenE1_ODN<-DenE_ODN[,2]; DenE2_ODN<-DenE_ODN[,3]
DenC_ODN<-Catch_MONandHYR(DenDN)
DenC1_ODN<-DenC_ODN[,2]; DenC2_ODN<-DenC_ODN[,3]
# Longlining
DenLL<-subset(denmark, GEAR=="LLD")
DenLL%>%count(TP_TYPE) # HYR and MON data
filter(DenLL, is.na(EFFORT)==T) # 34 NA in EFFORT!!!
filter(DenLL, is.na(NUMB)==T) # noNA's in NUMB

DenE_OLL<-Effort_MONandHYR(filter(DenLL, is.na(EFFORT)==F))
DenE1_OLL<-DenE_OLL[,2]; DenE2_OLL<-DenE_OLL[,3]
DenC_OLL<-Catch_MONandHYR(DenLL)
DenC1_OLL<-DenC_OLL[,2]; DenC2_OLL<-DenC_OLL[,3]

# Finland
# ============
finland<-subset(salmon, COUNTRY=="FI" & FISHERY=="S")

# Driftnetting
FinDN<-subset(finland, GEAR=="GND")
FinDN%>%count(TP_TYPE) # HYR data only
filter(FinDN, is.na(EFFORT)==T) # noNA's in EFFORT!!!
filter(FinDN, is.na(NUMB)==T) # noNA's in NUMB

FinE_ODN<-Effort_HYR(FinDN)
FinE1_ODN<-FinE_ODN[,2]; FinE2_ODN<-FinE_ODN[,3]
FinC_ODN<-Catch_HYR(FinDN)
FinC1_ODN<-FinC_ODN[,2]; FinC2_ODN<-FinC_ODN[,3]
# Longlining
FinLL<-subset(finland, GEAR=="LLD")
FinLL%>%count(TP_TYPE) # HYR data only

FinE_OLL<-Effort_HYR(FinLL)
FinE1_OLL<-FinE_OLL[,2]; FinE2_OLL<-FinE_OLL[,3]
FinC_OLL<-Catch_HYR(FinLL)
FinC1_OLL<-FinC_OLL[,2]; FinC2_OLL<-FinC_OLL[,3]

# Sweden
# ============
sweden<-subset(salmon, COUNTRY=="SE" & FISHERY=="S")

# Driftnetting
SweDN<-subset(sweden, GEAR=="GND")
SweDN%>%count(TP_TYPE) # HYR data only

SweE_ODN<-Effort_HYR(SweDN)
SweE1_ODN<-SweE_ODN[,2]; SweE2_ODN<-SweE_ODN[,3]
SweC_ODN<-Catch_HYR(SweDN)
SweC1_ODN<-SweC_ODN[,2]; SweC2_ODN<-SweC_ODN[,3]
# Longlining
SweLL<-subset(sweden, GEAR=="LLD")
SweLL%>%count(TP_TYPE) # HYR data only

SweE_OLL<-Effort_HYR(SweLL)
SweE1_OLL<-SweE_OLL[,2]; SweE2_OLL<-SweE_OLL[,3]
SweC_OLL<-Catch_HYR(SweLL)
SweC1_OLL<-SweC_OLL[,2]; SweC2_OLL<-SweC_OLL[,3]

# ==============================================================================

# Calculate proportions of effort and catch for 1. HYR and 2. HYR
# from combined FIN, SWE, DEN and LAT data for different years and for ODN and OLL.
# Then Divide Polish catch and effort for each HYR based on those.

# Proportions of effort and catch each HYR in combined data

# For ODN
C1_ODN<-LatC1_ODN+DenC1_ODN+FinC1_ODN+SweC1_ODN
E1_ODN<-LatE1_ODN+DenE1_ODN+FinE1_ODN+SweE1_ODN
C2_ODN<-LatC2_ODN+DenC2_ODN+FinC2_ODN+SweC2_ODN
E2_ODN<-LatE2_ODN+DenE2_ODN+FinE2_ODN+SweE2_ODN

propC1_ODN<-C1_ODN/(C1_ODN+C2_ODN)
propE1_ODN<-E1_ODN/(E1_ODN+E2_ODN)

# For OLL
C1_OLL<-DenC1_OLL+FinC1_OLL+SweC1_OLL
E1_OLL<-DenE1_OLL+FinE1_OLL+SweE1_OLL
C2_OLL<-DenC2_OLL+FinC2_OLL+SweC2_OLL
E2_OLL<-DenE2_OLL+FinE2_OLL+SweE2_OLL

propC1_OLL<-C1_OLL/(C1_OLL+C2_OLL)
propE1_OLL<-E1_OLL/(E1_OLL+E2_OLL)


DenC1_OLL+FinC1_OLL+SweC1_OLL
DenE1_OLL+FinE1_OLL+SweE1_OLL


DenC2_OLL+FinC2_OLL+SweC2_OLL
DenE2_OLL+FinE2_OLL+SweE2_OLL

round(cbind(SweC1_OLL/SweE1_OLL,
FinC1_OLL/FinE1_OLL,
DenC1_OLL/DenE1_OLL),2)

round(cbind(SweC2_OLL/SweE2_OLL,
FinC2_OLL/FinE2_OLL,
DenC2_OLL/DenE2_OLL),2)


round(cbind(SweC2_OLL,SweE2_OLL,
FinC2_OLL,FinE2_OLL,
DenC2_OLL,DenE2_OLL),2)


# Calculate combined CPUE from FIN, SWE, DEN and LAT data for ODN and OLL (only those data
# lines taken into account where both information are known)
# With this we can calculate new Polish catch based on other countries CPUE and Polish effort

# For ODN
CPUE1_ODN<-C1_ODN/E1_ODN
CPUE2_ODN<-C2_ODN/E2_ODN

# For OLL
CPUE1_OLL<-C1_OLL/E1_OLL
CPUE2_OLL<-C2_OLL/E2_OLL


round(cbind(min_year:max_year,CPUE1_OLL,CPUE2_OLL),4)
round(cbind(min_year:max_year,CPUE1_ODN,CPUE2_ODN),4)



