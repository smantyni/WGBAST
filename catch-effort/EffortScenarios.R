## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:   Combine effort data for scenarios

# c:		2015 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


#Run first CatchEffort.r
source("H:/Biom/FullLifeHistoryModel/2017/prg/catch&effort/CatchEffort17.r")


# =======================================================================
# Longlining:
# =====================
dim(DenE_OLLx)
Ny<-dim(DenE_OLLx)[1]

# Model years: 2002 autumn + 2003 spring =2002
E_DK<-c()
E_PL<-c()
for(y in 1:(Ny-3)){
  E_DK[y]<-DenE_OLLx[y+2,3]+DenE_OLLx[y+3,2] 
  E_PL[y]<-PolE_OLLxx[y+2,3]+PolE_OLLxx[y+3,2] 
}
# For assessment year-1 take same year autumn and spring effort
E_DK[Ny-2]<-DenE_OLLx[Ny,3]+DenE_OLLx[Ny,2]
E_PL[Ny-2]<-PolE_OLLxx[Ny,3]+PolE_OLLxx[Ny,2]

# This goes to EffortOLL_ICES.txt
############################
cbind(2002:(2002+Ny-3),E_DK/100000,E_PL/100000)
############################

# =======================================================================
# Trapnetting & Gillnetting:
# =====================
FinE_COT30x
dim(FinE_COT30x)
Ny<-dim(FinE_COT30x)[1]
# Model years: 2002 spring +2002 autumn =2002
ETN30_FI<-c();ETN31_FI<-c()
ETN30_SE<-c();ETN31_SE<-c()
EGN30_FI<-c();EGN31_FI<-c()
EGN30_SE<-c();EGN31_SE<-c()
for(y in 1:(Ny-2)){
  ETN30_FI[y]<-FinE_CTN30x[y+2,2]+FinE_CTN30x[y+2,3] 
  ETN30_SE[y]<-SweE_CTN30x[y+2,2]+SweE_CTN30x[y+2,3] 
  ETN31_FI[y]<-FinE_CTN31x[y+2,2]+FinE_CTN31x[y+2,3] 
  ETN31_SE[y]<-SweE_CTN31x[y+2,2]+SweE_CTN31x[y+2,3] 

  EGN30_FI[y]<-FinE_COT30x[y+2,2]+FinE_COT30x[y+2,3] 
  EGN30_SE[y]<-SweE_COT30x[y+2,2]+SweE_COT30x[y+2,3] 
  EGN31_FI[y]<-FinE_COT31x[y+2,2]+FinE_COT31x[y+2,3] 
  EGN31_SE[y]<-SweE_COT31x[y+2,2]+SweE_COT31x[y+2,3] 

}
# This goes to EffortCTN_ICES.txt
############################
cbind(2002:(2002+Ny-3),ETN30_FI/1e+3,ETN30_SE/1e+3,ETN31_FI/1e+3,ETN31_SE/1e+3)
############################
# This goes to EffortCGN_ICES.txt
############################
cbind(2002:(2002+Ny-3),EGN30_FI/1e+5,EGN30_SE/1e+5,EGN31_FI/1e+5,EGN31_SE/1e+5)
############################

# =======================================================================
# Note! Kate (or someone else) will ask OLL efforts from all countries
# for a certain graph in the report. Print the following (half year
# specific numbers) for her. 2014 Finnish longline effort is to be ignored
# (Tapsa will give details on this if needed). 
PolE_OLLxx
FinE_OLLx
DenE_OLLx
SweE_OLLx
# =======================================================================


