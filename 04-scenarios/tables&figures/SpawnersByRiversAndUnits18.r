# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Laskee tunnusluvut kutijam??rille vuosittain ja uniteittain,
# erikseen villeille, viljellyille ja molemmille yhteens?, ja tallettaa
# tiedot tiedostoon Spawners_WildAndReared_ByUnits.csv
# -------------------------
# ~*~ (C): Henni (2008/2011) ~*~
# -------------------------
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

rm(list=ls(all=TRUE))
library(coda)

PathScen<-"H:/FLR/WGBAST18/Scenarios/" # scenario results 
PathOut<-"H:/Biom/Scenarios/2018/prg/" # output


################################################################################
#! #############################################################################
# Version of the estimation model
#Model<-"Orig"
Model<-"New_SR"

# Time
LastHistYear<-2017
LastPredYear<-2032
Years<-c(1992:LastPredYear)
length(Years)

# ?????????????????????????????????????????????????????????????????
# Scenarios
#! Mps
choice<-"MED"  

#! Effort 
EffScen<-1 
# ?????????????????????????????????????????????????????????????????


#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen",EffScen,".RData")

File
load(File)
#! #############################################################################
################################################################################


# Function stats calculates a set of statistics from a vector 
stats<-function(dat){

		summary_dat<-summary(as.mcmc(dat))
		M<-mean(dat)
   	S<-sd(dat)
  	q2.5<-summary_dat$quantiles[1]
		q25<-summary_dat$quantiles[2]
		q50<-summary_dat$quantiles[3]
		q75<-summary_dat$quantiles[4]
		q97.5<-summary_dat$quantiles[5]
	
return(data.frame(M,S,q2.5,q50,q97.5))
}

# Years: 92=1, 2007=16
NspW<-array(NA, dim=c(4,length(Years),1000))
NspR<-array(NA, dim=c(4,length(Years),1000))
for(y in 1:length(Years)){
  for(s in 1:1000){
    NspW[1,y,s]<-sum(SpawnerW[1:4,y,s])
    NspW[2,y,s]<-sum(SpawnerW[5:12,y,s])
    NspW[3,y,s]<-SpawnerW[13,y,s]
    NspW[4,y,s]<-sum(SpawnerW[14:15,y,s])
    
    NspR[1,y,s]<-SpawnerR[1,y,s]
    NspR[2,y,s]<-SpawnerR[2,y,s]
    NspR[3,y,s]<-SpawnerR[3,y,s]
    NspR[4,y,s]<-SpawnerR[4,y,s]
  }
}

#*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
#   Calculate statistics separately for wild and reared by units and 
#   as totals per unit.
#*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

#*~*~*~*~*~*~*~**~*~*~*
#   ASSESSMENT UNITS  
#*~*~*~*~*~*~*~**~*~*~*
for(u in 1:4){
	if(u==1){data1<-as.mcmc(NspW[1,,]); data2<-as.mcmc(NspR[1,,])}
	if(u==2){data1<-as.mcmc(NspW[2,,]);	data2<-as.mcmc(NspR[2,,])}
	if(u==3){data1<-as.mcmc(NspW[3,,]);	data2<-as.mcmc(NspR[3,,])}
	if(u==4){data1<-as.mcmc(NspW[4,,]);	data2<-as.mcmc(NspR[4,,])}
	
	for(t in 1:2){
		Mean<-NULL; Q95<-NULL; Q5<-NULL; Sd<-NULL; Median<-NULL;
		if(t==1){dat<-data1}
		if(t==2){dat<-data2}

		for(y in 1:length(Years)){
	
			result<-stats(dat[y,])
	    Q5[y]<-summary(as.mcmc(dat[y,]))$quantiles[1]
	    Q95[y]<-summary(as.mcmc(dat[y,]))$quantiles[5]
	    Median[y]<-summary(as.mcmc(dat[y,]))$quantiles[3]
      Mean[y]<-summary(as.mcmc(dat[y,]))$statistics[1]
      Sd[y]<-summary(as.mcmc(dat[y,]))$statistics[2]
	
		}
if(u==1&&t==1){t1w<-rbind(rep("AU1/Wild", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==2&&t==1){t2w<-rbind(rep("AU2/Wild", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==3&&t==1){t3w<-rbind(rep("AU3/Wild", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==4&&t==1){t4w<-rbind(rep("AU4/Wild", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==1&&t==2){t1r<-rbind(rep("AU1/Reared", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==2&&t==2){t2r<-rbind(rep("AU2/Reared", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==3&&t==2){t3r<-rbind(rep("AU3/Reared", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==4&&t==2){t4r<-rbind(rep("AU4/Reared", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
	}

# Totals per unit
	Mean<-NULL; Q95<-NULL; 
	Q5<-NULL; Sd<-NULL; Median<-NULL;

	dataTot<-data1+data2

		for(y in 1:length(Years)){
	
			result<-stats(dataTot[y,])
	    Q5[y]<-summary(as.mcmc(dat[y,]))$quantiles[1]
	    Q95[y]<-summary(as.mcmc(dat[y,]))$quantiles[5]
	    Median[y]<-summary(as.mcmc(dat[y,]))$quantiles[3]
      Mean[y]<-summary(as.mcmc(dat[y,]))$statistics[1]
      Sd[y]<-summary(as.mcmc(dat[y,]))$statistics[2]
	 }

if(u==1){t1tot<-rbind(rep("AU1_Tot", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==2){t2tot<-rbind(rep("AU2_Tot", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==3){t3tot<-rbind(rep("AU3_Tot", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==4){t4tot<-rbind(rep("AU4_Tot", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}

}

#*~*~*~*~*~*~*~**~*~*~*
#   RIVERS  
#*~*~*~*~*~*~*~**~*~*~*


for(r in 1:15){
  Mean<-NULL;  Q95<-NULL;  Q5<-NULL; 
  PI<-NULL; Sd<-NULL; Median<-NULL;

  dat<-as.mcmc(SpawnerW[r,,])  
  
  for(y in 1:length(Years)){
    
			result<-stats(dat[y,])
	    Q5[y]<-summary(as.mcmc(dat[y,]))$quantiles[1]
	    Q95[y]<-summary(as.mcmc(dat[y,]))$quantiles[5]
	    Median[y]<-summary(as.mcmc(dat[y,]))$quantiles[3]
      Mean[y]<-summary(as.mcmc(dat[y,]))$statistics[1]
      Sd[y]<-summary(as.mcmc(dat[y,]))$statistics[2]
      PI[y]<-paste(sep="", "'",round(Q5[y],0),"-",round(Q95[y],0))

  }

if(r==1){t_tornio<-rbind(rep("Tornio", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==2){t_simo<-rbind(rep("Simo", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==3){t_kalix<-rbind(rep("Kalix", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==4){t_rane<-rbind(rep("Rane", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==5){t_pite<-rbind(rep("Pite", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==6){t_aby<-rbind(rep("Aby", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==7){t_byske<-rbind(rep("Byske", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==8){t_rick<-rbind(rep("Ricklean", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==9){t_savaran<-rbind(rep("Savaran", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==10){t_vindel<-rbind(rep("Ume/Vindel", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==11){t_orea<-rbind(rep("Ore", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==12){t_lodge<-rbind(rep("Lodge", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==13){t_ljungan<-rbind(rep("Ljungan", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==14){t_morrumsan<-rbind(rep("Morrum", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==15){t_eman<-rbind(rep("Eman", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
}

#########################
t_units<-rbind(t1w,t2w,t3w,t4w,t1r,t2r,t3r,t4r,t1tot,t2tot,t3tot,t4tot)
t_rivers<-rbind(t_tornio, t_simo,t_kalix,t_rane,t_pite,t_aby,t_byske,t_rick,
t_savaran,t_vindel,t_orea,t_lodge,t_ljungan,t_morrumsan,t_eman)

#path<-"H:/Biom/Scenarios/2015/prg/output/"

write.xlsx(t_units,paste0(PathOut,"SpawnersByUnit_",Model,"Mps",choice,"_EScen",EffScen,".xlsx"))
write.xlsx(t_rivers,paste0(PathOut,"SpawnersByRiver_",Model,"Mps",choice,"_EScen",EffScen,".xlsx"))


