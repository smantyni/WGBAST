# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Jokikohtaisten smolttim??rien tunnuslukujen talletus
# excel-tiedostoon. Aikajana kattaa sek? historiallisen osan (alkaen tosin
# vuodesta 92 vuoden 87 sijaan) ett? eteenp?in ennustavan osan.
# -------------------------
# ~*~ (C): Henni (9/2008) ~*~
# -------------------------
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

rm(list=ls(all=TRUE))
library(coda)
library(xlsx)


#! #############################################################################
################################################################################ 
# Path for the output
path<-"H:/Biom/Scenarios/2017/prg/output/"

# Version of the estimation model
Model<-"New_SR"

# Time
LastHistYear<-2017
LastPredYear<-2032
Year<-c(1992:LastPredYear)
length(Year)

# ?????????????????????????????????????????????????????????????????
# Scenarios
#! Mps
choice<-"MED"   # corresponds to Mps during 2008-2011 period

#! Effort 
EffScen<-1

PathScen<-"H:/FLR/WGBAST18/Scenarios/" # scenario results 

#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen",EffScen,".RData")

File
load(File)


#! #############################################################################
################################################################################

Years<-c(1992:LastPredYear)
length(Years)

yBreak<-length(Years)
sims<-c(1:1000)
numStocks<-16; numUnits<-4 

# ls()

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Lasketaan tunnusluvut eri joille, talletetaan excel-tiedostoon
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

River<-c("Torne","Simo","Kalix","Rane","Pite","Aby","Byske","Ricklean",
"Savaran","Ume/Vindel","Ore","Lodge","Ljungan","Morrumsan","Eman")

dim(SmoltW)

for(r in 1:numStocks){
	Mean<-NULL; Sd<-NULL; Q5<-NULL; Median<-NULL; Q95<-NULL; PI<-NULL

	for(y in 1:length(Years)){
    
    sum_temp<-summary(as.mcmc(SmoltW[r,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
		
    Mean[y]<-sum_temp$statistics[1]
    Sd[y]<-sum_temp$statistics[2]
    Q5[y]<-sum_temp$quantiles[1]
		Median[y]<-sum_temp$quantiles[3]
		Q95[y]<-sum_temp$quantiles[5]

# This makes more decimals for the probability intervals of small rivers
		ifelse(r==8||r==9||r==13, 
		PI[y]<-paste(sep="", "'",round(Q5[y],1),"-",round(Q95[y],1)),
		PI[y]<-paste(sep="", "'",round(Q5[y],0),"-",round(Q95[y],0)))
	}

	t1<-rbind(rep(River[r],length(Years)),Years,Mean,Sd,Median,Q5,Q95)#,PI)

	t2<-rbind(rep(River[r],length(Years)),Years, PI)

	ifelse(r==1, table1<-t1, table1<-rbind(table1, t1))
	#ifelse(r==1, table2<-t2, table2<-rbind(table2, t2))
}

write.xlsx(table1, file = paste0("results/SmoltsByRiver_",Model,"Mps",choice,"_EScen",EffScen,".csv"))


SmoltW_AU<-array(NA, dim=c(4,length(Years),1000))

for(y in 1:length(Years)){
  for(s in 1:1000){
    SmoltW_AU[1,y,s]<-sum(SmoltW[1:4,y,s])
    SmoltW_AU[2,y,s]<-sum(SmoltW[5:12,y,s])+SmoltW[16,y,s]
    SmoltW_AU[3,y,s]<-SmoltW[13,y,s]
    SmoltW_AU[4,y,s]<-sum(SmoltW[14:15,y,s])
  }  
}
    
for(u in 1:4){
	Mean<-NULL; Sd<-NULL; Q5<-NULL; Median<-NULL; Q95<-NULL; PI<-NULL

	for(y in 1:length(Years)){
		
		sum_temp<-summary(as.mcmc(SmoltW_AU[u,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
		
		Mean[y]<-sum_temp$statistics[1]
    Sd[y]<-sum_temp$statistics[2]    
		Q5[y]<-sum_temp$quantiles[1]
		Median[y]<-sum_temp$quantiles[3]
		Q95[y]<-sum_temp$quantiles[5]

# This makes more decimals for the probability intervals of small rivers
		ifelse(r==8||r==9||r==13, 
		PI[y]<-paste(sep="", "'",round(Q5[y],1),"-",round(Q95[y],1)),
		PI[y]<-paste(sep="", "'",round(Q5[y],0),"-",round(Q95[y],0)))
	}

	t<-rbind(rep(paste(sep="", "AU",u),length(Years)),Years,Mean,Sd,Median,Q5,Q95)#,PI)

	t2<-rbind(rep(paste(sep="", "AU",u),length(Years)),Years, PI)

	ifelse(u==1, table<-t, table<-rbind(table, t))
	#ifelse(u==1, table2<-t2, table2<-rbind(table2, t2))
}

write.xlsx(table, file = paste0("results/SmoltsByUnit_",Model,"Mps",choice,"_EScen",EffScen,".csv"))


