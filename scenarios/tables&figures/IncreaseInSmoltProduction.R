# Draw density functions for river specific smolt production within a generation
# under different scenarios and compare with production in last year
# How is smolt production expected to change under different fishing pressures?

rm(list=ls(all=TRUE))

################################################################################
#! #############################################################################
# Version of the estimation model
PathScen<-"H:/FLR/WGBAST18/Scenarios/" # scenario results 

Model<-"New_SR"

# Time
LastHistYear<-2017
LastPredYear<-2032
year<-c(1992:LastPredYear)
length(year)
Nyears<-length(year)
cbind(c(1992:2032),c(1:41))
compyear<-26 # 2017
refyear<-33 # 2024
CompYear<-2017

# ?????????????????????????????????????????????????????????????????
# Scenarios
#! Mps
choice<-"MED"   
# ?????????????????????????????????????????????????????????????????
SmoltList<-list()

#EffScen<-1

for(EffScen in 1:4){
  File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen",EffScen,".RData")
  load(File)
  SmoltList[[EffScen]]<-SmoltW
}
File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen6.RData")
load(File)
SmoltList[[5]]<-SmoltW

File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen5.RData")
load(File)
SmoltList[[6]]<-SmoltW

river<-c("Tornio", "Simo", "Kalix", "Råne", "Pite", "Åby", "Byske", "Rickleån",
"Sävarån", "Ume/Vindel", "Öre", "Lögde", "Ljungan", "Mörrumsån", "Emån", "Kåge")

MinSmolts<-c(
800,10,100,0,
10,0,30,0,
0,80,0,0,
0,0,0,0)
MaxSmolts<-c(
3200,100,1500,150,
80,40,350,15,
20,600,80,40,
5,120,20,100)

COL<-c(1,1,2,3,4,1,6)
LTY<-c(1,1,1,1,1,2,1)
LWD<-c(2,1,1,1,1,1,1)

windows(record=T)
par(mfrow=c(4,2),mar=c(4,2.5,4,1))
for(r in 1:16){
 # r<-1
  if(r==14 | r==15){ryear<-refyear-1}else{ryear<-refyear}
  
  plot(density(SmoltList[[1]][r,compyear,]), lwd=2, main=river[r], xlab="Smolt production",
       xlim=c(MinSmolts[r],MaxSmolts[r]))
  abline(v=median(SmoltW1[r,compyear,]), lwd=2)
  
  for(scen in 1:6){
    #scen<-2
    points(density(SmoltList[[scen]][r,ryear,]), lwd=LWD[scen+1], type="l", col=COL[scen+1], 
           lty=LTY[scen+1])
    abline(v=median(SmoltList[[scen]][r,ryear,]), col=COL[scen+1], lty=LTY[scen+1])
  } 
  legend("topright", col=COL, lwd=LWD, lty=LTY,
         legend=c(CompYear,"Scen1","Scen2","Scen3","Scen4","Scen5","Scen6"))
  
}  
  




