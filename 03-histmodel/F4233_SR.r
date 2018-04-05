# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 produce figure F4.2.3.3, SR graphs

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

#source("compare-models/models-select.R")

## ---- load-SRcurves

# Time
######################
Year<-c(1996:2018) # Smolt years, add one each year
Nyears<-length(Year) #16
######################

# Points
######################

E<-array(NA, dim=c(Nyears,16,1000))
S<-array(NA, dim=c(Nyears,16,1000))
for(r in 1:16){   
  for(y in 10:(10+Nyears-1)){ 
    S[y-9,r,]<-chains[,str_c("SmoltWW[",y,",",r,"]")][[1]]  
  }
  for(y in 6:(6+Nyears-1)){ 
    E[y-5,r,]<-chains[,str_c("Eggstot_M74[",y,",",r,"]")][[1]] 
  }
}


E_med<-array(NA, dim=c(Nyears,16)); E_low<-array(NA, dim=c(Nyears,16))
E_high<-array(NA, dim=c(Nyears,16));S_med<-array(NA, dim=c(Nyears,16))
S_low<-array(NA, dim=c(Nyears,16)); S_high<-array(NA, dim=c(Nyears,16))

for(r in 1:16){
for(y in 1:Nyears){
    E_med[y,r]<-summary(as.mcmc(E[y,r,]))$quantiles[3]
    E_low[y,r]<-summary(as.mcmc(E[y,r,]))$quantiles[1]
    E_high[y,r]<-summary(as.mcmc(E[y,r,]))$quantiles[5]
    S_med[y,r]<-summary(as.mcmc(S[y,r,]))$quantiles[3]
    S_low[y,r]<-summary(as.mcmc(S[y,r,]))$quantiles[1]
    S_high[y,r]<-summary(as.mcmc(S[y,r,]))$quantiles[5]
  }
}

cbind(E_low[,1],E_med[,1],E_high[,1],S_low[,1],S_med[,1],S_high[,1])


# Curves
######################

a<-array(NA, dim=c(16,1000))
b<-array(NA, dim=c(16,1000))

# Load simulated values from files
for(i in 1:16){
  a[i,]<-chains[,str_c("alphaSR[",i,"]")][[1]]  
  b[i,]<-chains[,str_c("betaSR[",i,"]")][[1]]  
}

# Take a sample or use all 1000 simulated values
#samp<-round(runif(100,1,1000),0)
samp<-1:1000
a_samp<-array(NA, dim=c(16, length(samp)))
b_samp<-array(NA, dim=c(16, length(samp)))
for(r in 1:16){
  for(i in 1:length(samp)){
   a_samp[r,i]<-a[r,samp[i]]
   b_samp[r,i]<-b[r,samp[i]]
  }
}

# calculate number of smolts for each amount of eggs with B-H stock recruitment 
# function
# Note!! length(eggs)==length(eggs2) (or change the code)
eggs<-seq(0,1000, by=2)
eggs2<-seq(0,10, by=0.02)  # For small rivers
length(eggs)==length(eggs2) # These must be equal so that the following will work
smolts<-array(NA, dim=c(16,length(eggs),length(samp)))
smolts2<-array(NA, dim=c(16, length(eggs),length(samp)))
for(r in 1:16){
  for(i in 1:length(eggs)){
    for(j in 1:length(samp)){
    ifelse(r<5|r==7|r==10|r==14|r==16,
      smolts[r,i,j]<-eggs[i]*1000/(a_samp[r,j]+b_samp[r,j]*eggs[i]*1000),
      smolts2[r,i,j]<-eggs2[i]*1000/(a_samp[r,j]+b_samp[r,j]*eggs2[i]*1000)
      )
   }
  }
}

############          
## Graphs ##
############


## ---- graphs-SRcurves



############
#windows(12,12,record=TRUE,title="Stock-Recruitment")
#windows(12,12)
par(mfrow=c(2,2),mar=c(4.5,4,3.5,1))
#par(mfrow=c(1,2),mar=c(4.5,4,3.5,1))

col1<-rgb(0.5,0,0.5,0.03)
col2<-rgb(0,0.7,1,0.1)

col1<-rgb(1,0,0,0.03)
col2<-rgb(0,0,1,0.1)

cexLab<-1.2
cexAxis<-1.2
cexMain<-1.5

relx<-7 # relationship between x and y axis is 1:8

############
# Tornionjoki

#windows()

plot(eggs, smolts[1,,1], type="l",ylim=c(0,relx*800), xlim=c(0,800), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main=Rivername_long[1], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,1,]/1000,S[i,1,], col=col2)
  #points(e1[i,1,]/1000,s1[i,1,], col=col2)
}

for(j in 2:length(samp)){
  points(eggs, smolts[1,,j], type="l", col=col1)
}

#summary(as.mcmc(E[1,1,]))
#summary(as.mcmc(S[1,1,]))
#summary(as.mcmc(S[1,1,]/E[1,1,]))

#for(i in 1:Nyears){
#  points(E_med[i,1]/1000, S_med[i,1], pch=19)
#  text(E_med[i,1]/1000+20,S_med[i,1], Year[i])
#}

############
# Simojoki

plot(eggs, smolts[2,,1], type="l",ylim=c(0,150), xlim=c(0,50), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main=Rivername_long[2], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,2,]/1000,S[i,2,], col=col2)
}

for(j in 2:length(samp)){
  points(eggs, smolts[2,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,2]/1000, S_med[i,2], pch=19)
#  text(E_med[i,2]/1000+3,S_med[i,2], Year[i])
#}
############
# Kalix

plot(eggs, smolts[3,,1], type="l",ylim=c(0,relx*300), xlim=c(0,300), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main=Rivername_long[3], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,3,]/1000,S[i,3,], col=col2)
}

for(j in 2:length(samp)){
  points(eggs, smolts[3,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,3]/1000, S_med[i,3], pch=19)
#  text(E_med[i,3]/1000+12,S_med[i,3], Year[i])
#}

############
# R?ne

plot(eggs, smolts[4,,1], type="l",ylim=c(0,relx*35), xlim=c(0,35), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main=Rivername_long[4], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,4,]/1000,S[i,4,], col=col2)
}

for(j in 2:length(samp)){
  points(eggs, smolts[4,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,4]/1000, S_med[i,4], pch=19)
#  text(E_med[i,4]/1000+1,S_med[i,4], Year[i])
#}
################################################################################
#windows(12,12,record=TRUE,title="Stock-Recruitment")
par(mfrow=c(2,2),mar=c(4.5,4,3.5,1))

############
# Pite

plot(eggs2, smolts2[5,,1], type="l",ylim=c(0,relx*9), xlim=c(0,9), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main=Rivername_long[5], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,5,]/1000,S[i,5,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[5,,j], type="l", col=col1)
}

#for(i in 1:Nyears){
#  points(E_med[i,5]/1000, S_med[i,5], pch=19)
#  text(E_med[i,5]/1000+0.3,S_med[i,5], Year[i])
#}

############
# ?by

plot(eggs2, smolts2[6,,1], type="l",ylim=c(0,relx*10), xlim=c(0,10), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main=Rivername_long[6], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,6,]/1000,S[i,6,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[6,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,6]/1000, S_med[i,6], pch=19)
#  text(E_med[i,6]/1000+0.5,S_med[i,6], Year[i])
#}
############
# Byske

plot(eggs, smolts[7,,1], type="l",ylim=c(0,relx*90), xlim=c(0,90), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main=Rivername_long[7], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,7,]/1000,S[i,7,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs, smolts[7,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,7]/1000, S_med[i,7], pch=19)
#  text(E_med[i,7]/1000+2.5,S_med[i,7], Year[i])
#}

############
# Rickle?n

plot(eggs2, smolts2[8,,1], type="l",ylim=c(0,relx*2.5), xlim=c(0,2.5), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main=Rivername_long[8], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,8,]/1000,S[i,8,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[8,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,8]/1000, S_med[i,8], pch=19)
#  text(E_med[i,8]/1000+0.03,S_med[i,8], Year[i])
#}
####################################################
#windows(12,12,record=TRUE,title="Stock-Recruitment")
par(mfrow=c(2,2),mar=c(4.5,4,3.5,1))

############
# S?var?n

plot(eggs2, smolts2[9,,1], type="l",ylim=c(0,relx*3), xlim=c(0,3), col=col1, 
ylab="Smolts (thousands)", xlab="Eggs (millions)", main=Rivername_long[9], cex.lab=cexLab, 
cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,9,]/1000,S[i,9,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[9,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,9]/1000, S_med[i,9], pch=19)
#  text(E_med[i,9]/1000+0.05,S_med[i,9], Year[i])
#}


################################################################################
############
# Ume

plot(eggs, smolts[10,,1], type="l",ylim=c(0,relx*100), xlim=c(0,100), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main=Rivername_long[10], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,10,]/1000,S[i,10,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs, smolts[10,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,10]/1000, S_med[i,10], pch=19)
#  text(E_med[i,10]/1000+2,S_med[i,10], Year[i])
#}

############
# ?re

plot(eggs2, smolts2[11,,1], type="l",ylim=c(0,relx*5), xlim=c(0,5), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main=Rivername_long[11], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,11,]/1000,S[i,11,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[11,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,11]/1000, S_med[i,11], pch=19)
#  text(E_med[i,11]/1000+0.05,S_med[i,11], Year[i])
#}

############
# L?gde

plot(eggs2, smolts2[12,,1], type="l",ylim=c(0,relx*6), xlim=c(0,6), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main=Rivername_long[12], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,12,]/1000,S[i,12,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[12,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,12]/1000, S_med[i,12], pch=19)
#  text(E_med[i,12]/1000+0.2,S_med[i,12], Year[i])
#}
####################################################
#windows(12,12,record=TRUE,title="Stock-Recruitment")
par(mfrow=c(2,2),mar=c(4.5,4,3.5,1))

############
# K?ge?lven
plot(eggs2, smolts2[16,,1], type="l",ylim=c(0,relx*20), xlim=c(0,20), col=col1, 
ylab="Smolts (thousands)", xlab="Eggs (millions)", main=Rivername_long[16], 
cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,16,]/1000,S[i,16,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs, smolts[16,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,16]/1000, S_med[i,16], pch=19)
#  text(E_med[i,16]/1000+0.2,S_med[i,16], Year[i])
#}


############
# Ljungan

plot(eggs2, smolts2[13,,1], type="l",ylim=c(0,relx*2), xlim=c(0,2), 
col=col1, ylab="Smolts (thousands)",xlab="Eggs (millions)", main=Rivername_long[13], 
cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,13,]/1000,S[i,13,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[13,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,13]/1000, S_med[i,13], pch=19)
#  text(E_med[i,13]/1000+0.07,S_med[i,13], Year[i])
#}


############
# M?rrum

plot(eggs, smolts[14,,1], type="l",ylim=c(0,200), xlim=c(0,100), col=col1, 
ylab="Smolts (thousands)", xlab="Eggs (millions)", main=Rivername_long[14], 
cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,14,]/1000,S[i,14,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs, smolts[14,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,14]/1000, S_med[i,14], pch=19)
#  text(E_med[i,14]/1000+5,S_med[i,14], Year[i])
#}


############
# Em?n

plot(eggs2, smolts2[15,,1], type="l",ylim=c(0,20), xlim=c(0,10), 
col=col1, ylab="Smolts (thousands)",  xlab="Eggs (millions)", main=Rivername_long[15], 
cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,15,]/1000,S[i,15,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[15,,j], type="l", col=col1)
}
#for(i in 1:Nyears){
#  points(E_med[i,15]/1000, S_med[i,15], pch=19)
#  text(E_med[i,15]/1000+0.5,S_med[i,15], Year[i])
#}



## ---- graphs-SRcurves-tornereport


# To Torne report
###########################

windows(12,12)

col2rgb("brown", alpha = FALSE)

col1<-rgb(1,0,0,0.03) # Viivat
col2<-rgb(0,0,1,0.3)  # 2015, sininen
col3<-rgb(0,0,0,0.3)  # 2016, harmaa
col4<-rgb(0.17,0.66,0.17,0.05)  # muut vuodet, vihre?

cexLab<-1.2
cexAxis<-1.2
cexMain<-1.5

############
# Tornionjoki

#windows()

plot(eggs, smolts[1,,1], type="l",ylim=c(0,relx*800), xlim=c(0,800), col=col1, ylab="Smolts (thousands)",
xlab="Eggs (millions)", main="Tornionjoki", cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

# viivat
for(j in 2:length(samp)){
  points(eggs, smolts[1,,j], type="l", col=col1)
}

# pisteet
for(i in 1:(Nyears-1)){
  points(E[i,1,]/1000,S[i,1,], col=col4)
}

points(E[22,1,]/1000,S[22,1,], col=col2) # 2017
points(E[21,1,]/1000,S[21,1,], col=col3) # 2016
points(E[20,1,]/1000,S[20,1,], col=col2) # 2015

points(E_med[22,1]/1000, S_med[22,1], pch=19, cex=1.2, col="blue")
text(E_med[22,1]/1000+25,S_med[22,1], Year[22], cex=1.2)

points(E_med[21,1]/1000, S_med[21,1], pch=19, cex=1.2, col=1)
text(E_med[21,1]/1000+25,S_med[21,1], Year[21], cex=1.2)
  
points(E_med[20,1]/1000, S_med[20,1], pch=19, col=1, cex=1.2)
text(E_med[20,1]/1000+25,S_med[20,1], Year[20], cex=1.2, col=1)
  
abline(v=440)

