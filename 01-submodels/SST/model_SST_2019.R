setwd("c:/models/wgbast_dp/files_hp/fulllifehistorymodel/2019/dat/orig/")
tempdata<-read.csv("SMHI_tempdata_8_stations_jan1980_jan2019.csv",sep=";")
knoll<-read.csv("knolls_grund_2019_02_14.csv",sep=";")



tempdata<-tempdata[tempdata$Depth<=10,]
summary(tempdata)
temp<-aggregate(tempdata$Temperature,by=list(site=tempdata$Station, month=tempdata$Month,year=tempdata$Year),mean)
aggregate(tempdata$Temperature,by=list(month=tempdata$Month,year=tempdata$Year),length)
summary(knoll)
knoll$year<-format(as.Date(knoll$Date.and.time,format="%d.%m.%Y"),"%Y")
knoll$month<-as.numeric(format(as.Date(knoll$Date.and.time,format="%d.%m.%Y"),"%m"))
knoll<-aggregate(knoll$Temperature,by=list(month=knoll$month,year=knoll$year),mean)
knoll$site<-"Knollgrund"
knoll<-knoll[c(4,1,2,3)]
temp<-rbind(temp,knoll)
#combine
temp$site[as.character(temp$site)=="HANÖBUKTEN"]<-"Hanöbukten"
temp$site=factor(temp$site)
l<-levels(temp$site)
temp$siteID<-c()

for(i in 1:length(l)){
  for(j in 1:length(temp$site)){
    if(as.character(temp$site[j])==as.character(l[i])) temp$siteID[j] <-i
  }
}

temp$yearID<-as.numeric(temp$year)-1991
temp<-temp[temp$yearID>0,]
temp$x<-ifelse(is.nan(temp$x),NA,temp$x)
temp<-temp[temp$month<5,]


model<-"
model{

for(i in 1:3){ #loop over months
for(y in 1:Nyears){
mean_temp[i+1,y]~dnorm(alpha[i]+beta[i]*mean_temp[i,y],sd_process^(-2))  # regression of the mean temp
}
alpha[i]~dnorm(0,0.1)
beta[i]~dnorm(0,0.1)
}

for(y in 1:Nyears){
mean_temp[1,y]~dnorm(mu1,sd1^(-2))
}

for(j in 1:Nstations){
station_effect[j]~dnorm(0,sd_between_stations^(-2))
}

for(k in 1:length(x)){ #loop over observations at each station

x[k]~dnorm(mean_temp[month[k],year[k]]+station_effect[station[k]],sd_within_station^(-2))

}

sd1~dunif(0.01,5)
sd_process~dunif(0.01,5)
sd_between_stations~dunif(0.01,5)
sd_within_station~dunif(0.01,5)


mu1~dnorm(0,0.1)

}

"

data<-list(x=temp$x,month=temp$month,year=temp$yearID,station=temp$siteID,Nyears=30,Nstations=9)
load.module("glm")
jm<-jags.model(textConnection(model),data,n.chains=4)
chains<-coda.samples(jm,c(
  "mean_temp",
  "station_effect",
  "sd1",
  "sd_process",
  "sd_between_stations",
  "sd_within_station",
  "mu1",
  "alpha",
  "beta"),n.iter=10000,thin=10)
windows(record=TRUE)
plot(chains)

d<-as.matrix(chains)
linesplot<-function(d,name,range1,range2,n,...){
  names<-c()
  for(i in range1[1]:range1[2]){
    for(j in range2[1]:range2[2]){
      names<-c(names,paste(name,"[",i,",",j,"]",sep=""))
    }
  }
  z<-d[,names]
  
for(i in 1:n){
if(i==1) plot(z[i,],type="l",ylim=c(min(z),max(z)),ylab=name,...)
else points(z[i,],type="l")
}
}
par(mfrow=c(2,2))
linesplot(d,"mean_temp",c(1,4),c(1,1),100,xlab="month")
linesplot(d,"mean_temp",c(1,4),c(28,28),100,xlab="month")
linesplot(d,"mean_temp",c(1,4),c(29,29),100,xlab="month")
linesplot(d,"mean_temp",c(1,4),c(30,30),100,xlab="month")


