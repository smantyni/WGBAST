#######################
## Prepare SST data on 9 stations for JAGS model
##
#######################

#install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(lubridate)

# Path for input data
pathIn<-"H:/Biom/FullLifeHistoryModel/2017/data/orig/temperature/August/"

# Path for output
#pathOut<-

#############
# Data from 8 stations

(dat1<-read_xlsx(paste(sep="",pathIn,"SMHI tempdata 8 stations jan1980-apr2017.xlsx"),
                sheet="Data", na="NaN"))%>%
  select(Station, Year, Month,Day,Depth,Temperature)

station<-c()
for(i in 1:(dim(dat1)[1])){
  if(dat1$Station[i]=="BY1"){station[i]<-1}
  if(dat1$Station[i]=="BY2"){station[i]<-2}
  if(dat1$Station[i]=="BY38"){station[i]<-3}
  if(dat1$Station[i]=="BY4"){station[i]<-4}
  if(dat1$Station[i]=="BY5"){station[i]<-5}
  if(dat1$Station[i]=="BY10"){station[i]<-6}
  if(dat1$Station[i]=="BCS III-10"){station[i]<-7}
  if(dat1$Station[i]=="Hanöbukten"){station[i]<-8}
  if(dat1$Station[i]=="HANÖBUKTEN"){station[i]<-8}
}
dat1$station<-station

dat1<-filter(dat1, Month<5 & Depth<=10 & Year>1991 & is.na(Temperature)==F)%>%
  dplyr::mutate(year=Year-1991)%>%
  select(Temperature, Year, Month, Day, year, station)
dat1

#############
# Knolls Grund -data (station nr. 9)

dat2<-read_xlsx(paste(sep="",pathIn,"Knolls grund 2017-08-01.xlsx"),
               sheet=1, skip=5)

# Never mind the warnings about column 'Station' etc.just some bug in the package
dat2<-mutate(dat2,Year=year(as.POSIXct(dat2$Date)))%>%
  mutate(Month=month(as.POSIXct(dat2$Date)))%>%
  mutate(Day=day(as.POSIXct(dat2$Date)))%>%
  filter(Month<5)%>%
  mutate(year=Year-1991)%>%
  mutate(station=9)%>%
  setNames(c("Date", "Time", "Temperature", "Quality", "Depth", "Year", "Month", "Day", "year", "station"))%>%
  select(Temperature, Year, Month, Day, year, station)


###################
# Join datasets, full set covers 9 stations

(dat<-full_join(dat1,dat2, by=NULL))
#View(dat)


#############

# data for plotting SST data
(tmp<-dat %>% 
    group_by(Year, Month) %>%
    summarise(sst=mean(Temperature)))%>%
  mutate(month=parse_factor(Month, levels=c(1:4)))
View(tmp)

ggplot(data = tmp) + 
  geom_line(mapping = aes(x = Year, y = sst, color=parse_factor(Month, levels=c(1:4))), size=1)+
  labs(x="Year", y="Sea surface temperature", color="Month")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))


# Data to be inputted to JAGS
(df.jags<-dat %>%
    group_by(station, year, Month) %>%
    summarise(sst=mean(Temperature)))

extra.year<-tibble(
  station =rep(c(1:9),4),
  year=rep(max(select(df.jags,year))+1,9*4),
  Month= rep(1:4, 9),
  sst= parse_double(rep(NA, 4*9)))
extra.year

df.jags<-full_join(df.jags,extra.year, by=NULL)
View(df.jags)











# The rest has not been tidied
meanTemp<-apply(TempST,c(2,3), mean, na.rm=T) 
sdTemp<-apply(TempST,c(2,3), sd, na.rm=T) 

cbind(1992:(1992+maxY-1),meanTemp)

# Lines:
ltyL=rep(1,4)#c(1,2,3,1)
colL=c(1,2,4,3)


res <- 6
name_figure <- "figure.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)

# in colour:
end<-26
par(mfrow=c(1,1))
plot(1992:(1992+end), meanTemp[,1], col=1, ylim=c(0,7), type="l",
xlab="Year", ylab="Temperature (C)" , lwd=2, cex.lab=1.2, cex.axis=1.1)
segments(1992:(1992+end), meanTemp[,1]+sdTemp[,1],
1992:(1992+end), meanTemp[,1]-sdTemp[,1], lwd=2)

for(m in 2:4){
  points(1992:(1992+end)+0.05*m, meanTemp[,m], col=colL[m],
  type="l", lwd=2, lty=ltyL[m])
  segments(1992:(1992+end)+0.05*m, meanTemp[,m]+sdTemp[,m],
        1992:(1992+end)+0.05*m, meanTemp[,m]-sdTemp[,m], col=colL[m], lwd=2)
}
legend("topleft", c("Jan","Feb","March","April"), col=colL,
lty=ltyL, lwd=2)

dev.off()

# Lines:
lwdL=c(1,1,2,1)
ltyL=c(1,2,1,3)

# in grey:
end<-24
par(mfrow=c(1,1))
plot(1992:(1992+end), meanTemp[,1], col=1, ylim=c(0,7), type="l",
xlab="Year", ylab="Temperature (C)" , lwd=1)
segments(1992:(1992+end), meanTemp[,1]+sdTemp[,1],
1992:(1992+end), meanTemp[,1]-sdTemp[,1], lwd=2)

for(m in 2:4){
  points(1992:(1992+end)+0.05*m, meanTemp[,m], col=1,
  type="l", lwd=lwdL[m], lty=ltyL[m])#m)
  segments(1992:(1992+end)+0.05*m, meanTemp[,m]+sdTemp[,m],
        1992:(1992+end)+0.05*m, meanTemp[,m]-sdTemp[,m], col=1, lwd=2)
}
legend("topleft", c("Jan","Feb","March","April"), col=1,#col=c(1:4),
lty=ltyL)#c(1:4), lwd=c(1,2,2,2))




# Expected April temperatures (median & 95% PI) from the model
april<-read.table("prg/temperature/ModelExpectedAprilTemp_Jan17.txt", header=T)
april

end<-26

res <- 6
name_figure <- "figure2.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)


#windows()
cbind(1992:(1992+end), april$med[1:(end+1)], april$low[1:(end+1)], april$high[1:(end+1)])

plot(1992:(1992+end), april$med[1:(end+1)], type="l", lwd=2, col="red", ylim=c(1,7),
xlab="Year", ylab="Temperature (C)", main="SST in April", cex.lab=1.2, cex.axis=1.1)
points(1992:(1992+end), april$low[1:(end+1)], type="l", lwd=1, col="red")
points(1992:(1992+end), april$high[1:(end+1)], type="l", lwd=1, col="red")
m<-4
cbind(1992:(1992+(end-1)), meanTemp[1:end,m])
  points(1992:(1992+(end-1)), meanTemp[1:end,m], col=m,type="l", lty=1, lwd=2)
  segments(1992:(1992+(end-1)), meanTemp[,m]+sdTemp[,m],
        1992:(1992+(end-1)), meanTemp[,m]-sdTemp[,m], col=m, lwd=2)
legend("topleft", c("Model expected","Observed"), 
col=c("red", "blue"), lty=c(1,1), lwd=c(2,2))

dev.off()

for(m in 1:3){
#m<-1
#cbind(1992:(1992+end), meanTemp[1:end,m])
  points(1992:(1992+end), meanTemp[1:end,m], col=m,type="l", lty=1, lwd=2)
  segments(1992:(1992+end), meanTemp[,m]+sdTemp[,m],
        1992:(1992+end), meanTemp[,m]-sdTemp[,m], col=m, lwd=2)
}
