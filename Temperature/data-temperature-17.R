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


# Data to be inputted to BUGS
df.bugs<-dat %>%
    group_by(year, Month) %>%
    summarise(sst=mean(Temperature))

# This makes an empty set of data for the assessment year +1 for scenarios 
extra.year<-tibble(
  year=rep(max(select(df.jags,year))+1,9*4),
  Month= rep(1:4, 9),
  sst= parse_double(rep(NA, 4*9)))
extra.year

df.bugs<-full_join(df.bugs,extra.year, by=NULL)
#View(df.bugs)

write_csv(df.bugs, path="data.bugs.csv")
# copy paste data from excel to OpenBUGS using paste special -> unicode text
# change column names as year[]	month[]	SST[]
# Add text END at the bottom of the data file and press ENTER (empty line is needed at the end of the file)






