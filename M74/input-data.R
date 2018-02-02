library(rjags)
library(runjags)
library(tidyverse)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)
library(gridExtra)
library(coda)

pathM74<-"H:/Biom/SubE_M74/2018/"


ColNames<-c("smolts", "empty", "n_schools", "school_size")
Day<-c(c(1:30), c(1:31), c(1:31))
Month<-c(rep(6,30), rep(7,31), rep(8,31))

df<-read_xlsx(str_c(pathM74,"data/orig/Finnish_M74_data-2017_paivitetty.xlsx"), 
              col_names = T, guess_max = 10000, sheet=1, na="")

df<-df%>%   mutate(River=fct_recode(RIVER,
                                    "1"="Simo",
                                    "2"="Tornio",
                                    "3"="Kemi",
                                    "4"="Iijoki"))%>%
  mutate(year=FEMALE_YEAR-1984)
  
  df<-df%>%mutate(Eggs=ifelse(is.na(eggs)==F,eggs,ifelse(year<10,100,115)))
filter(df, is.na(Eggs)==T)


View(df)

read_xl
             
D05<-read_xls(str_c(sep="",pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2005.xls"),
              sheet=1, na="", 
              range="Z10:AC70", col_names=ColNames)
D05[23,]<-rep(NA,4) # 23.6. 00-09 missing
tmp<-array(0, dim=c(31,4));colnames(tmp)<-colnames(D05)
D05<-rbind(D05, tmp) #1.8.-31.8. missing but replace with zeros
D05<-D05 %>% mutate(day=c(1:92))%>%
  mutate(Year=2005)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)
#View(D05)