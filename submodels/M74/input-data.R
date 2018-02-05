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


# FI data
df<-read_xlsx(str_c(pathM74,"data/orig/Finnish_M74_data-2017_paivitetty.xlsx"), 
              col_names = T, guess_max = 10000, sheet=1, na="")%>%   
  mutate(river=fct_recode(RIVER,
                                    "1"="Simo",
                                    "2"="Tornio",
                                    "3"="Kemi",
                                    "4"="Iijoki"))%>%
  mutate(YEAR=FEMALE_YEAR)%>%
  mutate(year=FEMALE_YEAR-1984)%>%
  mutate(Eggs=ifelse(is.na(eggs)==F,eggs,ifelse(year<10,100,115)))%>%
  mutate(eggs=Eggs)%>%
  mutate(surv_eggs=round(eggs*(1-(YSFM/100)),0) )%>%
  mutate(M74_mort=fct_recode(M74,
                             "1"="Ei",
                             "2"="M74"))%>%
  # mort100 is 2 if 100% mortality, 1 if <100% and NA if M74 unknown (XX should not appear anywhere)
  mutate(mortality100=ifelse(is.na(M74)==F, ifelse(is.na(M74_100)==F, ifelse(M74_100==100,2,"XX"),1),NA ))
  
# check that these match                      
#View(select(df, M74,`M74 mortality`,  M74_mort,M74_100,`100% mortality`, mortality100))
#View(select(df, `surviving eggs`, surv_eggs))
            

dfFI<-df%>% 
  select(YEAR, eggs, year, river, surv_eggs, M74_mort, mortality100, YSFM)%>%
  mutate(M74=M74_mort)%>%
  select(YEAR, eggs, year, river, surv_eggs, M74, mortality100, YSFM)%>%
  filter(river!=4) # remove iijoki for now. Avoid confusion with Luleälven.
  
#View(filter(df, river==4))


# SE data
df1<-read_tsv(str_c(pathM74,"data/der/datR_M74_SE16.txt"))

df2<-read_xlsx(str_c(pathM74,"data/der/Swedish_M74_data_17.xlsx"))%>%
  mutate(ss=fct_recode(river,
                       "11"="Dal", "10"="Ljusnan", "8"="Indals", "7"="Angerman",
                       "6"="Ume", "5"="Skellefte", "4"="Lule"
                       ))%>%
  mutate(yy=32)%>%
  mutate(ff=Kramade)%>%
  mutate(xx=`Antal M74`)%>%
  select(ss, yy, ff, xx)

  


df2
View(df1)

dfSE


