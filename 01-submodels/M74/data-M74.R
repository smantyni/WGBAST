
# Modify input data for M74 model (OpenBUGS)

# Stocks:
#Simojoki="1", Tornionjoki="2", Kemijoki="3", Iijoki="4",
#Luleälven="5",Skellefteälven="6",Umeälven="7",Ångermanälven="8",
#Indalsälven="9",Ljungan="10",Ljusnan="11",Dalälven="12",
#Morrumsån="13",`Unsampled stock`="14"))



library(rjags)
library(runjags)
library(tidyverse)
library(readxl)
library(xlsx)
library(forcats)
library(lubridate)
library(stringr)
library(gridExtra)
library(coda)

source("00-functions/tidy-functions.r")

pathM74<-"H:/Biom/SubE_M74/2019/"

# FI data

dat<-read_xlsx(path=str_c(pathM74,"dat/orig/Finnish_M74_data-2018_paivitetty.xlsx"), 
              col_names = T, guess_max = 10000, sheet=1, na="")   
  
df<-dat%>%
  mutate(river=fct_recode(RIVER,
                                    "1"="Simo",
                                    "2"="Tornio",
                                    "3"="Kemi",
                                    "4"="Iijoki"))%>%
  mutate(rivername=fct_recode(river,
                          "Simojoki"="1","Tornionjoki"="2",
                          "Kemijoki"="3","Iijoki"="4"))%>%
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

dfFI<-df%>% 
  mutate(M74=as.numeric(M74_mort))%>%
  select(YEAR, year, rivername, river, eggs, surv_eggs, M74, mortality100, YSFM)

# Swedish M74 data until 2016 (in final format)
df1<-read_tsv(str_c(pathM74,"dat/der/M74dataSE16.txt"), col_names = T)
              
# New Swedish M74 data
df2<-read_xlsx(str_c(pathM74,"dat/der/Swedish_M74_data_17-18.xlsx"), na="NA")%>%
  mutate(stock=fct_recode(river,
                       "11"="Dal", "10"="Ljusnan", "8"="Indals", "7"="Angerman",
                       "6"="Ume", "5"="Skellefte", "4"="Lule"
                       ))%>%
  mutate(yy=year-1985)%>%
  mutate(Females=Kramade)%>%
  mutate(xx=`Antal M74`)%>%
  select(stock, yy, Females, xx)%>%
  mutate(stock=as.numeric(stock))

# add missing data for swedish stocks with no recent M74 data, plus for unsampled stock 
df3<-tibble(
  yy=32, #2017
  stock=c(9,12,13), # Ljungan, Morrum, unsampled stock
  Females=100,
  xx=as.numeric(NA)
)
df4<-df3%>%mutate(yy=yy+1) # 2018
  
dfSE<-df2%>%
  full_join(df3)%>%
  full_join(df4)%>%
  mutate(stock=stock+1)%>% # +1 is needed to start SE stocks from index 5 (Iijoki included as stock 4)
  select(yy,stock,Females,xx)%>%
  full_join(df1) # This has correct stock numbers already


#View(dfFI)
#View(dfSE)
#filter(dfFI, mortality100=="XX")

# input to BUGS:

length(dfFI$eggs)
# 1562
length(dfSE$xx)
# 330

dfFI.bugs<-dfFI%>%
  select(eggs, year, river, surv_eggs, M74, mortality100)

dfSE.bugs<-dfSE
  
write_csv(dfFI.bugs, str_c(pathM74,"prg/input/M74dataFI18.csv"))
write_csv(dfSE.bugs, str_c(pathM74,"prg/input/M74dataSE18.csv"))

#write_tsv(dfSE.bugs, str_c(pathM74,"prg/input/M74dataSE18.txt"))


# wrangle for figures

ysfm<-dfFI%>%
  group_by(river, YEAR)%>%
  summarise(ysfm=round(mean(YSFM/100),2),
            N_fem=n())

n_M74notNA<-dfFI%>%
  filter(is.na(M74)==F)%>%
  group_by(river, YEAR)%>%
  summarise(N_M74=n())

cases_M74<-dfFI%>%
  group_by(river, YEAR)%>%
  count(M74)%>%
  filter(M74==2)%>%
  mutate(N_M74fem=n)

cases_mort100<-dfFI%>%
  group_by(river, YEAR)%>%
  count(mortality100)%>%
  filter(mortality100==2)%>%
  mutate(N_mort100=n)

dfFI.2<-full_join(ysfm, n_M74notNA)%>%
  full_join(cases_M74)%>%
  full_join(cases_mort100, by=c("river", "YEAR"))%>%
  select(river, YEAR, ysfm, N_fem, N_M74, N_M74fem, N_mort100)%>%
  mutate(N_M74fem=ifelse(is.na(N_M74fem)==T, ifelse(is.na(N_M74)==T, NA, 0),N_M74fem))%>%
  mutate(N_mort100=ifelse(is.na(N_mort100)==T, ifelse(is.na(N_M74)==T, NA, 0),N_mort100))%>%
  mutate(propM74=round(N_M74fem/N_fem,2))%>%
  mutate(prop_mort100=round(N_mort100/N_M74,2))


#View(df)

dfSE.2<-dfSE%>%
  mutate(propM74=round(xx/Females,2))%>%
  mutate(river=as.factor(stock))%>%
#  mutate(rivername=fct_recode(river,
#                          "Lulealven"="5","Skelleftealven"="6",
#                          "Umealven"="7","Angermanalven"="8",
#                          "Indalsalven"="9","Ljungan"="10",
#                          "Ljusnan"="11","Dalalven"="12",
#                          "Morrumsan"="13","Unsampled stock"="14"))%>%
  mutate(YEAR=yy+1984)%>%
  mutate(N_fem=Females)%>%
  mutate(N_M74fem=xx)%>%
  select(YEAR, river, N_fem, N_M74fem, propM74)


#filter(dfFI.2, rivername=="Simo")
#filter(dfSE.2, rivername=="Simo")

dfM74<-full_join(dfFI.2,dfSE.2)%>%
  select(-N_M74)%>%
  ungroup()%>%
  mutate(river=as.factor(river))

#filter(dfM74, RIVER=="Simo")
View(dfM74)

