# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# CatchGraphs.R  from Polina (9/2008)
# 
# Makes the graphs of the amount of estimated and predicted (future) catches 
# for coastal, offshore and river fisheries and in total.
#
# Changes by Henni.
# - graphs without FLCore 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~ 
              
library(coda)
library(tidyverse)
library(stringr)

PathScen<-"H:/FLR/WGBAST18/Scenarios/" # scenario results 
pathData<-"C:/Users/412hpulkkin/Dropbox/WGBAST/JAGS/data_2018/"
source("functions/tidy-functions.r")



################################################################################
#! #############################################################################
# Version of the estimation model
#Model<-"Orig"
Model<-"New_SR"

# Time
LastHistYear<-2017
LastPredYear<-2032
yBreak<-length(c(1992:LastHistYear))
year<-c(1992:LastPredYear)
length(year)
Nyears<-length(year)
Nstocks<-16


E_OLL_hist<-read.table(str_c(pathData,"EffortOLL_ICES.txt"))
tmp<-(E_OLL_hist$Trolling/E_OLL_hist$Total)
prop_recr<-tmp[6:length(tmp)] # skip 1987-1991

# ===============================================================================
# Scenarios
#! Mps
choice<-"MED"   # corresponds to Mps during 2008-2011 period

# Maturation is the same in all scenarios
#! Effort 
EffScen<-1

scen_nr<-c(1:4,6,5)

for(s in 1:6){
  
  EffScen<-scen_nr[s]
  #Load the file containing stats
  File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen",EffScen,".RData")
  
  File
  load(File)
  
  Coef1<-0.741 
  CoefTrollingF<-0.679 # this coef produces the 32k of recr catch when commercial fisheries are removed
  if(EffScen==1){Coef2<-1} # previous advice approach
  if(EffScen==2){Coef2<-1.221} #+20%
  if(EffScen==3){Coef2<-0.781} #-20%
  if(EffScen==4){Coef2<-1.412} #F0.1a)
  if(EffScen==5){Coef2<-0} 
  
  Coef<-Coef1*Coef2
  
  # These according to ProjEffort-file
  E_OLL_DEN<-0.42*Coef
  E_OLL_PL<-13.5*Coef
  E_OLL_TROLLING<-6.56*CoefTrollingF # this covers all recr fishing
  prop_recr[(yBreak+1):Nyears]<-E_OLL_TROLLING/(E_OLL_DEN+E_OLL_PL+E_OLL_TROLLING)
  prop_recr<-as.matrix(prop_recr)
  rownames(prop_recr)<-year
  # ===============================================================================
  
  cbind(c(1992:LastPredYear),c(1:length(year)))
  
  C_OLL<-array(NA, c(1000,Nyears))
  C_CTN<-array(NA, c(1000,Nyears))
  CalC_comm<-array(NA, c(1000,Nyears))
  CalC_recr<-array(NA, c(1000,Nyears))
  for(i in 1:1000){
    for(y in 1:Nyears){
      C_OLL[i,y]<-sum(WOLLCtot[2:6,y,1:Nstocks,i], na.rm=T)+
                  sum(ROLLCtot[2:6,y,1:4,i])    
      C_CTN[i,y]<-sum(WCTNCtot[2:6,y,1:Nstocks,i], na.rm=T)+
                  sum(RCTNCtot[2:6,y,1:4,i])
      
      # 0.546 is the proportion of wanted catch reported out of all commercial, see T4_3_2_1_workfile.xlsx 
      
      if(y>1){
        if(s<5){
          CalC_recr[i,y]<-C_OLL[i,y-1]*prop_recr[y-1]
          CalC_comm[i,y]<-(C_OLL[i,y-1]*(1-prop_recr[y-1])+C_CTN[i,y])*0.546
        }else{
          if(y<=(yBreak+1)){ # until 2018 (yBreak+1 is the interim year, fishing scenarios affect coastal but not yet offshore fisheries)
            CalC_recr[i,y]<-C_OLL[i,y-1]*prop_recr[y-1]
            CalC_comm[i,y]<-(C_OLL[i,y-1]*(1-prop_recr[y-1])+C_CTN[i,y])*0.546
          }
          if(y>(yBreak+1)){ #2019->
            CalC_comm[i,y]<-0
            CalC_recr[i,y]<-C_OLL[i,y-1]
          }
        }
      }
    }
  }
  
  # CalC_tot is the commercial sea removal, containing misreported, unreported and discards
  dfC<-boxplot.bugs.df(CalC_comm, 1:Nyears)%>%
    mutate(Type="Comm")
  dfR<-boxplot.bugs.df(CalC_recr, 1:Nyears)%>%
    mutate(Type="Recr")
  
  tmp<-full_join(dfC,dfR)
  
  tmp<-as.tibble(setNames(tmp,c("Year","q5","q25","q50","q75","q95", "Type")))%>%
    mutate(Year=Year+1991)%>%
    mutate(Scen=s)
  if(s>1){df<-full_join(df,tmp)}else{df<-tmp}
}

df<-df%>%mutate(Scen=parse_factor(Scen, levels=NULL))
#View(df)
# ============================================

# Observations
tmp<-read_tsv(str_c(pathData, "Catch_withTrolling.txt"))
colnames(tmp)<-c("river", "coast", "offs")

obs_r<-tmp[,1]%>%mutate(Type="River", Year=1987:LastHistYear, obs_catch=river)%>%select(-river)
obs_c<-tmp[,2]%>%mutate(Type="Coast", Year=1987:LastHistYear, obs_catch=coast)%>%select(-coast)
obs_o<-tmp[,3]%>%mutate(Type="Offshore", Year=1987:LastHistYear, obs_catch=offs)%>%select(-offs)

obs<-full_join(obs_r,obs_c, by=NULL)
obs<-full_join(obs,obs_o, by=NULL)

obs_t<-obs%>%group_by(Year)%>%
  summarise(obs_catch=sum(obs_catch))%>%
  mutate(Type="Total")

obs<-full_join(obs, obs_t, by=NULL)

df<-full_join(df, obs_o, by=NULL)

# ============================================

df<-filter(df, Year>2009 & Year<2026)

df1<-filter(df, Type=="Recr")
df2<-filter(df, Type=="Comm")

ggplot(df2, aes(Year))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Year", y="Catch (1000's)", title="Sea catches")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3))+
  coord_cartesian(xlim=c(2009,2026))+
  facet_wrap(~Scen)

#filter(df,Scen==5, Year==2018)






  