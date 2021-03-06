#source("05-results/compare-models/models-select.R")



# from cohort+age to calendar years
hrW<-array(NA, dim=c(2,length(Years)-2, 1000))
hrR<-array(NA, dim=c(2,length(Years)-2, 1000))
hcW.au1<-array(NA, dim=c(2,length(Years)-2, 1000))
hcR.au1<-array(NA, dim=c(2,length(Years)-2, 1000))
hdcW<-array(NA, dim=c(2,length(Years)-2, 1000))
hdcR<-array(NA, dim=c(2,length(Years)-2, 1000))
for(y in 3:(length(Years))){ 
  for(a in 2:3){ # Grilse & 2SW (=MSW)
    #hrW[grilse:MSW, ]
    hrW[a-1,y-2,]<-chains[,str_c("HrW[",y-(a-1),",",a,"]")][[1]]
    hrR[a-1,y-2,]<-chains[,str_c("HrR[",y-(a-1),",",a,"]")][[1]]
    
    #hcW[grilse:MSW, ] AU1
    hcW.au1[a-1,y-2,]<-chains[,str_c("HcW[",y-(a-1),",",a,",1]")][[1]]
    hcR.au1[a-1,y-2,]<-chains[,str_c("HcR[",y-(a-1),",",a,",1]")][[1]]
    
    #hdcW[grilse:MSW, ]
    hdcW[a-1,y-2,]<-chains[,str_c("HdcW[",y-(a-1),",",a,"]")][[1]]
    hdcR[a-1,y-2,]<-chains[,str_c("HdcR[",y-(a-1),",",a,"]")][[1]]
    
  }}

hdoW<-array(NA, dim=c(2,length(Years)-2, 1000))
hdoR<-array(NA, dim=c(2,length(Years)-2, 1000))
hlW<-array(NA, dim=c(2,length(Years)-2, 1000))
hlR<-array(NA, dim=c(2,length(Years)-2, 1000))
for(y in 3:(length(Years))){ 
  for(a in 1:2){ # Grilse & 2SW (=MSW)
    
    #hdoW[grilse:MSW, ]
    hdoW[a,y-2,]<-chains[,str_c("HdoW[",y-a,",",a,"]")][[1]]
    hdoR[a,y-2,]<-chains[,str_c("HdoR[",y-a,",",a,"]")][[1]]
    
    #hlW[grilse:MSW, ]
    hlW[a,y-2,]<-chains[,str_c("HlW[",y-a,",",a,"]")][[1]]
    hlR[a,y-2,]<-chains[,str_c("HlR[",y-a,",",a,"]")][[1]]
  }}


# wrangle
##########################

# River fishery

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){ # grilse vs msw
  dfW<-boxplot.bugs.df2(hrW, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="River", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hrR, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="River", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hr<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1993)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1","MSW"= "2"))                        
df.2.Hr


# COASTAL FISHERIES (TRAPNET&GILLNET)

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hcW.au1, a, 1:(length(Years)-2))%>%
    mutate(age=a, Fishery="Coast", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hcR.au1, a, 1:(length(Years)-2))%>%
    mutate(age=a, Fishery="Coast", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type,
         everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1","MSW"= "2"))                        

df.2.Hc


# COASTAL DRIFTNET

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hdcW, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="CDN", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hdcR, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="CDN", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hdc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"="1", "MSW"="2"))
df.2.Hdc


# OFFSHORE DRIFTNET

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hdoW, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="ODN", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hdoR, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="ODN", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hdo<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"="1",
                        "MSW"= "2"))
df.2.Hdo

# OFFSHORE LONGLINE

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hlW, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="OLL", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hlR, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="OLL", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hl<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"="1",
                        "MSW"= "2"))
df.2.Hl



## ---- F42311a

#View(df.2.Hdo)
df2<-filter(df.2.Hdo, Type=="Wild")

ggplot(df2, aes(Year))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore driftnet HR"))+
  geom_line(aes(Year,q50))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age, scales="free") 
  

df2<-filter(df.2.Hl, Type=="Wild")

ggplot(df2, aes(Year))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore longline HR"))+
  geom_line(aes(Year,q50))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age, scales="free") 

## ---- F42311b

# coastal TN & GN

df2<-filter(df.2.Hc, Type=="Wild")

ggplot(df2, aes(Year))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Coastal HR AU1"))+
  geom_line(aes(Year,q50))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age) 


df2<-filter(df.2.Hdc, Type=="Wild")

ggplot(df2, aes(Year))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Coastal driftnet HR"))+
  geom_line(aes(Year,q50))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age, scales="free") 



## ---- F42311c

# river wild
df2<-filter(df.2.Hr, Type=="Wild")

ggplot(df2, aes(Year))+
  theme_bw()+
  geom_boxplot(
    data=df2,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="River HR")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)
