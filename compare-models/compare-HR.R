# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-HR

# Model 1: BUGS
# =================

# input
##########################
HrW<-array(NA, dim=c(6,length(Years)-1, 1000))
HrR<-array(NA, dim=c(6,length(Years)-1, 1000))
HcW<-array(NA, dim=c(6,3,length(Years)-1, 1000))
HcR<-array(NA, dim=c(6,3,length(Years)-1, 1000))
for(y in 1:(length(Years)-1)){
  for(a in 1:6){
    HrW[a,y,]<-read.table(paste(sep="", folder1,"/HrW[",y,",",a,"]1.txt"))[,2]
    HrR[a,y,]<-read.table(paste(sep="", folder1,"/HrR[",y,",",a,"]1.txt"))[,2]

    for(u in 1:3){
      HcW[a,u,y,]<-read.table(paste(sep="", folder1,"/HcW[",y,",",a,",",u,"]1.txt"))[,2]
      HcR[a,u,y,]<-read.table(paste(sep="", folder1,"/HcR[",y,",",a,",",u,"]1.txt"))[,2]

      }
}}

HdcW<-array(NA, dim=c(5,length(Years)-1, 1000))
HdcR<-array(NA, dim=c(5,length(Years)-1, 1000))
for(y in 1:(length(Years)-1)){
  for(a in 1:5){
    HdcW[a,y,]<-read.table(paste(sep="", folder1,"/HdcW[",y,",",a,"]1.txt"))[,2]
    HdcR[a,y,]<-read.table(paste(sep="", folder1,"/HdcR[",y,",",a,"]1.txt"))[,2]
  }
}


# wrangle
##########################
# RIVER FISHERIES
for(a in 1:6){
  dfW<-boxplot.bugs.df2(HrW, a ,1:(length(Years)-1))%>%
    mutate(age=a, Fishery="River", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)

  dfR<-boxplot.bugs.df2(HrR, a ,1:(length(Years)-1))%>%
    mutate(age=a, Fishery="River", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.bugs.Hr<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1986)%>%
  mutate(Age=fct_recode(factor(Age),
                        "PS"= "1","Grilse"= "2",
                        "2SW"= "3","3SW"= "4",
                        "4SW"= "5","5SW"= "6"))
df.bugs.Hr

# COASTAL FISHERIES (TRAPNET&GILLNET)
for(a in 1:6){
  for(u in 1:3){
    dfW<-boxplot.bugs.df3(HcW, a, u, 1:(length(Years)-1))%>%
      mutate(age=a, Fishery="Coast", Type="Wild", AU=u)
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df3(HcR, a, u, 1:(length(Years)-1))%>%
      mutate(age=a, Fishery="Coast", Type="Reared", AU=u)
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}}

df<-full_join(dfW2,dfR2, by=NULL)

df.bugs.Hc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type", "AU")))%>%
  select(Age, Fishery, Type, AU, everything())%>%
  mutate(Year=Year+1986)%>%
  mutate(Age=fct_recode(factor(Age),
                        "PS"= "1","Grilse"= "2",
                        "2SW"= "3","3SW"= "4",
                        "4SW"= "5","5SW"= "6"))
df.bugs.Hc


# COASTAL DRIFTNET
for(a in 1:5){
  dfW<-boxplot.bugs.df2(HdcW, a ,1:(length(Years)-1))%>%
    mutate(age=a, Fishery="CDN", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  #dfR<-boxplot.bugs.df2(HdcR, a ,1:(length(Years)-1))%>%
  #  mutate(age=a, Fishery="CDN", Type="Reared")
  #ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.bugs.Hdc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1986)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"="1",
                        "2SW"= "2","3SW"= "3",
                        "4SW"= "4","5SW"= "5"))
df.bugs.Hdc


# Model 2: JAGS
# =================
#
#summary(chains[ ,regexpr("LW",varnames(chains))>0])

#for(a in 1:4){
#  dfW<-boxplot.jags.df2(chains, "HrW[",str_c(a,"]"),1:length(Years))%>%
#    mutate(Age=a, Fishery="River", Type="Wild")
#  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
#}

#df<-full_join(dfW2,dfR2, by=NULL)
#df<-dfW2 # Reared is now missing

#df.jags<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
#  select(Age, Fishery, Type, everything())%>%
#  mutate(Year=Year+1986)%>%
#  mutate(Age=fct_recode(factor(Age),
#                        "Grilse"= "1",
#                        "2SW"= "2",
#                        "3SW"= "3",
#                        "4SW"= "4"))
#df.jags

# Draw boxplots to compare
# ==========================

## ---- graphs-HR

# Wild
df1<-filter(df.bugs, Type=="Wild")
df2<-filter(df.jags, Type=="Wild")

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
  labs(x="Year", y="Proportion per age group", title="Homing rates, wild")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)




