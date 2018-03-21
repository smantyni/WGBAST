# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-mat

# Model 1: BUGS
# =================



LW<-array(NA, dim=c(4,length(YearsB)+1,1000))
LR<-array(NA, dim=c(4,length(YearsB)+1,1000))
for(y in 1: length(YearsB)){
#    ifelse(prevAss==1,length(Years),length(Years)+1)){
  for(a in 1:4){
    LR[a,y,]<-read.table(paste(sep="", folder1,"/LR[",y,",",a,"]1.txt"))[,2]
    LW[a,y,]<-read.table(paste(sep="", folder1,"/LW[",y,",",a,"]1.txt"))[,2]
  }
}

for(a in 1:4){
  dfR<-boxplot.bugs.df2(LR, a ,1:length(YearsB))%>%
    mutate(age=a, Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)

  dfW<-boxplot.bugs.df2(LW, a ,1:length(YearsB))%>%
    mutate(age=a, Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.bugs<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Type")))%>%
  select(Age, Type, everything())%>%
  mutate(Year=Year+1986)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1",
                        "2SW"= "2",
                        "3SW"= "3",
                        "4SW"= "4"))
df.bugs
#View(df.bugs)


# Model 2: JAGS
# =================
#summary(chains[ ,regexpr("LR",varnames(chains))>0])

for(a in 1:4){
  dfW<-boxplot.jags.df2(chains, "LW[",str_c(a,"]"),1:length(Years))%>%
    mutate(Age=a, Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)

  dfR<-boxplot.jags.df2(chains, "LR[",str_c(a,"]"),1:length(Years))%>%
    mutate(Age=a, Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)
#df<-dfW2 # if reared is missing

df.jags<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Type")))%>%
  select(Age, Type, everything())%>%
  mutate(Year=Year+1986)%>%
  mutate(Age=fct_recode(factor(Age),
                                 "Grilse"= "1",
                                 "2SW"= "2",
                                 "3SW"= "3",
                                 "4SW"= "4"))
df.jags

# Draw boxplots to compare
# ==========================

## ---- graphs-mat

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

df1<-filter(df.bugs, Type=="Reared")
df2<-filter(df.jags, Type=="Reared")

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
  labs(x="Year", y="Proportion per age group", title="Homing rates, reared")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)



## ---- graphs-mat-traces


par(mfrow=c(2,3))
for(i in 1:length(Years)){
  for(a in 1:4){
#i<-1
#a<-4
        gd<-gelman.diag(chains[,str_c("LW[",i,",",a,"]")])
    if(gd$psrf[2]>2){
      #print(c(a,i, gd$psrf))
      traceplot(chains[,str_c("LW[",i,",",a,"]")], main=str_c("LW age=",a," ",df.jags$Year[i]))
    }
  }
}

par(mfrow=c(2,3))
for(i in 1:length(Years)){
  for(a in 1:4){
    #i<-1
    #a<-4
    gd<-gelman.diag(chains[,str_c("LR[",i,",",a,"]")])
    if(gd$psrf[2]>2){
      #print(c(a,i, gd$psrf))
      traceplot(chains[,str_c("LR[",i,",",a,"]")], main=str_c("LR age=",a," ",df.jags$Year[i]))
    }
  }
}






