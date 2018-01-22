# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-mat

# Model 1: BUGS
# =================



LW<-array(NA, dim=c(4,length(Years)+1,1000))
LR<-array(NA, dim=c(4,length(Years)+1,1000))
for(y in 1:     length(Years)){
#    ifelse(prevAss==1,length(Years),length(Years)+1)){
  for(a in 1:4){
    LR[a,y,]<-read.table(paste(sep="", folder1,"/LR[",y,",",a,"]1.txt"))[,2]
    LW[a,y,]<-read.table(paste(sep="", folder1,"/LW[",y,",",a,"]1.txt"))[,2]
  }
}

for(a in 1:4){
  dfR<-boxplot.bugs.df2(LR, a ,1:length(Years))%>%
    mutate(age=a, wild=0)
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)

  dfW<-boxplot.bugs.df2(LW, a ,1:length(Years))%>%
    mutate(age=a, wild=1)
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.bugs<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Wild")))%>%
  select(Age, Wild, everything())%>%
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

# Number of spawners per river
for(a in 1:4){
  dfW<-boxplot.jags.df2(chains, "LW[",str_c(a,"]"),1:length(Years))%>%
    mutate(Age=a, Wild=1)
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)

#  dfR<-boxplot.jags.df2(chains, "LR[",str_c(a,"]"),1:length(Years))%>%
#    mutate(Age=a)
#  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

#df<-full_join(dfW2,dfR2, by=NULL)
df<-dfW2 # Reared is now missing

df.jags<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age", "Wild")))%>%
  select(Age, Wild, everything())%>%
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
df1<-filter(df.bugs, Wild==1)
df2<-filter(df.jags, Wild==1)

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






