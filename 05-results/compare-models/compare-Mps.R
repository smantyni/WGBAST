# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-mps

# Model 1: BUGS
# =================

if(compare=="BJ"){
    
  for(y in 1:(length(YearsB))){
    x1<-read.table(paste(sep="", folder1,"/MpsW[",y,"]1.txt") )
    ifelse(y==1, WMort<-x1[,2], WMort<-cbind(WMort,x1[,2]))
  
    x2<-read.table(paste(sep="", folder1,"/MpsR[",y,"]1.txt") )
    ifelse(y==1, RMort<-x2[,2], RMort<-cbind(RMort,x2[,2]))
  }
  
  WSurv<-(exp(-as.mcmc(WMort*(11/12))))
  RSurv<-(exp(-as.mcmc(RMort*(9/12))))
  ratio<-RSurv/WSurv
  #dim(WSurv)
  
  dfW<-boxplot.bugs.df(WSurv, 1:(length(YearsB)))%>%
    mutate(Type="Wild")
  dfR<-boxplot.bugs.df(RSurv, 1:(length(YearsB)))%>%
    mutate(Type="Reared")
  df_ratio<-boxplot.bugs.df(ratio, 1:(length(YearsB)))%>%
    mutate(Type="Ratio")
  
  df<-full_join(dfW,dfR, by=NULL)
  df<-full_join(df,df_ratio, by=NULL)
  
  df.1<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
    mutate(Year=Year+1986)
  df.1
  
  # adult M
  
  #traceplot(as.mcmc(M[,1]))
  #traceplot(as.mcmc(M[,2]))
  
                    
  M<-cbind(read.table(paste(sep="", folder1,"/MW1.txt"))[,2],
           read.table(paste(sep="", folder1,"/MR1.txt"))[,2])
  M<-exp(-M)
  M<-as.tibble(M)%>%
    setNames(c("Wild", "Reared"))
  
  M<-M%>%mutate(Ratio=Reared/Wild)
  
  dfM<-boxplot.bugs.df(M, 1:3) #Both types in same!
  dfM.1<-as.tibble(setNames(dfM,c("Type","q5","q25","q50","q75","q95")))%>%
    mutate(Type= fct_recode(factor(Type), "Wild"="1", "Reared"="2", "Ratio"="3"))
}

#traceplot(as.mcmc(M$Reared))


if(compare=="JJ"){
  survMpsW<-array(NA, dim=c(length(chains1[,"MpsW[1]"][[1]])*2,length(Years)))
  survMpsR<-array(NA, dim=c(length(chains1[,"MpsR[1]"][[1]])*2,length(Years)))
  ratio<-array(NA, dim=c(length(chains1[,"MpsR[1]"][[1]])*2,length(Years)))
  for(y in 1:(length(Years))){
    survMpsW[,y]<-exp(-as.matrix(chains1[,str_c("MpsW[",y,"]")]))
    survMpsR[,y]<-exp(-as.matrix(chains1[,str_c("MpsR[",y,"]")]))
    ratio[,y]<-survMpsR[,y]/survMpsW[,y]
  }
  
  #survMpsW<-array(NA, dim=c(length(chains1[,"MpsW[1]"][[1]]),length(Years)))
  #survMpsR<-array(NA, dim=c(length(chains1[,"MpsR[1]"][[1]]),length(Years)))
  #ratio<-array(NA, dim=c(length(chains1[,"MpsR[1]"][[1]]),length(Years)))
  #for(y in 1:(length(Years))){
  #  survMpsW[,y]<-exp(-chains1[,str_c("MpsW[",y,"]")][[2]])
  #  survMpsR[,y]<-exp(-chains1[,str_c("MpsR[",y,"]")][[2]])
  #  ratio[,y]<-survMpsR[,y]/survMpsW[,y]
  #}
  
  dfW<-boxplot.bugs.df(survMpsW, 1:(length(Years)))%>%
    mutate(Type="Wild")
  dfR<-boxplot.bugs.df(survMpsR, 1:(length(Years)))%>%
    mutate(Type="Reared")
  df_ratio<-boxplot.bugs.df(ratio, 1:(length(Years)))%>%
    mutate(Type="Ratio")
  
  df<-full_join(dfW,dfR, by=NULL)
  df<-full_join(df,df_ratio, by=NULL)
  
  df.1<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
    mutate(Year=Year+1986)
  df.1
  
  # adult M
  dW<-as.matrix(chains1[,"MW"])
  dR<-as.matrix(chains1[,"MR"])
  M<-cbind(dW,dR)
  #M<-cbind(chains1[,"MW"][[1]],chains1[,"MR"][[1]])
  M<-exp(-M)
  summary(as.mcmc(M))
  M<-as.tibble(M)%>%
    setNames(c("Wild", "Reared"))%>%
    mutate(Ratio=Reared/Wild)
  
  dfM1<-boxplot.bugs.df(M, 1:3) #Both types in same!
  dfM.1<-as.tibble(setNames(dfM1,c("Type","q5","q25","q50","q75","q95")))%>%
    mutate(Type= fct_recode(factor(Type), "Wild"="1", "Reared"="2", "Ratio"="3"))
} 
  


# Model 2: JAGS
# =================

#summary(chains[ ,regexpr("Mps",varnames(chains))>0])
#summary(chains[,"MpsW[1]"][[1]])
#summary(exp(-chains[,"MpsW[1]"][[1]]))

if(JAGSversion=="old"){
  survMpsW<-array(NA, dim=c(length(chains[,"survMpsW[1]"][[1]])*2,length(Years)))
  survMpsR<-array(NA, dim=c(length(chains[,"survMpsW[1]"][[1]])*2,length(Years)))
  ratio<-array(NA, dim=c(length(chains[,"survMpsW[1]"][[1]])*2,length(Years)))
  for(y in 1:(length(Years))){
    survMpsW[,y]<-as.matrix(chains[,str_c("survMpsW[",y,"]")])
    survMpsR[,y]<-as.matrix(chains[,str_c("survMpsR[",y,"]")])
    ratio[,y]<-survMpsR[,y]/survMpsW[,y]
  }
}else{

  survMpsW<-array(NA, dim=c(length(chains[,"MpsW[1]"][[1]])*2,length(Years)))
  survMpsR<-array(NA, dim=c(length(chains[,"MpsR[1]"][[1]])*2,length(Years)))
  ratio<-array(NA, dim=c(length(chains[,"MpsR[1]"][[1]])*2,length(Years)))
  for(y in 1:(length(Years))){
    survMpsW[,y]<-exp(-as.matrix(chains[,str_c("MpsW[",y,"]")]))
    survMpsR[,y]<-exp(-as.matrix(chains[,str_c("MpsR[",y,"]")]))
    ratio[,y]<-survMpsR[,y]/survMpsW[,y]
  }
}


dfW<-boxplot.bugs.df(survMpsW, 1:(length(Years)))%>%
  mutate(Type="Wild")
dfR<-boxplot.bugs.df(survMpsR, 1:(length(Years)))%>%
  mutate(Type="Reared")
df_ratio<-boxplot.bugs.df(ratio, 1:(length(Years)))%>%
  mutate(Type="Ratio")

df<-full_join(dfW,dfR, by=NULL)
df<-full_join(df,df_ratio, by=NULL)


df.2<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
mutate(Year=Year+1986)
df.2

# adult M
dW<-as.matrix(chains[,"MW"])
dR<-as.matrix(chains[,"MR"])
M2<-cbind(dW,dR)
M2<-exp(-M2)
summary(as.mcmc(M2))
#1. Empirical mean and standard deviation for each variable,
#plus standard error of the mean:
  
#  Mean      SD  Naive SE Time-series SE
#var1 0.9159 0.01598 0.0003573       0.003978
#var1 0.7513 0.03554 0.0007948       0.001674

#2. Quantiles for each variable:
  
#  2.5%    25%    50%    75%  97.5%
#var1 0.8848 0.9041 0.9151 0.9270 0.9484
#var1 0.7070 0.7236 0.7434 0.7717 0.8380

M2<-as.tibble(M2)%>%
  setNames(c("Wild", "Reared"))%>%
  mutate(Ratio=Reared/Wild)

dfM2<-boxplot.bugs.df(M2, 1:3) #Both types in same!
dfM.2<-as.tibble(setNames(dfM2,c("Type","q5","q25","q50","q75","q95")))%>%
  mutate(Type= fct_recode(factor(Type), "Wild"="1", "Reared"="2", "Ratio"="3"))


# Draw boxplots to compare
# ==========================

## ---- graphs-mps
df1<-df.1
df2<-df.2

#for(i in 1:2){}
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
  labs(x="Year", y="Survival", title="Post-smolt survival")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_grid(Type~.)

# AdultM
df1<-dfM.1
df2<-dfM.2

ggplot(df2, aes(Type))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4))+
  labs(x="Year", y="Survival", title="Adult survival")
  

## ---- graphs-mortality-traces



# Model 1 traces, if JAGS
# windows()
#par(mfrow=c(2,3))
#for(i in 1:length(Years)){
#    gd<-gelman.diag(chains1[,str_c("MpsW[",i,"]")])
#    if(gd$psrf[2]>1.5){
#      #print(c(i, gd$psrf))
#      traceplot(chains1[,str_c("MpsW[",i,"]")], main=str_c("MpsW ",df.2$Year[i]))
#    }
#}


par(mfrow=c(2,3))
for(i in 1:length(Years)){
  if(JAGSversion=="old"){
    gd<-gelman.diag(chains[,str_c("survMpsW[",i,"]")])
    if(gd$psrf[2]>1.5){
      traceplot(chains[,str_c("survMpsW[",i,"]")], main=str_c("MpsW ",df.2$Year[i]))
    }
  }else{
    gd<-gelman.diag(chains[,str_c("MpsW[",i,"]")])
    if(gd$psrf[2]>1.5){
      #print(c(i, gd$psrf))
      traceplot(chains[,str_c("MpsW[",i,"]")], main=str_c("MpsW ",df.2$Year[i]))
    }
  }
}

traceplot(chains[,"MW"], main="MW")
traceplot(chains[,"MR"], main="MR")

#par(mfrow=c(2,1))
#traceplot(exp(-chains[,"MR"]))
#traceplot(as.mcmc(M$Reared))
