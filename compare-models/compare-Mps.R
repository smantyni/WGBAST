# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-mps

# Model 1: BUGS
# =================

for(y in 1:(length(YearsB))){
  x1<-read.table(paste(sep="", folder1,"/MpsW[",y,"]1.txt") )
  ifelse(y==1, WMort<-x1[,2], WMort<-cbind(WMort,x1[,2]))

  x2<-read.table(paste(sep="", folder1,"/MpsR[",y,"]1.txt") )
  ifelse(y==1, RMort<-x2[,2], RMort<-cbind(RMort,x2[,2]))
}

WSurv<-(exp(-as.mcmc(WMort)))
RSurv<-(exp(-as.mcmc(RMort)))
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

df.bugs<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
  mutate(Year=Year+1986)
df.bugs

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
dfM.bugs<-as.tibble(setNames(dfM,c("Type","q5","q25","q50","q75","q95")))%>%
  mutate(Type= fct_recode(factor(Type), "Wild"="1", "Reared"="2", "Ratio"="3"))


# Model 2: JAGS
# =================

#summary(chains[ ,regexpr("Mps",varnames(chains))>0])

#dfW<-boxplot.jags.df(chains, "survMpsW[", 1:length(Years))%>%
#  mutate(Type="Wild")
#dfR<-boxplot.jags.df(chains, "survMpsR[", 1:length(Years))%>%
#  mutate(Type="Reared")

#summary(chains[,"MpsW[1]"][[1]])
#summary(exp(-chains[,"MpsW[1]"][[1]]))

survMpsW<-array(NA, dim=c(length(chains[,"MpsW[1]"][[1]]),length(Years)))
survMpsR<-array(NA, dim=c(length(chains[,"MpsR[1]"][[1]]),length(Years)))
ratio<-array(NA, dim=c(length(chains[,"MpsR[1]"][[1]]),length(Years)))
for(y in 1:(length(Years))){
  survMpsW[,y]<-exp(-chains[,str_c("MpsW[",y,"]")][[2]])
  survMpsR[,y]<-exp(-chains[,str_c("MpsR[",y,"]")][[2]])
  ratio[,y]<-survMpsR[,y]/survMpsW[,y]
}



dfW<-boxplot.bugs.df(survMpsW, 1:(length(Years)))%>%
  mutate(Type="Wild")
dfR<-boxplot.bugs.df(survMpsR, 1:(length(Years)))%>%
  mutate(Type="Reared")
df_ratio<-boxplot.bugs.df(ratio, 1:(length(Years)))%>%
  mutate(Type="Ratio")

df<-full_join(dfW,dfR, by=NULL)
df<-full_join(df,df_ratio, by=NULL)

df.jags<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
mutate(Year=Year+1986)
df.jags

# adult M
M2<-cbind(chains[,"MW"][[1]],chains[,"MR"][[1]])
M2<-exp(-M2)
M2<-as.tibble(M2)%>%
  setNames(c("Wild", "Reared"))%>%
  mutate(Ratio=Reared/Wild)

dfM2<-boxplot.bugs.df(M2, 1:3) #Both types in same!
dfM.jags<-as.tibble(setNames(dfM2,c("Type","q5","q25","q50","q75","q95")))%>%
  mutate(Type= fct_recode(factor(Type), "Wild"="1", "Reared"="2", "Ratio"="3"))


# Draw boxplots to compare
# ==========================

## ---- graphs-mps
df1<-df.bugs
df2<-df.jags

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
df1<-dfM.bugs
df2<-dfM.jags

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


par(mfrow=c(2,3))
for(i in 1:length(Years)){
  gd<-gelman.diag(chains[,str_c("MpsW[",i,"]")])
  if(gd$psrf[2]>2){
    #print(c(i, gd$psrf))
    traceplot(chains[,str_c("MpsW[",i,"]")], main=str_c("MpsW ",df.jags$Year[i]))
  }
}

traceplot(chains[,"MW"], main="MW")
traceplot(chains[,"MR"], main="MR")


