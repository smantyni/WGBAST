# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-mps

# Model 1: BUGS
# =================

for(y in 1:(length(Years)-1)){
  x<-paste(sep="", folder1,"/MpsW[",y,"]1.txt") 
  tempW <-read.table(x)
  ifelse(y==1, WMort<-tempW[,2], WMort<-cbind(WMort,tempW[,2]))
}

WSurv<-(exp(-as.mcmc(WMort)))

df<-boxplot.bugs.df(WSurv, 1:(length(Years)-1))

df.bugs<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95")))%>%
  mutate(Year=Year+1986)
df.bugs


# Model 2: JAGS
# =================

df2<-boxplot.jags.df(chains, "survMpsW[", 1:length(Years))
df.jags<-as.tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95")))%>%
mutate(Year=Year+1986)
df.jags

# Draw boxplots to compare
# ==========================

## ---- graphs-mps
df1<-df.bugs
df2<-df.jags

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
  labs(x="Year", y="Survival", title="Post-smolt survival, wild")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

 

