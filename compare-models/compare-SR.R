# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-SR

# Model 1: BUGS
# =================

for(i in 1:nstocks){
  x1<-read.table(paste(sep="", folder1,"/R[",i,"]1.txt")) 
  ifelse(i==1, R0<-x1[,2], R0<-cbind(R0,x1[,2]))
  
  x2<-read.table(paste(sep="", folder1,"/z[",i,"]1.txt")) 
  ifelse(i==1, z<-x2[,2], z<-cbind(z,x2[,2]))

  x3<-read.table(paste(sep="", folder1,"/alpha[",i,"]1.txt")) 
  ifelse(i==1, alpha<-x3[,2], alpha<-cbind(alpha,x3[,2]))

  x4<-read.table(paste(sep="", folder1,"/beta[",i,"]1.txt")) 
  ifelse(i==1, beta<-x4[,2], beta<-cbind(beta,x4[,2]))

#  x5<-read.table(paste(sep="", folder1,"/SBPR[",i,"]1.txt")) 
#  ifelse(i==1, EPR<-x5[,2], beta<-cbind(EPR,x5[,2]))
}


df1<-boxplot.bugs.df(R0, 1:nstocks)%>%
  mutate(par="R0")
df2<-boxplot.bugs.df(z, 1:nstocks)%>%
  mutate(par="z")
df3<-boxplot.bugs.df(alpha, 1:nstocks)%>%
  mutate(par="alpha")
df4<-boxplot.bugs.df(beta, 1:nstocks)%>%
  mutate(par="beta")
#df5<-boxplot.bugs.df(EPR, 1:nstocks)%>%
#  mutate(par="EPR")

df<-full_join(df1,df2)
df<-full_join(df,df3)
df<-full_join(df,df4)

df.bugs<-as.tibble(setNames(df,c("stock","q5","q25","q50","q75","q95", "par" )))
#df.bugs<-mutate(df.bugs,river = fct_recode(factor(stock),
#                                "Torne"="1", 
#                                "Simo"="2", 
#                                "Kalix"="3", 
#                                "Råne"="4", 
#                                "Pite"="5", 
#                                "Åby"="6", 
#                                "Byske"="7", 
#                                "Rickle"="8", 
#                                "Savarån"="9", 
#                                "Ume"="10", 
#                                "Öre"="11", 
#                                "Lögde"="12", 
#                                "Ljungan"="13", 
#                                "Emån"="14", 
#                                "Mörrum"="15", 
#                                "Kåge"="16" ))

# Model 2: JAGS
# =================

df1<-boxplot.jags.df(chains, "R0[", 1:nstocks)%>%
  mutate(par="R0")
df2<-boxplot.jags.df(chains, "z[", 1:nstocks)%>%
  mutate(par="z")
df3<-boxplot.jags.df(chains, "alphaSR[", 1:nstocks)%>%
  mutate(par="alpha")
df4<-boxplot.jags.df(chains, "betaSR[", 1:nstocks)%>%
  mutate(par="beta")

df<-full_join(df1,df2)
df<-full_join(df,df3)
df<-full_join(df,df4)

df.jags<-as.tibble(setNames(df,c("stock","q5","q25","q50","q75","q95","par")))
df.jags

# Draw boxplots to compare
# ==========================

## ---- graphs-SR

df1<-filter(df.bugs, par=="R0")
df2<-filter(df.jags, par=="R0")


ggplot(df2, aes(stock))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Stock", y="R0", title="Potential smolt production capacity")+
#coord_cartesian(ylim=c(0,2500))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  #scale_x_discrete(labels=Rivername)


df1<-filter(df.bugs, par=="z")
df2<-filter(df.jags, par=="z")

ggplot(df2, aes(stock))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Stock", y="z", title="Steepness")+
  #coord_cartesian(ylim=c(0,2500))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

df1<-filter(df.bugs, par=="alpha")
df2<-filter(df.jags, par=="alpha")

ggplot(df2, aes(stock))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Stock", y="alpha", title="alpha SR")+
  #coord_cartesian(ylim=c(0,2500))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


df1<-filter(df.bugs, par=="beta")
df2<-filter(df.jags, par=="beta")

ggplot(df2, aes(stock))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Stock", y="beta", title="beta SR")+
  #coord_cartesian(ylim=c(0,2500))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

