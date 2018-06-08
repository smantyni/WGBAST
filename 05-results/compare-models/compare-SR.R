# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-SR

# Model 1: BUGS
# =================
if(compare=="BJ"){
  
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
  
  df.1<-as.tibble(setNames(df,c("stock","q5","q25","q50","q75","q95", "par" )))
}

if(compare=="JJ"){
  
    
  if(SRnew=="no"){
    df1<-boxplot.jags.df(chains1, "R0[", 1:nstocks)%>%
      mutate(par="R0")
    
    df2<-boxplot.jags.df(chains1, "z[", 1:nstocks)%>%
      mutate(par="z")
    df5<-df1%>%
      mutate(par="R0_rep")
    df<-full_join(df1,df2)
    
  }
  if(SRnew=="yes"){
    df1<-boxplot.jags.df(chains1, "K[", 1:nstocks)%>%
      mutate(par="K")
    
    df2<-boxplot.jags.df(chains1, "z[32,", 1:nstocks)%>%
      mutate(par="z")
    df<-full_join(df1,df2)
    
    df5<-boxplot.jags.df(chains1, "R0[32,", 1:nstocks)%>%
      mutate(par="R0")
    
  }
  df3<-boxplot.jags.df(chains1, "alphaSR[", 1:nstocks)%>%
    mutate(par="alpha")
  df4<-boxplot.jags.df(chains1, "betaSR[", 1:nstocks)%>%
    mutate(par="beta")
  
  df<-full_join(df,df3)
  df<-full_join(df,df4)
  df<-full_join(df,df5)
  
  df.1<-as.tibble(setNames(df,c("stock","q5","q25","q50","q75","q95","par")))
  df.1
}

# Model 2: JAGS
# =================

if(SRnew=="no"){
  df1<-boxplot.jags.df(chains, "R0[", 1:nstocks)%>%
    mutate(par="R0")
  
  df2<-boxplot.jags.df(chains, "z[", 1:nstocks)%>%
    mutate(par="z")
  df5<-df1%>%
    mutate(par="R0_rep")
  df<-full_join(df1,df2)
  
}
if(SRnew=="yes"){
  df1<-boxplot.jags.df(chains, "K[", 1:nstocks)%>%
    mutate(par="K")
  
  df2<-boxplot.jags.df(chains, "z[32,", 1:nstocks)%>%
    mutate(par="z")
  df<-full_join(df1,df2)
  
  df5<-boxplot.jags.df(chains, "R0[32,", 1:nstocks)%>%
    mutate(par="R0")
  
}
df3<-boxplot.jags.df(chains, "alphaSR[", 1:nstocks)%>%
  mutate(par="alpha")
df4<-boxplot.jags.df(chains, "betaSR[", 1:nstocks)%>%
  mutate(par="beta")

df<-full_join(df,df3)
df<-full_join(df,df4)
df<-full_join(df,df5)

df.2<-as.tibble(setNames(df,c("stock","q5","q25","q50","q75","q95","par")))
df.2

# Draw boxplots to compare
# ==========================

## ---- graphs-SR

df1<-filter(df.1, par=="R0", stock!=1 & stock!=3)
df2<-filter(df.2, par=="R0", stock!=1 & stock!=3)

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
  coord_cartesian(ylim=c(0,450))+
  scale_x_continuous(breaks = c(1:16), labels=Rivername)


df1<-filter(df.1, par=="R0", stock<4 & stock!=2)
df2<-filter(df.2, par=="R0", stock<4 & stock!=2)

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
  #coord_cartesian(ylim=c(0,450))+
  scale_x_continuous(breaks = c(1:3), labels=Rivername[1:3])
  


df1<-filter(df.1, par=="z")
df2<-filter(df.2, par=="z")

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
  scale_x_continuous(breaks = c(1:16), labels=Rivername)

df1<-filter(df.1, par=="alpha")
df2<-filter(df.2, par=="alpha")

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
  scale_x_continuous(breaks = c(1:16), labels=Rivername)


df1<-filter(df.1, par=="beta")
df2<-filter(df.2, par=="beta")

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
  scale_x_continuous(breaks = c(1:16), labels=Rivername)


## ---- graphs-SR-traces



#traceplot(chains[,"MW"], main="MW")
#traceplot(chains[,"MR"], main="MR")

