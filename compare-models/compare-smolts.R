
# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-smolts


# Model 1: BUGS
# =================

smolts<-array(NA, dim=c(16,length(Years2B)+7, 1000))

for(y in 6:(length(Years2B)+7)){
  for(r in 1:15){
    x<-read.table(paste(sep="", folder1,"/SmoltWW[",y,",",r,"]1.txt"))
    smolts[r,y,]<-x[1:1000,2]
  }
}
for(r in 16:16){ #Kåge
  for(y in 27:(length(Years2B)+7)){ # 2013->
    x<-read.table(paste(sep="", folder1,"/SmoltWW[",y,",",r,"]1.txt"))
    smolts[r,y,]<-x[1:1000,2]
  }
}

for(r in 1:nstocks){
  df<-boxplot.bugs.df2(smolts, r ,1:length(YearsB))%>%
    mutate(River=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}
df.bugs<-as.tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1986)
df.bugs


# Model 2: JAGS
# =================

# Number of spawners per river
for(r in 1:nstocks){
  #r<-1
  df<-boxplot.jags.df2(chains, "SmoltWW[",str_c(r,"]"),1:(length(Years)+3))
  #df<-boxplot.jags.df2(dsub, "NspWtot[",str_c(r,"]"),1:length(Years))
  df<-mutate(df, River=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}
df.jags<-as.tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1986)
df.jags


## ---- graphs-smolts


# Draw boxplots to compare
# ==========================

#df1<-df.bugs
#df2<-df.jags

for(r in 1:16){
  #r<-1
  df1<-filter(df.bugs, River==r, Year>1991)
  df2<-filter(df.jags, River==r, Year>1991)
  print(ggplot(df2, aes(Year))+
          theme_bw()+
          geom_boxplot(
            data=df1,
            mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
            stat = "identity",
            colour="grey", fill="grey95")+
          geom_boxplot(
            aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
            stat = "identity",fill=rgb(1,1,1,0.6))+
          labs(x="Year", y="Number of smolts (1000s)", title=Rivername_long[r])+
          geom_line(aes(Year,q50))+
          geom_line(data=df1,aes(Year,q50),col="grey")+  
          scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
  )
}

