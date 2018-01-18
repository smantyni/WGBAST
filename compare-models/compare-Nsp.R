
# Compare BUGS/JAGS results

#source("models-select.R")


## ---- load-nsp

# Model 1: BUGS
# =================

Nsp<-array(NA, dim=c(16,30, 1000))

for(y in 6:(length(Years2)+5)){
  for(r in 1:15){
    x<-read.table(paste(sep="", folder1,"/NspWtot[",y,",",r,"]1.txt"))
    Nsp[r,y,]<-x[1:1000,2]
  }
}
for(r in 16:16){ #Kåge
  for(y in 27:(length(Years2)+5)){ # 2013->
    x<-paste(sep="", folder1,"/NspWtot[",y,",",r,"]1.txt")
    temp <-read.table(x)
    Nsp[r,y,]<-temp[1:1000,2]
  }
}

dim(Nsp)

for(r in 1:nstocks){
  df<-boxplot.bugs.df2(Nsp, r ,1:length(Years))%>%
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
  df<-boxplot.jags.df2(chains, "NspWtot[",str_c(r,"]"),1:length(Years))
  #df<-boxplot.jags.df2(dsub, "NspWtot[",str_c(r,"]"),1:length(Years))
  df<-mutate(df, River=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}
df.jags<-as.tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1986)
df.jags


# Spawner count datasets
# =================
counts<-read_tsv("data/spawner_counts.txt")%>%
  mutate(Year=c(1:30))%>%
  mutate(Year=Year+1986)%>%
  gather(key="River", value="Count", `Tornio[]`:`Kalix[]`)%>% # re-arrange 
  mutate(River=fct_recode(River,
                          "1"="Tornio[]",
                          "2"="Simo[]",
                          "3"="Kalix[]"))%>%
  mutate(River=parse_integer(River))%>%
  mutate(Count=Count/1000)

df.bugs<-  left_join(df.bugs,counts, by=NULL)


## ---- graphs-nsp


# Draw boxplots to compare
# ==========================

#df1<-filter(df.bugs, Year>1991)
#df2<-filter(df.jags, Year>1991)

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
  labs(x="Year", y="Number of spawners (1000s)", title=Rivername[r])+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+  
  geom_point(data=df1, aes(Year, Count),col="red")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))#+
  #coord_cartesian(xlim = 1992:2016)
#  facet_wrap(~River) # Facet if you like to have all graphs together, downside is you cannot easily control ylim and scales are very different
)
}

