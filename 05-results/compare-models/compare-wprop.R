# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-wprop

tmp1<-read.table(str_c(pathData, "Scale.txt"), header=T)[,1]
tmp2<-read.table(str_c(pathData, "Scale.txt"), header=T)[,3]


fix<-2 # or 1

tmp1<-as.tibble(tmp1)%>%
  mutate(Type="2SW", Year=c(1987:(Years[length(Years)]+fix)))
tmp2<-as.tibble(tmp2)%>%
  mutate(Type="3SW", Year=c(1987:(Years[length(Years)]+fix)))

obs<-full_join(tmp1, tmp2, by=NULL)
colnames(obs)<-c("obs_prop", "Type", "Year")



# Model 1: BUGS
# =================
if(compare=="BJ"){
  
  Wprop2SW<-array(NA, dim=c(1000,length(YearsB)))
  Wprop3SW<-array(NA, dim=c(1000,length(YearsB)))
  
  for(y in 7:(length(YearsB)-3)){
    Wprop2SW[,y]<-read.table(str_c(folder1,"/Wprop[",y,",1]1.txt"))[,2] # 2SW
    Wprop3SW[,y]<-read.table(str_c(folder1,"/Wprop[",y,",2]1.txt"))[,2] # 3SW
  }
  
  df_2sw<-boxplot.bugs.df(Wprop2SW, 1:(length(YearsB)))%>%
    mutate(Type="2SW")
  df_3sw<-boxplot.bugs.df(Wprop3SW, 1:(length(YearsB)))%>%
    mutate(Type="3SW")
  
  df<-full_join(df_2sw,df_3sw, by=NULL)
  
  df.1<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
    mutate(Year=Year+1986)
  df.1
}

if(compare=="JJ"){
  df_2sw<-boxplot.jags.df2(chains1, "Wprop[", "1]", 6:(length(Years)-1))%>%
    mutate(Type="2SW")
  df_3sw<-boxplot.jags.df2(chains1, "Wprop[", "2]", 6:(length(Years)-1))%>%
    mutate(Type="3SW")
  
  df<-full_join(df_2sw,df_3sw, by=NULL)
  
  df.1<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
    mutate(Year=Year+1986)
  
  df.1<-full_join(df.1,obs, by=NULL)
  
  df.1
  
}

# Model 2: JAGS
# =================

#summary(chains[ ,regexpr("Wprop",varnames(chains))>0])

df_2sw<-boxplot.jags.df2(chains, "Wprop[", "1]", 6:(length(Years)-1))%>%
  mutate(Type="2SW")
df_3sw<-boxplot.jags.df2(chains, "Wprop[", "2]", 6:(length(Years)-1))%>%
  mutate(Type="3SW")

df<-full_join(df_2sw,df_3sw, by=NULL)

df.2<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
  mutate(Year=Year+1986)

df.2<-full_join(df.2,obs, by=NULL)

df.2

# Draw boxplots to compare
# ==========================

## ---- graphs-wprop


df1<-filter(df.1, is.na(Year)==F)
df2<-filter(df.2, is.na(Year)==F)

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
  labs(x="Year", y="proportion", title="Wild proportion")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  geom_point(aes(x=Year, y=obs_prop))+
  facet_grid(Type~.)


## ---- graphs-wprop-traces

par(mfrow=c(1,2))
for(i in 6:length(Years)){
  for(a in 1:2){
    #i<-1
    #a<-1
    gd<-gelman.diag(chains[,str_c("Wprop[",i,",",a,"]")])
    if(gd$psrf[2]>2){
      #print(c(a,i, gd$psrf))
      traceplot(chains[,str_c("Wprop[",i,",",a,"]")], main=str_c("Wprop age=",a," ",df.2$Year[i]))
    }
  }
}




