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

df.bugs<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
  mutate(Year=Year+1986)
df.bugs



# Model 2: JAGS
# =================

#summary(chains[ ,regexpr("Wprop",varnames(chains))>0])

df_2sw<-boxplot.jags.df2(chains, "Wprop[", "1]", 6:(length(Years)-1))%>%
  mutate(Type="2SW")
df_3sw<-boxplot.jags.df2(chains, "Wprop[", "2]", 6:(length(Years)-1))%>%
  mutate(Type="3SW")

df<-full_join(df_2sw,df_3sw, by=NULL)

df.jags<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
  mutate(Year=Year+1986)

df.jags<-full_join(df.jags,obs, by=NULL)

df.jags

# Draw boxplots to compare
# ==========================

## ---- graphs-wprop


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
  labs(x="Year", y="proportion", title="Wild proportion")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  geom_point(aes(x=Year, y=obs_prop))+
  facet_grid(Type~.)
  
  


