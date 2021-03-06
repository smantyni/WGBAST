# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-catches

# unrep coefs (needed to adjust BUGS but not JAGS)
# =================
coef_r<-c(rep(NA,5),rep(1.24,9), rep(1.22,7), rep(1.23,(length(YearsB)-5)-16))
coef_c<-c(rep(NA,5),rep(1.33,9), rep(1.21,7),rep(1.2,5), rep(1.11,(length(YearsB)-5)-21))
coef_o<-c(rep(NA,5),rep(1.18,9), rep(1.15,7),rep(1.16,5), rep(1.12,(length(YearsB)-5)-21))

cbind(YearsB,coef_r,coef_c,coef_o)

# Catch data (reported catches)
# =================
#tmp<-read_tsv(str_c(pathData, "Catch_incolumns.txt"))
tmp<-read_tsv(str_c(pathData, "Catch_withTrolling.txt"))
colnames(tmp)<-c("river", "coast", "offs")

fix<-0 # or 1

obs_r<-tmp[,1]%>%
  mutate(Type="River", Year=Years[1:(length(Years)-fix)], obs_catch=river)%>%select(-river)
obs_c<-tmp[,2]%>%
  mutate(Type="Coast", Year=Years[1:(length(Years)-fix)], obs_catch=coast)%>%select(-coast)
obs_o<-tmp[,3]%>%
  mutate(Type="Offshore", Year=Years[1:(length(Years)-fix)], obs_catch=offs)%>%select(-offs)

obs<-full_join(obs_r,obs_c, by=NULL)
obs<-full_join(obs,obs_o, by=NULL)

obs_t<-obs%>%group_by(Year)%>%
  summarise(obs_catch=sum(obs_catch))%>%
  mutate(Type="Total")

obs<-full_join(obs, obs_t, by=NULL)

FullPLmisrep<-1
if(FullPLmisrep==1){
tmp<-read_tsv(str_c(pathData, "Catch_withTrolling_FullPLmisrep.txt"))
colnames(tmp)<-c("river", "coast", "offs")

fix<-0 # or 1

obs_r<-tmp[,1]%>%
  mutate(Type="River", Year=Years[1:(length(Years)-fix)], obs_catch=river)%>%select(-river)
obs_c<-tmp[,2]%>%
  mutate(Type="Coast", Year=Years[1:(length(Years)-fix)], obs_catch=coast)%>%select(-coast)
obs_o<-tmp[,3]%>%
  mutate(Type="Offshore", Year=Years[1:(length(Years)-fix)], obs_catch=offs)%>%select(-offs)

obs2<-full_join(obs_r,obs_c, by=NULL)
obs2<-full_join(obs2,obs_o, by=NULL)

obs_t<-obs2%>%group_by(Year)%>%
  summarise(obs_catch=sum(obs_catch))%>%
  mutate(Type="Total")

obs2<-full_join(obs2, obs_t, by=NULL)
}else{
  obs2<-obs
}

#View(obs)

# Model 1: BUGS
# =================
if(compare=="BJ"){
  
  for(y in 6:length(YearsB)){ 
    tempR<-read.table(str_c(folder1,"/ncr_Tot[",y,"]1.txt"))/coef_r[y]
    tempC<-read.table(str_c(folder1,"/ncc_Tot[",y,"]1.txt"))/coef_c[y]
    tempO<-read.table(str_c(folder1,"/nco_Tot[",y,"]1.txt"))/coef_o[y]
    
    ifelse(y==6, catch_r<-tempR[,2], catch_r<-cbind(catch_r,tempR[,2]))
    ifelse(y==6, catch_c<-tempC[,2], catch_c<-cbind(catch_c,tempC[,2]))
    ifelse(y==6, catch_o<-tempO[,2], catch_o<-cbind(catch_o,tempO[,2]))
  }
  #dim(catch_t)
  catch_t<-catch_r+catch_c+catch_o
  
  dfr<-boxplot.bugs.df(catch_r, 6:(length(YearsB)))%>%
    mutate(Type="River")
  dfc<-boxplot.bugs.df(catch_c, 6:(length(YearsB)))%>%
    mutate(Type="Coast")
  dfo<-boxplot.bugs.df(catch_o, 6:(length(YearsB)))%>%
    mutate(Type="Offshore")
  dft<-boxplot.bugs.df(catch_t, 6:(length(YearsB)))%>%
    mutate(Type="Total")
  
  df<-full_join(dfr,dfc, by=NULL)
  df<-full_join(df, dfo, by=NULL)
  df<-full_join(df, dft, by=NULL)
  
  df.1<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
    mutate(Year=Year+1986)
  df.1
}  

if(compare=="JJ"){
  
  catch_tot<-array(NA, dim=c(length(chains1[,"ncr_ObsTotX[1]"][[1]]),length(Years)-0))
  dim(catch_tot)
  for(y in 1:(length(Years)-0)){
    catch_tot[,y]<-chains1[,str_c("ncr_ObsTotX[",y,"]")][[1]]+
      chains1[,str_c("ncc_ObsTotX[",y,"]")][[1]]+
      chains1[,str_c("nco_ObsTotX[",y,"]")][[1]]
  }
  
  
  dfr<-boxplot.jags.df(chains1, "ncr_ObsTotX[", 1:(length(Years)-0))%>%
    mutate(Type="River")
  dfc<-boxplot.jags.df(chains1, "ncc_ObsTotX[", 1:(length(Years)-0))%>%
    mutate(Type="Coast")
  dfo<-boxplot.jags.df(chains1, "nco_ObsTotX[", 1:(length(Years)-0))%>%
    mutate(Type="Offshore")
  dft<-boxplot.bugs.df(catch_tot, 1:(length(Years)-0))%>%
    mutate(Type="Total", x=y)%>%select(-y)
  
  df<-full_join(dfr,dfc,by=NULL)
  df<-full_join(df,dfo,by=NULL)
  df<-full_join(df,dft,by=NULL)
  
  df.1<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
    mutate(Year=Year+1986)
  df.1
  
  df.1<-full_join(df.1,obs,by=NULL)
  
}


# Model 2: JAGS
# =================

#summary(chains[ ,regexpr("ncr_ObsTotX",varnames(chains))>0])


catch_tot<-array(NA, dim=c(length(chains[,"ncr_ObsTotX[1]"][[1]]),length(Years)-0))
dim(catch_tot)
for(y in 1:(length(Years)-0)){
  catch_tot[,y]<-chains[,str_c("ncr_ObsTotX[",y,"]")][[1]]+
    chains[,str_c("ncc_ObsTotX[",y,"]")][[1]]+
    chains[,str_c("nco_ObsTotX[",y,"]")][[1]]
}


dfr<-boxplot.jags.df(chains, "ncr_ObsTotX[", 1:(length(Years)-0))%>%
  mutate(Type="River")
dfc<-boxplot.jags.df(chains, "ncc_ObsTotX[", 1:(length(Years)-0))%>%
  mutate(Type="Coast")
dfo<-boxplot.jags.df(chains, "nco_ObsTotX[", 1:(length(Years)-0))%>%
  mutate(Type="Offshore")
dft<-boxplot.bugs.df(catch_tot, 1:(length(Years)-0))%>%
  mutate(Type="Total", x=y)%>%select(-y)

df<-full_join(dfr,dfc,by=NULL)
df<-full_join(df,dfo,by=NULL)
df<-full_join(df,dft,by=NULL)

df.2<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
  mutate(Year=Year+1986)
df.2

df.2<-full_join(df.2,obs2,by=NULL)



#View(df.2)
#View(df.1)


# Draw boxplots to compare
# ==========================

## ---- graphs-catches

df.1<-filter(df.1, Year>1991)
df.2<-filter(df.2, Year>1991)

for(i in 1:4){
  #i<-4
  if(i==1){ df1<-filter(df.1, Type=="River");df2<-filter(df.2, Type=="River")}
  if(i==2){ df1<-filter(df.1, Type=="Coast");df2<-filter(df.2, Type=="Coast")}
  if(i==3){ df1<-filter(df.1, Type=="Offshore");df2<-filter(df.2, Type=="Offshore")}
  if(i==4){ df1<-filter(df.1, Type=="Total");df2<-filter(df.2, Type=="Total")}
  
plot<-ggplot(df2, aes(Year))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Year", y="Catch (in thousands)", title="")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  geom_point(data=df1,aes(Year,obs_catch), col="red")+
  geom_point(data=df2,aes(Year,obs_catch), col="blue")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_grid(Type~.)

#print(plot)
if(i==1){plot1<-plot}
if(i==2){plot2<-plot}
if(i==3){plot3<-plot}
if(i==4){plot4<-plot}

}

#windows()
#par(mfrow=c(3,1))
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)
