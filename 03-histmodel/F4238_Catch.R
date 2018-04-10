source("05-results/compare-models/models-select.R")


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


# Model estimates
# =================
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

df.2<-full_join(df.2,obs,by=NULL)


## ---- F4238

df.2<-filter(df.2, Year>1991)

for(i in 1:4){
  #i<-4
  if(i==1){ df2<-filter(df.2, Type=="River")}
  if(i==2){ df2<-filter(df.2, Type=="Coast")}
  if(i==3){ df2<-filter(df.2, Type=="Offshore")}
  if(i==4){ df2<-filter(df.2, Type=="Total")}
  
  plot<-ggplot(df2, aes(Year))+
    theme_bw()+
    geom_boxplot(
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",fill=rgb(1,1,1,0.6))+
    labs(x="Year", y="Catch (in thousands)", title="")+
    geom_line(aes(Year,q50))+
    geom_point(aes(Year,obs_catch), col="red")+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    facet_grid(Type~.)
  
  #print(plot)
  if(i==1){plot1<-plot}
  if(i==2){plot2<-plot}
  if(i==3){plot3<-plot}
  if(i==4){plot4<-plot}
  
}

grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)
