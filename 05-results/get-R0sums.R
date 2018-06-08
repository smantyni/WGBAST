################################

library(stringr)
library(tidyverse)
library(openxlsx)
library(coda)
library(forcats)


pathData<-"C:/Users/412hpulkkin/Dropbox/WGBAST/JAGS/data_2018/"
Rivername_long<-read.table(str_c(pathData, "rivernames.txt"))[,1]

load(file="H:/FLR/WGBAST18/newSR_final2018-04-22.RData"); modelname<-"final"
#load(file="H:/FLR/WGBAST18/new_SR_HRR2018-03-22.RData");modelname<-"2018" 
#chains<-as.mcmc.list(run1)

#print stats to file
d<-as.matrix(chains)
dim(d)
#[1]   200 16500 # dimensions: iterations x number of variables

d<-d[,grep("R0",colnames(d),fixed=TRUE)]
dim(d)

nyears<-length(c(1987:2018))
nstocks<-16
nareas<-8 # AU1:4, AU1-2, GB (AU1-3), MB (AU4-5), Total (AU1-4)
niter<-dim(d)[1]


R0AU<-array(NA, dim=c(niter,nareas,nyears))
for(y in 1:nyears){  
  tmp<-array(NA, dim=c(niter,nstocks)) # iterations per stock one year at the time
  for(s in 1:nstocks){
    x<-str_c("R0[",y,",",s,"]")   
    tmp[,s]<-d[,grep(x,colnames(d),fixed=TRUE)]
  }
  
  for(i in 1:niter){
    R0AU[i,1,y]<-sum(tmp[i,1:4])
    R0AU[i,2,y]<-sum(tmp[i,5:12])+tmp[i,16]
    R0AU[i,3,y]<-tmp[i,13]
    R0AU[i,4,y]<-sum(tmp[i,14:15])
    R0AU[i,5,y]<-sum(tmp[i,1:12])+tmp[i,16] # AU1-2
    R0AU[i,6,y]<-sum(tmp[i,1:13])+tmp[i,16] # GB (AU1-3)
    R0AU[i,7,y]<-sum(tmp[i,14:15]) # MB (AU4)
    R0AU[i,8,y]<-sum(tmp[i,1:16]) # Tot AU1-4
  }
}

# Unit sums
for(u in 1:nareas){
  for(y in 1:nyears){
    x<-R0AU[,u,y]
    m<-mean(x)
    s<-sd(x)
    cv<-s/m
    q5<-quantile(x,0.05)
    q50<-quantile(x,0.50)
    q95<-quantile(x,0.95)
    mode<-q50/(cv*cv+1)
    PI90<-paste0("'",round(q5,0),"-",round(q95,0))  # change 0 if decimals needed
    
    printtxt<-c(u,y,m,mode,s,cv,q5,q50,q95,PI90)
    if(u==1 & y==1){df<-t(as.data.frame(printtxt))}
    else{df<-rbind(df,t(as.data.frame(printtxt)))}
    
  }
}
colnames(df)<-c("Area","year","mean","mode","sd","cv","q5","q50","q95","90%PI")
df<-as.tibble(df)%>%
  mutate(Area=parse_factor(Area, levels=NULL), year=parse_double(year),
         mean=parse_double(mean),mode=parse_double(mode),sd=parse_double(sd),cv=parse_double(cv),
         q5=parse_double(q5),q50=parse_double(q50), q95=parse_double(q95))%>%
  mutate(Area=fct_recode(Area, "AU1"="1","AU2"="2","AU3"="3","AU4"="4","AU1-2"="5","GB"="6","MB"="7", "AU1-4"="8"))%>%
  mutate(Year=year+1986)

df<-df%>%select(Area,year,Year,mode,q50,mean,q5,q95,`90%PI`)%>%
  filter(Year==2017)

write.xlsx(df,"05-results/stats_R0_AUsums_final.xlsx")

