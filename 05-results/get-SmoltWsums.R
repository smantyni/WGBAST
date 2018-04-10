################################

library(stringr)
library(tidyverse)
library(forcats)
library(openxlsx)
library(xlsx)


# Load simulation results
load(file="H:/FLR/WGBAST18/new_SR_HRR2018-03-22.RData");modelname<-"2018" 
#chains<-as.mcmc.list(run1)

# Read extra stuff (AU5-6 etc.)
pathData<-"C:/Users/412hpulkkin/Dropbox/WGBAST/JAGS/data_2018/"
extra<-as.tibble(read.xlsx(str_c(pathData, "SmoltWW_extra.xlsx"), sheetIndex=1))%>%
  mutate(Year=c(1:32))

#print stats to file
d<-as.matrix(chains)
dim(d)
#[1]   200 16500 # dimensions: iterations x number of variables

d<-d[,grep("SmoltWW",colnames(d),fixed=TRUE)]
dim(d)

nyears<-length(c(1987:2020))
nstocks<-16
nareas<-8 # AU1:4, AU1-2, GB (AU1-3), MB (AU4-5), Total (AU1-6)
niter<-dim(d)[1]


smoltsAU<-array(NA, dim=c(niter,nareas,nyears))
for(y in 1:nyears){  
  tmp<-array(NA, dim=c(niter,nstocks)) # iterations per stock one year at the time
  for(s in 1:nstocks){
    x<-str_c("SmoltWW[",y,",",s,"]")   
    tmp[,s]<-d[,grep(x,colnames(d),fixed=TRUE)]
  }
  for(i in 1:niter){
    smoltsAU[i,1,y]<-sum(tmp[i,1:4]) # AU 1
    smoltsAU[i,2,y]<-sum(tmp[i,5:12])+tmp[i,16] # AU 2
    smoltsAU[i,3,y]<-tmp[i,13] # AU 3, add Testeboån
    smoltsAU[i,4,y]<-sum(tmp[i,14:15]) # AU 4
    smoltsAU[i,5,y]<-sum(tmp[i,1:12])+tmp[i,16] # AU 1-2
    smoltsAU[i,6,y]<-sum(tmp[i,1:13])+tmp[i,16] # Gulf of Bothnia, add Testeboån (AU1-3)
    smoltsAU[i,7,y]<-sum(tmp[i,14:15]) # MB (Add AU 5)
    smoltsAU[i,8,y]<-sum(tmp[i,1:16]) # Total AU1-6 (Add AU5-6 & Testeboån)
  }
}

# "no" or "yes" depending on what is desired.'Extra' are not added for HELCOM output, for example
# Note that if 'Extra' is added, mean, sd & cv cannot be calculated
inclExtra<-"no" 

for(u in 1:nareas){
  for(y in 1:nyears){
    x<-smoltsAU[,u,y]
    
    if(inclExtra=="no"){
      m<-mean(x)
      s<-sd(x)
      cv<-s/m
      q5<-quantile(x,0.05)
      q50<-quantile(x,0.50)
      q95<-quantile(x,0.95)
      mode<-q50/(cv*cv+1)
      PI90<-paste0("'",round(q5,0),"-",round(q95,0))  # change 0 if decimals needed
    
      printtxt<-c(u,y,m,mode,s,cv,q5,q50,q95,PI90)
      
    }
    
    if(inclExtra=="yes"){
        if(u==1 | u==2 | u==4 | u==5){ #AU 1,2 & 4, AU1-2 
        q5<-quantile(x,0.05)
        q50<-quantile(x,0.50)
        q95<-quantile(x,0.95)
        PI90<-paste0("'",round(q5,0),"-",round(q95,0))  # change 0 if decimals needed
      }
      if(u==3| u==6){ # AU 3 &GoB (AU1-3)
        q5<-quantile(x,0.05)+ifelse(is.na(extra$testeb_low[y])==F,extra$testeb_low[y],0)
        q50<-quantile(x,0.50)+ifelse(is.na(extra$testeb_med[y])==F,extra$testeb_med[y],0)
        q95<-quantile(x,0.95)+ifelse(is.na(extra$testeb_high[y])==F,extra$testeb_high[y],0)
        PI90<-paste0("'",round(q5,0),"-",round(q95,0))  # change 0 if decimals needed
      }
      
      if(u==7){ # MB (AU4-5)
        q5<-quantile(x,0.05)+ifelse(is.na(extra$AU5[y])==F,extra$AU5[y],0)
        q50<-quantile(x,0.50)+ifelse(is.na(extra$AU5[y])==F,extra$AU5[y],0)
        q95<-quantile(x,0.95)+ifelse(is.na(extra$AU5[y])==F,extra$AU5[y],0)
        PI90<-paste0("'",round(q5,0),"-",round(q95,0))  # change 0 if decimals needed
      }
      if(u==8){ # Total (AU1-6)
        q5<-quantile(x,0.05)+ifelse(is.na(extra$testeb_low[y])==F,extra$testeb_low[y],0)+
          ifelse(is.na(extra$AU5[y])==F,extra$AU5[y],0)+ifelse(is.na(extra$AU6[y])==F,extra$AU6[y],0)
        q50<-quantile(x,0.50)+ifelse(is.na(extra$testeb_med[y])==F,extra$testeb_med[y],0)+
          ifelse(is.na(extra$AU5[y])==F,extra$AU5[y],0)+ifelse(is.na(extra$AU6[y])==F,extra$AU6[y],0)
        q95<-quantile(x,0.95)+ifelse(is.na(extra$testeb_high[y])==F,extra$testeb_high[y],0)+
          ifelse(is.na(extra$AU5[y])==F,extra$AU5[y],0)+ifelse(is.na(extra$AU6[y])==F,extra$AU6[y],0)
        PI90<-paste0("'",round(q5,0),"-",round(q95,0))  # change 0 if decimals needed
      }
      
      printtxt<-c(u,y,q5,q50,q95,PI90)
    }
    if(u==1 & y==1){df<-t(as.data.frame(printtxt))}
    else{df<-rbind(df,t(as.data.frame(printtxt)))}
    
  }
}

if(inclExtra=="no"){
  colnames(df)<-c("Area","year","mean","mode","sd","cv","q5","q50","q95","90%PI")
  df<-as.tibble(df)%>%
    mutate(Area=parse_factor(Area, levels=NULL), year=parse_double(year),
           mean=parse_double(mean),mode=parse_double(mode),sd=parse_double(sd),cv=parse_double(cv),
           q5=parse_double(q5),q50=parse_double(q50), q95=parse_double(q95))
}else{
  colnames(df)<-c("Area","year","q5","q50","q95","90%PI")
  df<-as.tibble(df)%>%
    mutate(Area=parse_factor(Area, levels=NULL), year=parse_double(year),
           q5=parse_double(q5),q50=parse_double(q50), q95=parse_double(q95))
}
df<-df%>%mutate(Area=fct_recode(Area, "AU1"="1","AU2"="2","AU3"="3","AU4"="4","AU1-2"="5","GB"="6","MB"="7", "Tot"="8"))%>%
  mutate(Year=year+1986)

#df<-df%>%select(Area,year, q50,`90%PI`)
df<-df%>%select(Area,Year, mean,mode, sd, cv,q5,q95, q50,`90%PI`)

write.xlsx(df,"05-results/stats_smoltsWW_AUsums.xlsx")


