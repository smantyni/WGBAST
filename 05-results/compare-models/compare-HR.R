# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-HR

# Model 1: BUGS
# =================
if(compare=="BJ"){
  
  # input
  ##########################
  HrW<-array(NA, dim=c(6,length(YearsB)-1, 1000))
  HrR<-array(NA, dim=c(6,length(YearsB)-1, 1000))
  HcW<-array(NA, dim=c(6,3,length(YearsB)-1, 1000))
  HcR<-array(NA, dim=c(6,3,length(YearsB)-1, 1000))
  for(i in 1:(length(YearsB)-1)){
    for(a in 1:6){
      HrW[a,i,]<-read.table(paste(sep="", folder1,"/HrW[",i,",",a,"]1.txt"))[,2]
      HrR[a,i,]<-read.table(paste(sep="", folder1,"/HrR[",i,",",a,"]1.txt"))[,2]
  
      for(u in 1:3){
        HcW[a,u,i,]<-read.table(paste(sep="", folder1,"/HcW[",i,",",a,",",u,"]1.txt"))[,2]
        HcR[a,u,i,]<-read.table(paste(sep="", folder1,"/HcR[",i,",",a,",",u,"]1.txt"))[,2]
  
        }
  }}
  
  HdcW<-array(NA, dim=c(5,length(YearsB)-1, 1000))
  HdcR<-array(NA, dim=c(5,length(YearsB)-1, 1000))
  HdoW<-array(NA, dim=c(5,length(YearsB)-1, 1000))
  HdoR<-array(NA, dim=c(5,length(YearsB)-1, 1000))
  HlW<-array(NA, dim=c(5,length(YearsB)-1, 1000))
  HlR<-array(NA, dim=c(5,length(YearsB)-1, 1000))
  for(i in 1:(length(YearsB)-1)){
    for(a in 1:5){
      HdcW[a,i,]<-read.table(paste(sep="", folder1,"/HdcW[",i,",",a,"]1.txt"))[,2]
      HdcR[a,i,]<-read.table(paste(sep="", folder1,"/HdcR[",i,",",a,"]1.txt"))[,2]
      HdoW[a,i,]<-read.table(paste(sep="", folder1,"/HdoW[",i,",",a,"]1.txt"))[,2]
      HdoR[a,i,]<-read.table(paste(sep="", folder1,"/HdoR[",i,",",a,"]1.txt"))[,2]
      HlW[a,i,]<-read.table(paste(sep="", folder1,"/HlW[",i,",",a,"]1.txt"))[,2]
      HlR[a,i,]<-read.table(paste(sep="", folder1,"/HlR[",i,",",a,"]1.txt"))[,2]
    }
  }
  
  HoffsW<-array(NA, dim=c(5,(length(YearsB)-1),1000))
  HoffsR<-array(NA, dim=c(5,(length(YearsB)-1),1000))
  for(a in 1:5){
    for(i in 1:(length(YearsB)-1)){
      for(s in 1:1000){
        HoffsW[a,i,s]<-(1-((1-HdoW[a,i,s])*(1-HlW[a,i,s])))
        HoffsR[a,i,s]<-(1-((1-HdoR[a,i,s])*(1-HlR[a,i,s])))
      }
    }
  }
  
  # from cohort+age to calendar years
  hrW<-array(NA, dim=c(2,length(YearsB)-2, 1000))
  hrR<-array(NA, dim=c(2,length(YearsB)-2, 1000))
  hcW.au1<-array(NA, dim=c(2,length(YearsB)-2, 1000))
  hcR.au1<-array(NA, dim=c(2,length(YearsB)-2, 1000))
  for(y in 3:(length(YearsB))){ 
    for(a in 2:3){ # Grilse & 2SW (=MSW)
      #hrW[grilse:MSW, ]
      hrW[a-1,y-2,]<-HrW[a,y-(a-1),]
      hrR[a-1,y-2,]<-HrR[a,y-(a-1),]
      
      #hcW[grilse:MSW, ] AU1
      hcW.au1[a-1,y-2,]<-HcW[a,1,y-(a-1),]
      hcR.au1[a-1,y-2,]<-HcR[a,1,y-(a-1),]
    }}
  
  hdcW<-array(NA, dim=c(2,length(YearsB)-2, 1000))
  hdcR<-array(NA, dim=c(2,length(YearsB)-2, 1000))
  hdoW<-array(NA, dim=c(2,length(YearsB)-2, 1000))
  hdoR<-array(NA, dim=c(2,length(YearsB)-2, 1000))
  hlW<-array(NA, dim=c(2,length(YearsB)-2, 1000))
  hlR<-array(NA, dim=c(2,length(YearsB)-2, 1000))
  for(y in 3:(length(YearsB))){ 
    for(a in 1:2){ # Grilse & 2SW (=MSW)
      #hdcW[grilse:MSW, ]
      hdcW[a,y-2,]<-HdcW[a,y-a,]
      hdcR[a,y-2,]<-HdcR[a,y-a,]
      
      #hdoW[grilse:MSW, ]
      hdoW[a,y-2,]<-HdoW[a,y-a,]
      hdoR[a,y-2,]<-HdoR[a,y-a,]
    
      #hlW[grilse:MSW, ]
      hlW[a,y-2,]<-HlW[a,y-a,]
      hlR[a,y-2,]<-HlR[a,y-a,]
    }}
  
  
  # wrangle
  ##########################
  # RIVER FISHERIES
  #for(a in 1:6){
  #  dfW<-boxplot.bugs.df2(HrW, a ,1:(length(YearsB)-1))%>%
  for(a in 1:2){ # grilse vs msw
    dfW<-boxplot.bugs.df2(hrW, a ,1:(length(YearsB)-2))%>%
      mutate(age=a, Fishery="River", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
    dfR<-boxplot.bugs.df2(hrR, a ,1:(length(YearsB)-2))%>%
      mutate(age=a, Fishery="River", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hr<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
  #  mutate(Year=Year+1986)%>%
    mutate(Year=Year+1993)%>%
    mutate(Age=fct_recode(factor(Age),
                          "Grilse"= "1","MSW"= "2"))                        
  #                        "PS"= "1","Grilse"= "2",
  #                        "2SW"= "3","3SW"= "4",
  #                        "4SW"= "5","5SW"= "6"))
  df.1.Hr
  
  # COASTAL FISHERIES (TRAPNET&GILLNET)
  for(a in 1:2){
  #  for(u in 1:3){
  #    dfW<-boxplot.bugs.df3(HcW, a, u, 1:(length(YearsB)-1))%>%
  #      mutate(age=a, Fishery="Coast", Type="Wild", AU=u)
  #    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  #  a<-1
       dfW<-boxplot.bugs.df2(hcW.au1, a, 1:(length(YearsB)-2))%>%
          mutate(age=a, Fishery="Coast", Type="Wild")
        ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
      dfR<-boxplot.bugs.df2(hcR.au1, a, 1:(length(YearsB)-2))%>%
        mutate(age=a, Fishery="Coast", Type="Reared")
      ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type"#, "AU"
                                      )))%>%
    select(Age, Fishery, Type, #AU, 
           everything())%>%
    mutate(Year=Year+1988)%>%
    mutate(Age=fct_recode(factor(Age),
                          "Grilse"= "1","MSW"= "2"))                        
  df.1.Hc
  
  
  # COASTAL DRIFTNET
  for(a in 1:2){
    dfW<-boxplot.bugs.df2(hdcW, a ,1:(length(YearsB)-2))%>%
      mutate(age=a, Fishery="CDN", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hdcR, a ,1:(length(YearsB)-2))%>%
      mutate(age=a, Fishery="CDN", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hdc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
    mutate(Year=Year+1988)%>%
    mutate(Age=fct_recode(factor(Age),
                          "Grilse"="1", "MSW"="2"))
  df.1.Hdc
  
  # OFFSHORE DRIFTNET
  for(a in 1:2){
    dfW<-boxplot.bugs.df2(hdoW, a ,1:(length(YearsB)-2))%>%
      mutate(age=a, Fishery="ODN", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hdoR, a ,1:(length(YearsB)-2))%>%
      mutate(age=a, Fishery="ODN", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hdo<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
    mutate(Year=Year+1988)%>%
    mutate(Age=fct_recode(factor(Age),
                          "1SW"="1",
                          "MSW"= "2"))
  df.1.Hdo
  
  # OFFSHORE LONGLINE
  for(a in 1:2){
    dfW<-boxplot.bugs.df2(hlW, a ,1:(length(YearsB)-2))%>%
      mutate(age=a, Fishery="OLL", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hlR, a ,1:(length(YearsB)-2))%>%
      mutate(age=a, Fishery="OLL", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hl<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
    mutate(Year=Year+1988)%>%
    mutate(Age=fct_recode(factor(Age),
                          "1SW"="1",
                          "MSW"= "2"))
  df.1.Hl
}

if(compare=="JJ"){
  
  nsample<-length(chains1[,1][[1]])
  
  # from cohort+age to calendar years
  hrW<-array(NA, dim=c(2,length(Years)-2, nsample))
  hrR<-array(NA, dim=c(2,length(Years)-2, nsample))
  hcW.au1<-array(NA, dim=c(2,length(Years)-2, nsample))
  hcR.au1<-array(NA, dim=c(2,length(Years)-2, nsample))
  hdcW<-array(NA, dim=c(2,length(Years)-2, nsample))
  hdcR<-array(NA, dim=c(2,length(Years)-2, nsample))
  for(y in 3:(length(Years))){ 
    for(a in 2:3){ # Grilse & 2SW (=MSW)
      #hrW[grilse:MSW, ]
      hrW[a-1,y-2,]<-chains1[,str_c("HrW[",y-(a-1),",",a,"]")][[1]]
      hrR[a-1,y-2,]<-chains1[,str_c("HrR[",y-(a-1),",",a,"]")][[1]]
      
      #hcW[grilse:MSW, ] AU1
      hcW.au1[a-1,y-2,]<-chains1[,str_c("HcW[",y-(a-1),",",a,",1]")][[1]]
      hcR.au1[a-1,y-2,]<-chains1[,str_c("HcR[",y-(a-1),",",a,",1]")][[1]]
      
      #hdcW[grilse:MSW, ]
      hdcW[a-1,y-2,]<-chains1[,str_c("HdcW[",y-(a-1),",",a,"]")][[1]]
      hdcR[a-1,y-2,]<-chains1[,str_c("HdcR[",y-(a-1),",",a,"]")][[1]]
      
    }}
  
  hdoW<-array(NA, dim=c(2,length(Years)-2, nsample))
  hdoR<-array(NA, dim=c(2,length(Years)-2, nsample))
  hlW<-array(NA, dim=c(2,length(Years)-2, nsample))
  hlR<-array(NA, dim=c(2,length(Years)-2, nsample))
  for(y in 3:(length(Years))){ 
    for(a in 1:2){ # Grilse & 2SW (=MSW)
      
      #hdoW[grilse:MSW, ]
      hdoW[a,y-2,]<-chains1[,str_c("HdoW[",y-a,",",a,"]")][[1]]
      hdoR[a,y-2,]<-chains1[,str_c("HdoR[",y-a,",",a,"]")][[1]]
      
      #hlW[grilse:MSW, ]
      hlW[a,y-2,]<-chains1[,str_c("HlW[",y-a,",",a,"]")][[1]]
      hlR[a,y-2,]<-chains1[,str_c("HlR[",y-a,",",a,"]")][[1]]
    }}
  
  
  # wrangle
  ##########################
  
  # River fishery
  
  # boxplot.bugs.df2 is used since chains1-structure is not anymore in hrW
  for(a in 1:2){ # grilse vs msw
    dfW<-boxplot.bugs.df2(hrW, a ,1:(length(Years)-2))%>%
      mutate(age=a, Fishery="River", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hrR, a ,1:(length(Years)-2))%>%
      mutate(age=a, Fishery="River", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hr<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
    mutate(Year=Year+1993)%>%
    mutate(Age=fct_recode(factor(Age),
                          "Grilse"= "1","MSW"= "2"))                        
  df.1.Hr
  
  
  # COASTAL FISHERIES (TRAPNET&GILLNET)
  
  # boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
  for(a in 1:2){
    dfW<-boxplot.bugs.df2(hcW.au1, a, 1:(length(Years)-2))%>%
      mutate(age=a, Fishery="Coast", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hcR.au1, a, 1:(length(Years)-2))%>%
      mutate(age=a, Fishery="Coast", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type,
           everything())%>%
    mutate(Year=Year+1988)%>%
    mutate(Age=fct_recode(factor(Age),
                          "Grilse"= "1","MSW"= "2"))                        
  
  df.1.Hc
  
  
  # COASTAL DRIFTNET
  
  # boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
  for(a in 1:2){
    dfW<-boxplot.bugs.df2(hdcW, a ,1:(length(Years)-2))%>%
      mutate(age=a, Fishery="CDN", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hdcR, a ,1:(length(Years)-2))%>%
      mutate(age=a, Fishery="CDN", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hdc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
    mutate(Year=Year+1988)%>%
    mutate(Age=fct_recode(factor(Age),
                          "Grilse"="1", "MSW"="2"))
  df.1.Hdc
  
  
  # OFFSHORE DRIFTNET
  
  # boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
  for(a in 1:2){
    dfW<-boxplot.bugs.df2(hdoW, a ,1:(length(Years)-2))%>%
      mutate(age=a, Fishery="ODN", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hdoR, a ,1:(length(Years)-2))%>%
      mutate(age=a, Fishery="ODN", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hdo<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
    mutate(Year=Year+1988)%>%
    mutate(Age=fct_recode(factor(Age),
                          "1SW"="1",
                          "MSW"= "2"))
  df.1.Hdo
  
  # OFFSHORE LONGLINE
  
  # boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
  for(a in 1:2){
    dfW<-boxplot.bugs.df2(hlW, a ,1:(length(Years)-2))%>%
      mutate(age=a, Fishery="OLL", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hlR, a ,1:(length(Years)-2))%>%
      mutate(age=a, Fishery="OLL", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hl<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
    mutate(Year=Year+1988)%>%
    mutate(Age=fct_recode(factor(Age),
                          "1SW"="1",
                          "MSW"= "2"))
  df.1.Hl
}

# OFFSHORE COMBINED
# missing for now


# Model 2: JAGS
# =================
#
#summary(chains[ ,regexpr("HrW",varnames(chains))>0])
#length(chains[, "HrW[1,1]"][[1]])
#summary(chains[ ,regexpr("HcW",varnames(chains))>0])
#length(chains[, "HcW[1,1]"][[1]])
#summary(chains[ ,regexpr("HdcW",varnames(chains))>0])
#summary(chains[ ,regexpr("HdoW",varnames(chains))>0])
#summary(chains[ ,regexpr("HlW",varnames(chains))>0])

nsample<-length(chains[,1][[1]])


# from cohort+age to calendar years
hrW<-array(NA, dim=c(2,length(Years)-2, nsample))
hrR<-array(NA, dim=c(2,length(Years)-2, nsample))
hcW.au1<-array(NA, dim=c(2,length(Years)-2, nsample))
hcR.au1<-array(NA, dim=c(2,length(Years)-2, nsample))
hdcW<-array(NA, dim=c(2,length(Years)-2, nsample))
hdcR<-array(NA, dim=c(2,length(Years)-2, nsample))
for(y in 3:(length(Years))){ 
  #y<-3
  #a<-2
  for(a in 2:3){ # Grilse & 2SW (=MSW)
    #hrW[grilse:MSW, ]
    hrW[a-1,y-2,]<-chains[,str_c("HrW[",y-(a-1),",",a,"]")][[1]]
    hrR[a-1,y-2,]<-chains[,str_c("HrR[",y-(a-1),",",a,"]")][[1]]
    
    #hcW[grilse:MSW, ] AU1
    hcW.au1[a-1,y-2,]<-chains[,str_c("HcW[",y-(a-1),",",a,",1]")][[1]]
    hcR.au1[a-1,y-2,]<-chains[,str_c("HcR[",y-(a-1),",",a,",1]")][[1]]

    #hdcW[grilse:MSW, ]
    hdcW[a-1,y-2,]<-chains[,str_c("HdcW[",y-(a-1),",",a,"]")][[1]]
    hdcR[a-1,y-2,]<-chains[,str_c("HdcR[",y-(a-1),",",a,"]")][[1]]
    
  }
  }

hdoW<-array(NA, dim=c(2,length(Years)-2, nsample))
hdoR<-array(NA, dim=c(2,length(Years)-2, nsample))
hlW<-array(NA, dim=c(2,length(Years)-2, nsample))
hlR<-array(NA, dim=c(2,length(Years)-2, nsample))
for(y in 3:(length(Years))){ 
  for(a in 1:2){ # Grilse & 2SW (=MSW)
    
    #hdoW[grilse:MSW, ]
    hdoW[a,y-2,]<-chains[,str_c("HdoW[",y-a,",",a,"]")][[1]]
    hdoR[a,y-2,]<-chains[,str_c("HdoR[",y-a,",",a,"]")][[1]]
    
    #hlW[grilse:MSW, ]
    hlW[a,y-2,]<-chains[,str_c("HlW[",y-a,",",a,"]")][[1]]
    hlR[a,y-2,]<-chains[,str_c("HlR[",y-a,",",a,"]")][[1]]
  }}


# wrangle
##########################

# River fishery

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){ # grilse vs msw
  dfW<-boxplot.bugs.df2(hrW, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="River", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hrR, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="River", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hr<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1993)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1","MSW"= "2"))                        
df.2.Hr


# COASTAL FISHERIES (TRAPNET&GILLNET)

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hcW.au1, a, 1:(length(Years)-2))%>%
    mutate(age=a, Fishery="Coast", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hcR.au1, a, 1:(length(Years)-2))%>%
    mutate(age=a, Fishery="Coast", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type,
         everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1","MSW"= "2"))                        

df.2.Hc


# COASTAL DRIFTNET

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hdcW, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="CDN", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hdcR, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="CDN", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hdc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"="1", "MSW"="2"))
df.2.Hdc


# OFFSHORE DRIFTNET

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hdoW, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="ODN", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hdoR, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="ODN", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hdo<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"="1",
                        "MSW"= "2"))
df.2.Hdo

# OFFSHORE LONGLINE

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hlW, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="OLL", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hlR, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="OLL", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hl<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"="1",
                        "MSW"= "2"))
df.2.Hl


# Draw boxplots to compare
# ==========================

## ---- graphs-Hdo
df1<-filter(df.1.Hdo, Type=="Wild")
df2<-filter(df.2.Hdo, Type=="Wild")

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
  geom_line(data=df1,aes(Year,q50),colour="grey")+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore driftnet HR wild"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age, scales="free") 


df1<-filter(df.1.Hdo, Type=="Reared")
df2<-filter(df.2.Hdo, Type=="Reared")

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
  geom_line(data=df1,aes(Year,q50),colour="grey")+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore driftnet HR reared"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age, scales="free") 



## ---- graphs-Hl

df1<-filter(df.1.Hl, Type=="Wild")
df2<-filter(df.2.Hl, Type=="Wild")

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
  geom_line(data=df1,aes(Year,q50),colour="grey")+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore longline HR wild"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age, scales="free") 


df1<-filter(df.1.Hl, Type=="Reared")
df2<-filter(df.2.Hl, Type=="Reared")

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
  geom_line(data=df1,aes(Year,q50),colour="grey")+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore longline HR reared"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age, scales="free") 




## ---- graphs-Hc

# coastal TN & GN

df1<-filter(df.1.Hc, Type=="Wild", Year<2017)
df2<-filter(df.2.Hc, Type=="Wild")

ggplot(df1, aes(Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    data=df2,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="Coastal TN & GN, wild AU1")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age)

df1<-filter(df.1.Hc, Type=="Reared", Year<2017)
df2<-filter(df.2.Hc, Type=="Reared")

ggplot(df1, aes(Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
    geom_boxplot(
      data=df2,
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="Coastal TN & GN, reared AU1")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age)
#  facet_grid(AU~Age)


## ---- graphs-Hdc

# coastal DN
df1<-filter(df.1.Hdc, Type=="Wild")
df2<-filter(df.2.Hdc, Type=="Wild")

ggplot(df1, aes(Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    data=df2,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="Coastal DN, wild")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age)

df1<-filter(df.1.Hdc, Type=="Reared")
df2<-filter(df.2.Hdc, Type=="Reared")

ggplot(df1, aes(Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
    geom_boxplot(
      data=df2,
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="Coastal DN, reared")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age)

## ---- graphs-Hr

# river wild
df1<-filter(df.1.Hr, Type=="Wild", Year<2019)
df2<-filter(df.2.Hr, Type=="Wild")

ggplot(df1, aes(Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
    geom_boxplot(
      data=df2,
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="River, wild")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)

# river reared
df1<-filter(df.1.Hr, Type=="Reared", Year<2019)
df2<-filter(df.2.Hr, Type=="Reared")

ggplot(df1, aes(Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    data=df2,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="River, reared")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)

