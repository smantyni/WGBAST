boxplot.jags.df2<-function(mcmc.chains, name1, name2, X){ # chain object, variable name, values to x-axis
  # note: length of x and dim variable need to match
  
  #mcmc.chains<-chains
  #name1<-"NspWtot["
  #name2<-str_c(r,"]")
  #X<-1:30

  #mcmc.chains<-dsub
  #name1<-"NspWtot["
  #name2<-str_c(r,"]")
  #X<-1:30
  
  Q5<-c();Q25<-c();Q50<-c();Q75<-c();Q95<-c()
  n<-length(X)
  
  for(i in 1:n){
    #i<-6
    #str_c(name1,i,",",name2)
    #y <- as.mcmc(mcmc.chains[,str_c(name1,i,",",name2)]) # if only one chain
    y <- as.mcmc(mcmc.chains[,str_c(name1,i,",",name2)][[1]]) # if several chains
    tmp<-summary(y,quantiles=c(0.05,0.25,0.5,0.75,0.95))
    Q5[i] = tmp$quantiles[1]
    Q25[i] = tmp$quantiles[2]
    Q50[i] = tmp$quantiles[3]
    Q75[i] = tmp$quantiles[4]
    Q95[i] = tmp$quantiles[5]
  }
  
  df<-data.frame(
    x<-X,
    q5=Q5,
    q25=Q25,
    q50=Q50,
    q75=Q75,
    q95=Q95
  )
  colnames(df)<-c("x","q5","q25","q50","q75","q95")
  return(df)
}

boxplot.bugs.df2<-function(param, R, Y){ # variable name, values to x-axis
  # note: length of x and dim variable need to match
  
#param<-Nsp
#  r<-1
#  Y<-1:30
  Q5<-c();Q25<-c();Q50<-c();Q75<-c();Q95<-c()
  n<-length(Y)
  
  for(i in 1:n){
    #i<-1
    
    x<-as.mcmc(param[r,i,])
    if(is.na(x[1])==F){      
      tmp<-summary(x,quantiles=c(0.05,0.25,0.5,0.75,0.95)) 
      Q5[i] = tmp$quantiles[1]
      Q25[i] = tmp$quantiles[2]
      Q50[i] = tmp$quantiles[3]
      Q75[i] = tmp$quantiles[4]
      Q95[i] = tmp$quantiles[5]
    }else{
      Q5[i]<-NA;Q25[i]<-NA;Q50[i]<-NA;Q75[i]<-NA;Q95[i]<-NA}
  }
  
  df<-data.frame(
    y<-Y,
    q5=Q5,
    q25=Q25,
    q50=Q50,
    q75=Q75,
    q95=Q95
  )
  colnames(df)<-c("y","q5","q25","q50","q75","q95")
  return(df)
}


boxplot.bugs.df<-function(param, Y){ # variable name, values to x-axis
  # note: length of x and dim variable need to match
  
  #param<-WSurv
  #  Y<-1:29
  Q5<-c();Q25<-c();Q50<-c();Q75<-c();Q95<-c()
  n<-length(Y)
  
  for(i in 1:n){
    #i<-1
    
    x<-as.mcmc(param[,i])
    if(is.na(x[1])==F){      
      tmp<-summary(x,quantiles=c(0.05,0.25,0.5,0.75,0.95)) 
      Q5[i] = tmp$quantiles[1]
      Q25[i] = tmp$quantiles[2]
      Q50[i] = tmp$quantiles[3]
      Q75[i] = tmp$quantiles[4]
      Q95[i] = tmp$quantiles[5]
    }else{
      Q5[i]<-NA;Q25[i]<-NA;Q50[i]<-NA;Q75[i]<-NA;Q95[i]<-NA}
  }
  
  df<-data.frame(
    y<-Y,
    q5=Q5,
    q25=Q25,
    q50=Q50,
    q75=Q75,
    q95=Q95
  )
  colnames(df)<-c("y","q5","q25","q50","q75","q95")
  return(df)
}


boxplot.jags.df<-function(mcmc.chains, name1, X){ # chain object, variable name, values to x-axis
  # note: length of x and dim variable need to match
  
  #mcmc.chains<-chains
  #name1<-"survMpsW["
  #X<-1:30
  Q5<-c();Q25<-c();Q50<-c();Q75<-c();Q95<-c()
  n<-length(X)
  
  for(i in 1:n){
    #i<-1
    #str_c(name1,i,",",name2)
    y <- as.mcmc(mcmc.chains[,str_c(name1,i,"]")][[1]])
    tmp<-summary(y,quantiles=c(0.05,0.25,0.5,0.75,0.95))
    Q5[i] = tmp$quantiles[1]
    Q25[i] = tmp$quantiles[2]
    Q50[i] = tmp$quantiles[3]
    Q75[i] = tmp$quantiles[4]
    Q95[i] = tmp$quantiles[5]
  }
  
  df<-data.frame(
    x<-X,
    q5=Q5,
    q25=Q25,
    q50=Q50,
    q75=Q75,
    q95=Q95
  )
  colnames(df)<-c("x","q5","q25","q50","q75","q95")
  return(df)
}
