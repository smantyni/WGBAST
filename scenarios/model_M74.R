
# Save M74 output (coda files) from Full life history model and 
# read those in 

#summary(chains[ ,regexpr("M74",varnames(chains))>0])$statistics

years<-c(1992:2015)
Nyears<-length(years)
Nstocks<-16

mean_M74<-matrix(nrow=N,ncol=Nstocks)
sd_M74<-matrix(nrow=N,ncol=Nstocks)
for(i in 1:Nyears){  
  for(r in 1:Nstocks){
    M74<-chains[,str_c("M74[",i+5,",",r,"]")][[1]]
    #length(M74)  
    logit_M74<-log(M74/(1-M74))
    mean_M74[i,r]<-mean(logit_M74)
    sd_M74[i,r]<-sd(logit_M74)
  }
}


M1<-"
model{
  means~dbeta(2,2)
  mu<-log(means/(1-means))	
  c<-mu*(1-w)
  sigma2~dunif(0,10)
  sigma2e<-(1-w*w)*sigma2
  tau<-1/sigma2e
  w~dunif(0,1)
  
  for(r in 1:Nstocks){
    x[1,r]~dnorm(0,0.001)

    for(i in 2:38){
      x[i,r]<-x[i-1,r]*w+e[i,r]
      e[i,r]~dnorm(c,tau)
      logit(S[i,r])<-x[i,r]
    }

    for(i in 1:Nyears){ #1992->  :history
      M74_obs[i,r]~dnorm(x[i,r],T_obs[i,r])
      T_obs[i,r]<-1/pow(sd_obs[i,r],2)
    }
  }
}
"
Mname<-"model_M74.txt"
cat(M1,file=str_c("scenarios/", Mname))

data<-list(
  Nyears=Nyears, # 2015
  Nstocks=Nstocks,
  M74_obs=mean_M74,
  sd_obs=sd_M74
)


var_names<-c(
  "mu",
  "sigma2","w"
)

run<-run.jags(str_c("scenarios/", Mname),monitor=var_names, data=data, n.chains=2,sample=10000, burnin=5000,
              method='rjparallel', modules = "mix", keep.jags.files = "test")

plot(run)

summary(run)
#         Lower95     Median   Upper95       Mean         SD Mode       MCerr MC%ofSD SSeff     AC.10     psrf
#mu     -4.370330 -3.5578050 -2.940640 -3.5935558 0.37390887   NA 0.020649036     5.5   328 0.6224519 1.041951
#sigma2  0.166778  0.4671340  0.872728  0.4904877 0.18426125   NA 0.011082341     6.0   276 0.7539614 1.012993
#w       0.832631  0.8803145  0.920131  0.8783066 0.02288808   NA 0.001617347     7.1   200 0.8212090 1.057104

# In inputs-file:
## Stable mean on logit scale
#  muM74<- -3.594    

## Marginal variance  on logit scale
#  sigma2M74<-0.490

## AutoC coefficient  on logit scale
#  wM74<- 0.878

