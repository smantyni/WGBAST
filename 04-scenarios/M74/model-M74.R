
# Save M74 output (coda files) from Full life history model and 
# read those in 

#summary(chains[ ,regexpr("M74",varnames(chains))>0])$statistics

years<-c(1992:2017)
Nyears<-length(years)
Nstocks<-16

mean_M74<-matrix(nrow=1000,ncol=Nstocks)
sd_M74<-matrix(nrow=1000,ncol=Nstocks)
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
	for(i in 1:38){
		meanx[i]<-mean(x[i,1:Nstocks])
  }
  pred_averageMean[1]<-mean(meanx[1:8]) # 1992:1999
  pred_averageMean[2]<-mean(meanx[1:26]) # 1992:2017

}
"
Mname<-"model_M74.txt"
cat(M1,file=str_c("scenarios/", Mname))

data<-list(
  Nyears=Nyears,
  Nstocks=Nstocks,
  M74_obs=mean_M74,
  sd_obs=sd_M74
)


var_names<-c(
  "pred_averageMean",
  "mu",
  "sigma2","w"
)

run<-run.jags(str_c("scenarios/", Mname),monitor=var_names, data=data, 
              n.chains=2,sample=10000, burnin=10000,
              method='parallel', modules = "mix", keep.jags.files = "test")

plot(run)

summary(run)
summary(run)
#Lower95     Median   Upper95       Mean         SD Mode        MCerr
#pred_averageMean[1] -0.658166 -0.5600045 -0.462239 -0.5600586 0.05021585   NA 0.0006022855
#pred_averageMean[2] -1.989330 -1.8928600 -1.793630 -1.8929764 0.05007603   NA 0.0012889136
#mu                  -2.901210 -2.6194750 -2.344190 -2.6235646 0.14282675   NA 0.0053569838
#sigma2               0.101602  0.2945650  0.541684  0.3048683 0.11184424   NA 0.0068576932
#w                    0.781017  0.8255965  0.864927  0.8242321 0.02200053   NA 0.0013760867
#MC%ofSD SSeff     AC.10     psrf
#pred_averageMean[1]     1.2  6951 0.0387491 1.002317
#pred_averageMean[2]     2.6  1509 0.1523759 1.006246
#mu                      3.8   711 0.3214422 1.004230
#sigma2                  6.1   266 0.7660634 1.026549
#w                       6.3   256 0.7753427 1.018834
# In inputs-file:
## Stable mean on logit scale
#  muM74<- -3.594    

## Marginal variance  on logit scale
#  sigma2M74<-0.490

## AutoC coefficient  on logit scale
#  wM74<- 0.878

