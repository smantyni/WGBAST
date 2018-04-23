
# AR(1) model for Mps

# Load input
#source("04-scenarios/Mps_cor.r")

M1<-"
model{
	
  # input from the model
  # Mps_obs: Medians of the instantaneous mortalities 
  # cv_obs: Cv's of the instantaneous mortalities
  for(i in 1:m){
    Mps_obs[i]~dlnorm(logM[i],T_obs[i])
    T_obs[i]<-1/log(cv_obs[i]*cv_obs[i]+1)
  }
  
  # 1st time step
  x[1]~dnorm(0,0.1)
  M[1]<--log(S[1])
  logit(S[1])<-x[1]
  logM[1]<-log(M[1])
  
  # next time steps
  for(i in 2:100){
    x[i]<-x[i-1]*w+e[i] # AR(1)
    e[i]~dnorm(c,tau)
    M[i]<--log(S[i])
    logit(S[i])<-x[i]
    logM[i]<-log(M[i])
  }
  
  means~dbeta(2,2)	# mean
  mu<-log(means/(1-means)) # mean on logit-scale
  c<-mu*(1-w)
  sigma2~dunif(0,1)
  sigma2e<-(1-w*w)*sigma2 # variance of the error term
  tau<-1/sigma2e
  w~dunif(0,1) # autocorrelation coefficient
  
  pred_averageMean<-mean(x[27:30]) #2013-2016, 4 year average without last year which is more uncertain
  #pred_averageLow<-x[19] # 19= 2005
  #pred_averageLast<-x[25] # 25= 2011
  #pred_average<-mu

}"
Mname<-"model_Mps.txt"
cat(M1,file=str_c("04-scenarios/", Mname))

data<-list(
  m=31, # until 2017
  Mps_obs=MpsMed,
  cv_obs=CV
)


var_names<-c(
  "pred_averageMean",
  "sigma2","w"
)

run<-run.jags(str_c("04-scenarios/", Mname),monitor=var_names, data=data, n.chains=2,sample=10000, burnin=5000,
               method='parallel', modules = "mix", keep.jags.files = "test")

plot(run)

summary(run)
#                 Lower95    Median   Upper95       Mean        SD Mode       MCerr MC%ofSD SSeff
#pred_averageMean -2.424480 -2.004730 -1.616730 -2.0091577 0.2066233   NA 0.001654271     0.8 15601
#sigma2            0.324166  0.629022  0.999906  0.6336885 0.1926548   NA 0.004428987     2.3  1892
#w                 0.104036  0.538321  0.871627  0.5180358 0.2110849   NA 0.019421764     9.2   118
#AC.10      psrf
#pred_averageMean 0.01551144 0.9999689
#sigma2           0.12305598 1.0001278
#w                0.88871687 1.0033520

# In Input-file:
# mu<--2.009    # median survival 2010-2013, 14%
# w<-0.52      
# sigma2<-0.634


