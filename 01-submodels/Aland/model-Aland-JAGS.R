

library(rjags)
library(runjags)
load.module("mix")
library(tidyverse)
library(stringr)
#library(ggmcmc)
library(readxl)
library(xlsx)
#library(forcats)
#library(lubridate)
#require(gridExtra)


M1<-"
model{
for (y in 1:8){
for (d in 1:33){
sCPUE[y,d] <- beta[y,d]/sum(beta[y,])
beta[y,d] <- pow(d/n_days, a[y]-1) * pow(1-d/n_days, b[y]-1)
ratio[y,d]<-mu_CPUE[y,d]/(sum(mu_CPUE[y,]) *sCPUE[y,d])
test_a[y,d] ~dnorm(ratio[y,d], tau[y,d])
tau[y,d]~dexp(0.01)
#test_a[y,d] <- 1
## Cumulative probability of salmon that have migrated to the coastal areas up to day d
cdf_CPUE[y,d] <- sum(mu_CPUE[y,1:d])
}
## Parameter transformations for the beta distibution
a[y]<-mu[y]*eta                                       
b[y]<-(1-mu[y])*eta
## Mean and indicator of variance of spawner run                                    
mu[y]<-mean_run[y]/n_days
mean_run[y]<-rmean_run[y]* n_days
rmean_run[y]~dbeta(3,3)
}
eta~dunif(2,10)

for (i in 1:338){
sEffort[i]~dbeta(a_Effort[i], b_Effort[i])
a_Effort[i]<- mu_Effort[year[i],day[i]] * eta_Effort
b_Effort[i]<- (1-mu_Effort[year[i],day[i]]) * eta_Effort

sSoak[i]~dbeta(a_Soak[i], b_Soak[i])
a_Soak[i]<- mu_Soak[year[i],day[i]]* eta_Soak
b_Soak[i]<- (1-mu_Soak[year[i],day[i]]) * eta_Soak

sCatch[i]~dbeta(a_Catch[i], b_Catch[i])
a_Catch[i]<- mu_Catch[year[i],day[i]] * eta_Catch
b_Catch[i]<- (1-mu_Catch[year[i],day[i]]) * eta_Catch
}
for (y in 1:8){
for (d in 1:33){
mu_CPUE[y,d]<- (mu_Catch[y,d]*150)/(mu_Soak[y,d] * 50 * mu_Effort[y,d]* 5)
mu_Catch[y,d]~dbeta(1,10)
mu_Soak[y,d]~dbeta(aaS,bbS)
mu_Effort[y,d]~dbeta(aaE,bbE)
}
}

aaS<-mu_S * eta_S
bbS<- (1-mu_S) * eta_S
aaE<-mu_E * eta_E
bbE<- (1-mu_E) * eta_E
mu_S~dbeta(3,3)
mu_E~dbeta(3,3)
eta_S~dunif(1,50)
eta_E~dunif(1,5)
eta_Catch~dunif(1,150)
eta_Soak~dunif(1,50)
eta_Effort~dunif(1,5)
}"

modelName<-"model1"


Mname<-str_c("01-submodels/Aland/",modelName, ".txt")
cat(M1,file=Mname)

dat<-read_xlsx("01-submodels/Aland/data_mean_run_Åland_FYK_2010-2017.xlsx", range="K2:O339", 
          col_names=c("sCatch", "sEffort", "sSoak", "day", "year"), na="NA")

data<-list(
  sCatch=dat$sCatch,
  sEffort=dat$sEffort,
  sSoak=dat$sSoak,
  day=dat$day,
  year=dat$year,
  n_days=34
)


inits<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
            list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))))


var_names<-c(
  "mu_CPUE")


#nb of samples = samples * thin, burnin doesn't take into account thin
# sample on tässä lopullinen sample, toisin kuin rjagsissa!!!

run <- run.jags(M1, 
                 monitor= var_names,data=data,#inits = inits,
                 n.chains = 2, method = 'parallel', thin=1, burnin =0, 
                 modules = "mix",keep.jags.files=T,sample =1000, adapt = 100, 
                 progress.bar=TRUE)
summary(run)



t1<-Sys.time();t1
run1 <- run.jags(M1, 
                 monitor= var_names,#data=data,inits = inits,
                 n.chains = 2, method = 'parallel', thin=300, burnin =0, 
                 modules = "mix",keep.jags.files=T,sample =1000, adapt = 100, 
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)
# 17h

run<-run1
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run2 <- extend.jags(run1, combine=F, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#2.2d?

run<-run2
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run3 <- extend.jags(run2, combine=T, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#2.2d?
run<-run3
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run4 <- extend.jags(run3, combine=T, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#1.6d
run<-run4
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run5 <- extend.jags(run4, combine=T, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#2.1d

run<-run5
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))


run<-run3

summary(run, var="D")
summary(run, var="P")
summary(run, var="B")
summary(run, var="Ntot")
summary(run, var="eta_alphaN")
summary(run, var="sum")


plot(run, var="D")
plot(run, var="P")
plot(run, var="B")
plot(run, var="Ntot")
plot(run, var="eta_alphaN")
#plot(run, var="sum")

chains<-as.mcmc.list(run)
chains<-window(chains,start=1000000)
#save(run, file="H:/Projects/ISAMA/prg/output/Utsjoki-smolts/Smolts_etaB_0714_run.RData")
save(chains, file="H:/Projects/ISAMA/prg/output/Utsjoki-smolts/Smolts_etaB_0714_chains.RData")


gelman.diag(chains[,"Ntot[1]"])
gelman.diag(chains[,"Ntot[2]"])
gelman.diag(chains[,"Ntot[3]"])
gelman.diag(chains[,"Ntot[4]"])
gelman.diag(chains[,"Ntot[5]"])

gelman.diag(chains[,"aP"])
gelman.diag(chains[,"bP"])
gelman.diag(chains[,"aD"])
gelman.diag(chains[,"cvD"])
gelman.diag(chains[,"cvmuD"])
