#######################
## Analyse SST data from 9 stations with JAGS
##
#######################
library(runjags)

SSTmodel<-"
model{
  for(i in 1:N){ 
    SST[i]~dnorm(mu[year[i],month[i]], tau[year[i],month[i]])
  }
  for(y in 1:Nyears){
    mu[y,1]~dnorm(mu_mu, tau_mu)
    mu[y,2]<-a[1]+b[1]*mu[y,1]
    mu[y,3]<-a[2]+b[2]*mu[y,2]	
    mu[y,4]<-a[3]+b[3]*mu[y,3]	
    cv[y]~dunif(0.001,5) # variation over stations

    for(m in 1:4){
      tau[y,m]<-1/(mu[y,m]*mu[y,m]*cv[y]*cv[y])
    }
  }
  
  mu_mu~dunif(-5,15)
  tau_mu<-1/(mu_mu*mu_mu*cv_mu*cv_mu)
  cv_mu~dunif(0.001,5) # variation over years
  
  for(i in 1:3){ # Feb, March & April effects
    a[i]~dunif(-10,10)
    b[i]~dunif(0,100)
  }


}"

data<-list(
  N=length(df.jags$sst), # Number of observations, 764
  Nyears=27, # Number of years, 27 = years 1992-2018 (last year as NA-> predicted dist for years without data)
  SST=df.jags$sst,
  month=df.jags$Month,
  year=df.jags$year
)

write_csv(df.jags, path="data.bugs.csv")

var_names<-c("a","b",
             "mu_mu", "cv_mu",
             "mu")

t1<-Sys.time()
run1 <- run.jags(SSTmodel, 
                 monitor= c(var_names),data=data,#initlist = inits,
                 n.chains = 2, method = 'rjparallel', thin=100, burnin =1000, modules = "mix",
                 keep.jags.files=F,
                 sample =1000, adapt = 1000, progress.bar=TRUE, jags.refresh=1000)
t2<-Sys.time()
difftime(t2,t1)

t1<-Sys.time()
run2 <- extend.jags(run1, combine=F, sample=1000, thin=500, jags.refresh=100, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)

t1<-Sys.time()
run3 <- extend.jags(run2, combine=F, sample=2500, thin=500, jags.refresh=100, keep.jags.files=F)
t2<-Sys.time()
difftime(t2,t1)

# Ajoaika 2.25 d - > maanantaiaamuksi
t1<-Sys.time()
run4 <- extend.jags(run3, combine=F, sample=12000, thin=4000, jags.refresh=100, keep.jags.files=F)
t2<-Sys.time()
difftime(t2,t1) #1.12d

t1<-Sys.time()
run5 <- extend.jags(run4, combine=F, sample=12000, thin=4000, jags.refresh=1000, keep.jags.files=F)
t2<-Sys.time()
difftime(t2,t1) # #1.12d

#1000*200 :6.7 min()
#1000*2000 :67 min
#12000*2000 :13.4h
#12000*8000 :2.25d



run<-run5

summary(run, var="mu_mu")
summary(run, var="cv_mu")
summary(run, var="a")
summary(run, var="b")

plot(run, var="mu_mu")
plot(run, var="cv_mu")
plot(run, var="a")
plot(run, var="b")

