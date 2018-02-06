# For reason or another this does not run in JAGS
# -> use BUGS for now.



M1<-"
model{
  for(i in 1:N_FI){ #number of females in the Finnish data
    x[i]~dbin(p[i,j[i]], Eggs[i]) # likelihood function: x = number of surviving eggs, p= probability of survival, Egg = total number of eggs

    j[i]~dcat(q[year[i],ss[i], 1:2]) # index if the female has M74 or not
    p[i,1]<- (1-M_YSFM[i])  # survival from normal yolk-sac-fry mortality
    p[i,2] <- (1-M_YSFM[i])*(1-M74[year[i],i,k[i]]) # survival from normal and M74 mortality

    k[i]~dcat(qq[year[i],ss[i],1:2]) # index if the female has 100% M74 mortality or not
    M74[year[i],i,1] ~dbeta(a_M74[year[i]],b_M74[year[i]]) # M74 mortality
    M74[year[i],i,2] <- 1 # 100% M74 mortality
    M_YSFM[i]~dbeta(a_M_YSFM,b_M_YSFM) # normal yolk-sac-fry mortality
  }

  # transformation from mean and eta into beta-parameters
  a_M_YSFM<- mu_M_YSFM * eta_M_YSFM
  b_M_YSFM<- (1- mu_M_YSFM) * eta_M_YSFM
  mu_M_YSFM~dbeta(2,2)I(0.01,0.99) # mean normal yolk-sac-fry mortality
  eta_M_YSFM~dunif(2,1000) # indicator of variance in normal yolk-sac-fry mortality

  for(y in 1:32){ # Years , last spawning yearclass in the data is 30=2014
    a_M74[y] <- mu_M74[y] * eta_M74[y] # conversion of mean and variance into alpha parameter of beta distribution
    b_M74[y] <- (1- mu_M74[y]) * eta_M74[y] # conversion of mean and variance into beta parameter of beta distribution
    mu_M74[y]~dbeta(2,2)I(0.01,0.99) # mean M74 value for each year
    eta_M74[y]~dunif(2,1000) # indicator of variance between stocks

    for(s in 1:14){  # Obs! Kemijoki added
      surv2_M74[y,s] <-  (qq[y,s,1] * (1-mu_M74[y])) + (qq[y,s,2]) *0 # probability of offspring surviving M74 mortality if female has M74
      mean2_M74[y,s] <- 1 - surv2_M74[y,s] #probability of offspring dying because of M74 mortality if female has M74

      surv_M74[y,s] <- (q[y,s,1] *1)+ (q[y,s,2] * qq[y,s,1] * (1-mu_M74[y])) + (q[y,s,2] * qq[y,s,2] *0) # probability of offspring surviving M74 mortality
      mean_M74[y,s] <- 1 - surv_M74[y,s] #probability of offspring dying because of M74 mortality
      
      q[y,s,1] ~dbeta(a_q[y],b_q[y]) # probability of a female not having M74 syndromes
      q[y,s,2] <- 1- q[y,s,1] # probability of a female having M74 syndromes
      
      qq[y,s,1] ~dbeta(a_qq[y],b_qq[y]) # probability of a female not having 100 % M74 mortality
      qq[y,s,2] <- 1- qq[y,s,1] # probability of a female having 100 % M74 mortality
    }

    a_q[y] <- mu_q[y] * eta_q
    b_q[y] <- (1- mu_q[y]) * eta_q
    mu_q[y]~dbeta(2,2)I(0.01,0.99)# mean probability of a female not having M74 syndromes
    
    a_qq[y] <- mu_qq[y] * eta_qq
    b_qq[y] <- (1- mu_qq[y]) * eta_qq
    mu_qq[y]~dbeta(2,2)I(0.05,0.95)# mean probability of a female not having 100 % M74 mortality
  }

  eta_q~dunif(2, 1000)# indicator of variance in q
  eta_qq~dunif(10, 1000)# indicator of variance in qq

  for (i in 1:N_SE){ # number of rows in the Swedish data 
    xx[i]~dbin(q[yy[i], stock[i], 2], Females[i]) # likelihood function: xx = number of females affected by M74, p= probability of a female having M74, females = total number of females
  }
}
"


modelName<-"M74model"

Mname<-str_c("submodels/M74/",modelName,".txt")
cat(M1,file=Mname)

data<-list(
  # FI data:
  Eggs= dfFI$eggs,
  year= dfFI$year,
  ss=dfFI$river,
  x=dfFI$surv_eggs,
  j=dfFI$M74,
  k=dfFI$mortality100,
  N_FI=length(dfFI$eggs), # number of females/rows
# SE data:
  yy=dfSE$yy,
  stock=dfSE$stock,
  Females=dfSE$ff,
  xx=dfSE$xx,
  N_SE=length(dfSE$xx) # number of rows
)


var_names<-c(
  "mean_M74", "mean2_M74"
)


#nb of samples = samples * thin, burnin doesn't take into account thin
# sample on tässä lopullinen sample, toisin kuin rjagsissa!!!

t1<-Sys.time()
run1 <- run.jags(M1, 
                 monitor= var_names,data=data,#inits = inits,
                 n.chains = 2, method = 'rjparallel', thin=300, burnin =0, 
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 100, 
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)
# 17h

