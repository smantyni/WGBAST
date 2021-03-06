
# Compare BUGS/JAGS results

#source("models-select.R")


## ---- load-nsp

# Model 1: BUGS
# =================
if(compare=="BJ"){
  
  Nsp<-array(NA, dim=c(16,(length(Years2B)+5), 1000))
  
  for(y in 6:(length(Years2B)+5)){
    for(r in 1:15){
      x<-read.table(paste(sep="", folder1,"/NspWtot[",y,",",r,"]1.txt"))
      Nsp[r,y,]<-x[1:1000,2]
    }
  }
  for(r in 16:16){ #Kåge
    for(y in 27:(length(Years2B)+5)){ # 2013->
      x<-paste(sep="", folder1,"/NspWtot[",y,",",r,"]1.txt")
      temp <-read.table(x)
      Nsp[r,y,]<-temp[1:1000,2]
    }
  }
  
  dim(Nsp)
  
  for(r in 1:nstocks){
    df<-boxplot.bugs.df2(Nsp, r ,1:length(YearsB))%>%
      mutate(River=r)
    ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
  }
  df.1<-as.tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
    select(River, everything())%>%
    mutate(Year=Year+1986)
  df.1
  #View(df.1)
}

if(compare=="JJ"){
  # Number of spawners per river
  for(r in 1:nstocks){
    #r<-1
    df<-boxplot.jags.df2(chains1, "NspWtot[",str_c(r,"]"),1:(length(Years)+1))
    #df<-boxplot.jags.df2(dsub, "NspWtot[",str_c(r,"]"),1:length(Years))
    df<-mutate(df, River=r)
    ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
  }
  df.1<-as.tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
    select(River, everything())%>%
    mutate(Year=Year+1986)
  df.1
  #View(df.1)
}

# Model 2: JAGS
# =================

# Number of spawners per river
for(r in 1:nstocks){
#r<-1
  df<-boxplot.jags.df2(chains, "NspWtot[",str_c(r,"]"),1:(length(Years)+1))
  #df<-boxplot.jags.df2(dsub, "NspWtot[",str_c(r,"]"),1:length(Years))
  df<-mutate(df, River=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}
df.2<-as.tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1986)
df.2
#View(df.2)


# Spawner count datasets
# =================

counts<-read_tsv(str_c(pathData,"spawner_counts.txt"),skip=7,col_names=T, na="NA")
colnames(counts)<-Rivername
counts<-counts%>%
  mutate(Year=c(1:(length(Years)+1)))%>%
  mutate(Year=Year+1986)%>%
  select(Torne, Simo, Kalix, Ume, Year)%>%
  gather(key="River", value="Count", `Torne`:`Ume`)%>%
  mutate(River=fct_recode(River,
                          "1"="Torne",
                          "2"="Simo",
                          "3"="Kalix",
                          "10"="Ume"))%>%
  mutate(River=parse_integer(River))%>%
  mutate(Count=Count/1000)

counts2<-read_tsv(str_c(pathData,"spawner_counts_notInJAGS.txt"),col_names=T, na="NA")
counts2<-counts2%>%
  gather(key="River", value="Count2", `Rane`:`Ore`)%>%
  mutate(River=fct_recode(River,
                        "4"="Rane","5"="Pite",
                        "6"="Aby","7"="Byske", "8"="Rickle", "11"="Ore"))%>%
  mutate(River=parse_integer(River))%>%
  mutate(Count2=Count2/1000)
  

counts<-full_join(counts, counts2, by=NULL)
#View(counts2)


df.2<-left_join(df.2,counts, by=NULL)
#View(df.1)

## ---- graphs-nsp


# Draw boxplots to compare
# ==========================


for(r in 1:16){
#r<-5
df1<-filter(df.1, River==r, Year>1991)
df2<-filter(df.2, River==r, Year>1991)
#df1<-filter(df.1, Year>1991)
#df2<-filter(df.2, Year>1991)
print(
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
  labs(x="Year", y="Number of spawners (1000s)", title=Rivername_long[r])+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+  
  geom_point(data=df2, aes(Year, Count),col="red")+
  geom_point(data=df2, aes(Year, Count2),col="blue", shape=17)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))#+
  #facet_wrap(~River, scales="free") # Facet if you like to have all graphs together, downside is you cannot easily control ylim and scales are very different
  
)
}

## ---- graphs-nsp-report


# Draw boxplots to compare
# ==========================

#df1<-filter(df.1, Year>1991)
#df2<-filter(df.2, Year>1991)

plots<-list()
for(r in 1:16){
  #r<-1
  df1<-filter(df.1, River==r, Year>1991)
  df2<-filter(df.2, River==r, Year>1991)
  plot<-ggplot(df2, aes(Year))+
          theme_bw()+
          geom_boxplot(
            aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
            stat = "identity",fill=rgb(1,1,1,0.6))+
          labs(x="Year", y="1000s spawners", title=Rivername_long[r])+
          geom_line(aes(Year,q50))+
          geom_point(data=df2, aes(Year, Count),col="red")+
          geom_point(data=df2, aes(Year, Count2),col="blue", shape=17)+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
  plots[[r]]<-plot
}

res <- 6
name_figure <- "spawners1.png"
png(filename = name_figure, height = 500*res, width = 500*res, res=72*res)

grid.arrange(plots[[1]],plots[[2]], plots[[3]],plots[[4]], 
             plots[[5]],plots[[6]], plots[[7]],plots[[8]], 
             plots[[9]],ncol=3)

dev.off()

res <- 6
name_figure <- "spawners2.png"
png(filename = name_figure, height = 500*res, width = 500*res, res=72*res)

grid.arrange(plots[[10]], plots[[11]],plots[[12]], 
             plots[[13]],plots[[14]], plots[[15]],plots[[16]],ncol=3)

dev.off()
