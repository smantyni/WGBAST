############################
# Graphs for SST data and estimates
###############################

# Workflow:

# 1: Run data-SST-17.R
# 2: Input data to BUGS (data-SST-Aug17.odc) and run model-SST.odc 
# 3: Run graphs-SST.R for figures
# 4: Produce input data for the full life history model (SSTinputToJAGS.xlsx)


#############


# Plot SST data (this graph not currently needed anywhere, but nice to check out )
df.plot<-df.bugs %>% 
   group_by(year, Month)%>%
   summarise(sst=mean(sst.station), # mean SST per month
             sd.sst=sd(sst.station, na.rm=T))%>%
  mutate(month=parse_factor(Month, levels=c(1:4)))%>%
  mutate(Year=year+1991)

df.plot
ggplot(df.plot) + 
  geom_line(aes(x = Year, y = sst, color=month), size=1)+
  geom_errorbar(aes(x=Year, ymin=sst-sd.sst, ymax=sst+sd.sst, color=month)) +
  labs(x="Year", y="Sea surface temperature", color="Month")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

####################


# Expected April temperatures (median & 95% PI) from the model
(april<-read_csv("submodels/SST/output-aprilSST.csv",skip=4))
april<-mutate(april,Year=parse_double(year))%>%
select(-year)

df.obs<-filter(df.plot, Month==4)
df.april<-full_join(df.obs, april)
#View(df.april)


ggplot(data = df.april) +
  geom_line(aes(x=Year, y=med), col="colEst")+
  geom_line(aes(x=Year, y=low), col="colEst")+
  geom_line(aes(x=Year, y=high), col="colEst")+
  geom_line(aes(x=Year, y=sst), col="colDat")+
  geom_errorbar(aes(x=Year, ymin=sst-sd.sst, ymax=sst+sd.sst), col="colDat")+
  scale_colour_manual("", 
                      breaks = c("colEst", "colDat"),
                      values = c("colEst"="red", "colDat"="blue"))
  labs(x="Year", y="Temperature (C)",title="April SST", col=c("Prediction", "Observation"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

ggplot(data = datos, aes(x = fecha)) +
  geom_line(aes(y = TempMax, colour = "TempMax")) +
  geom_line(aes(y = TempMedia, colour = "TempMedia")) +
  geom_line(aes(y = TempMin, colour = "TempMin")) +
  scale_colour_manual("", 
                      breaks = c("TempMedia", "TempMax", "TempMin"),
                      values = c("TempMedia"="green", "TempMax"="red", 
                                 "TempMin"="blue")) +

