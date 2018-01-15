############################
# Graphs for SST data and estimates
###############################

# Workflow:

# 1: Run data-temperature-17.R
# 2: Input data to BUGS (data-SST-Aug17.odc) and run model-SST.odc 
# 3: Run graphs-SST.R for figures


#############

# Plot SST data (this graph not currently needed anywhere, but nice to check out )
(tmp<-dat %>% 
   group_by(Year, Month) %>%
   summarise(sst=mean(Temperature),
             sd.sst=sd(Temperature)))%>%
  mutate(month=parse_factor(Month, levels=c(1:4)))
View(tmp)

ggplot(data = tmp) + 
  geom_line(mapping = aes(x = Year, y = sst, color=parse_factor(Month, levels=c(1:4))), size=1)+
  geom_errorbar(aes(x=Year, ymin=sst-sd.sst, ymax=sst+sd.sst, color=parse_factor(Month, levels=c(1:4)))) +
  labs(x="Year", y="Sea surface temperature", color="Month")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

####################
read_cs

# Expected April temperatures (median & 95% PI) from the model
(april<-read_csv("Temperature/output-aprilSST.csv",skip=4))


ggplot(data = april) +
  geom_line(aes=(x=Year, y=med))
  

#windows()
cbind(1992:(1992+end), april$med[1:(end+1)], april$low[1:(end+1)], april$high[1:(end+1)])

plot(1992:(1992+end), april$med[1:(end+1)], type="l", lwd=2, col="red", ylim=c(1,7),
     xlab="Year", ylab="Temperature (C)", main="SST in April", cex.lab=1.2, cex.axis=1.1)
points(1992:(1992+end), april$low[1:(end+1)], type="l", lwd=1, col="red")
points(1992:(1992+end), april$high[1:(end+1)], type="l", lwd=1, col="red")
m<-4
cbind(1992:(1992+(end-1)), meanTemp[1:end,m])
points(1992:(1992+(end-1)), meanTemp[1:end,m], col=m,type="l", lty=1, lwd=2)
segments(1992:(1992+(end-1)), meanTemp[,m]+sdTemp[,m],
         1992:(1992+(end-1)), meanTemp[,m]-sdTemp[,m], col=m, lwd=2)
legend("topleft", c("Model expected","Observed"), 
       col=c("red", "blue"), lty=c(1,1), lwd=c(2,2))

dev.off()

for(m in 1:3){
  #m<-1
  #cbind(1992:(1992+end), meanTemp[1:end,m])
  points(1992:(1992+end), meanTemp[1:end,m], col=m,type="l", lty=1, lwd=2)
  segments(1992:(1992+end), meanTemp[,m]+sdTemp[,m],
           1992:(1992+end), meanTemp[,m]-sdTemp[,m], col=m, lwd=2)
}

res <- 6
name_figure <- "figure2.png"
png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
