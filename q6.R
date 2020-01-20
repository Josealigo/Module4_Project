#Compare emissions from motor vehicle sources in Baltimore City
#with emissions from motor vehicle sources in Los Angeles County,
#California (\color{red}{\verb|fips == "06037"|}fips=="06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?
  
install.packages("dplyr")
library(dplyr)

if(!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")
if(!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

vehicle_scc = grep("[V|v]eh",SCC$Short.Name) 
vehicle_scc2 = grep("[V|v]eh",SCC$EI.Sector) 

vehicle_sccF = SCC$SCC[union(vehicle_scc,vehicle_scc2)]

Baltimore = NEI[which(NEI$fips == "24510"  & NEI$SCC %in% vehicle_sccF),]
California = NEI[which(NEI$fips == "06037"  & NEI$SCC %in% vehicle_sccF),]

vehicle_sccF_frame = as.data.frame(cbind(SCC = as.character(vehicle_sccF),id = 1:length(vehicle_sccF)))

Baltimore = merge(Baltimore,vehicle_sccF_frame,by = "SCC")
California = merge(California,vehicle_sccF_frame,by = "SCC")

Baltimore = Baltimore %>% group_by(id,year) %>% summarise(TEmissions = sum(Emissions))
California = California %>% group_by(id,year) %>% summarise(TEmissions = sum(Emissions))

Baltimore = as.data.frame(Baltimore)
California = as.data.frame(California)

png("Plot6.png")
par(mfrow = c(2,2),mar=c(1.1,1.1,1,1),cex = 0.6,oma=c(4,4,3,1),cex.main=2.5,col.main="blue")
plot(as.numeric(as.character(Baltimore$id))[which(Baltimore$year=="1999")],
     Baltimore$TEmissions[which(Baltimore$year=="1999")],pch = 20,col = rgb(1,0,0,0.7),
     ylim = c(0,400))
legend("top",legend = "1999")
legend("topright",legend = c("Baltimore","California"),pch=c(20,20),col=c(rgb(1,0,0,1),rgb(0,0,1,1)))
points(as.numeric(as.character(California$id))[which(California$year=="1999")],
       California$TEmissions[which(California$year=="1999")],pch = 20,col = rgb(0,0,1,0.7))
plot(as.numeric(as.character(Baltimore$id))[which(Baltimore$year=="2002")],
     Baltimore$TEmissions[which(Baltimore$year=="2002")],pch = 20,col = rgb(1,0,0,0.7),
     ylim = c(0,400))
legend("top",legend = "2002")
legend("topright",legend = c("Baltimore","California"),pch=c(20,20),col=c(rgb(1,0,0,1),rgb(0,0,1,1)))
points(as.numeric(as.character(California$id))[which(California$year=="2002")],
       California$TEmissions[which(California$year=="2002")],pch = 20,col = rgb(0,0,1,0.7))
plot(as.numeric(as.character(Baltimore$id))[which(Baltimore$year=="2005")],
     Baltimore$TEmissions[which(Baltimore$year=="2005")],pch = 20,col = rgb(1,0,0,0.7),
     ylim = c(0,400))
legend("top",legend = "2005")
legend("topright",legend = c("Baltimore","California"),pch=c(20,20),col=c(rgb(1,0,0,1),rgb(0,0,1,1)))
points(as.numeric(as.character(California$id))[which(California$year=="2005")],
       California$TEmissions[which(California$year=="2005")],pch = 20,col = rgb(0,0,1,0.7))
plot(as.numeric(as.character(Baltimore$id))[which(Baltimore$year=="2008")],
     Baltimore$TEmissions[which(Baltimore$year=="2008")],pch = 20,col = rgb(1,0,0,0.7),
     ylim = c(0,400))
legend("top",legend = "2008")
legend("topright",legend = c("Baltimore","California"),pch=c(20,20),col=c(rgb(1,0,0,1),rgb(0,0,1,1)))
points(as.numeric(as.character(California$id))[which(California$year=="2008")],
       California$TEmissions[which(California$year=="2008")],pch = 20,col = rgb(0,0,1,0.7))
title(main = "Emission by year per id SCC",outer=TRUE,xlab = "id_SCC",ylab = "Emissions" )
dev.off()
