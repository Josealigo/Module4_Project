#How have emissions from motor vehicle sources 
#changed from 1999–2008 in Baltimore City?
  
install.packages("dplyr")
library(dplyr)

if(!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")
if(!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

vehicle_scc = grep("[V|v]eh",SCC$Short.Name) 
vehicle_scc2 = grep("[V|v]eh",SCC$EI.Sector) 
vehicle_sccF = SCC$SCC[union(vehicle_scc,vehicle_scc2)]

NEI_V = NEI[which(is.element(NEI$SCC,vehicle_sccF)),]
NEI_V = NEI_V %>% group_by(SCC,year) %>% summarise(TEmissions = sum(Emissions))
NEI_V = as.data.frame(NEI_V)

vehicle_sccF_frame = as.data.frame(cbind(SCC = as.character(vehicle_sccF),id = 1:length(vehicle_sccF)))
NEI_V = merge(NEI_V,vehicle_sccF_frame,by = "SCC")

png("Plot5.png")
par(mfrow = c(2,2),mar=c(1.1,1.1,1,1),cex = 0.6,oma=c(4,4,3,1),cex.main=2.5,col.main="blue")
plot(as.numeric(as.character(NEI_V$id))[which(NEI_V$year=="1999")],
     NEI_V$TEmissions[which(NEI_V$year=="1999")],pch = 20,col = rgb(1,0,0,0.1),
     ylim = c(0,4000))
legend("topleft",legend = "1999")
plot(as.numeric(as.character(NEI_V$id))[which(NEI_V$year=="2002")],
     NEI_V$TEmissions[which(NEI_V$year=="2002")],pch = 20,col = rgb(1,0,1,0.4),
     ylim = c(0,4000))
legend("topleft",legend = "2002")
plot(as.numeric(as.character(NEI_V$id))[which(NEI_V$year=="2005")],
     NEI_V$TEmissions[which(NEI_V$year=="2005")],pch = 20,col = rgb(1,0,1,0.7),
     ylim = c(0,4000))
legend("topleft",legend = "2005")
plot(as.numeric(as.character(NEI_V$id))[which(NEI_V$year=="2008")],
     NEI_V$TEmissions[which(NEI_V$year=="2008")],pch = 20,col = rgb(0,0,1,1),
     ylim = c(0,4000))
legend("topleft",legend = "2008")
title(main = "Emission by year per id SCC",outer=TRUE,xlab = "id_SCC",ylab = "Emissions" )
dev.off()
