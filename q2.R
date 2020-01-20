#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
#(fips == "24510") from 1999 to 2008? Use the base plotting system to make
#a plot answering this question.

library(dplyr)

if(!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")
if(!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

both_SCC = intersect(NEI$SCC[which(NEI$year== 1999 & NEI$fips == "24510")],NEI$SCC[which(NEI$year== 2008 & NEI$fips == "24510")])
both_SCC = intersect(both_SCC,NEI$SCC[which(NEI$year== 2002 & NEI$fips == "24510")])
both_SCC = intersect(both_SCC,NEI$SCC[which(NEI$year== 2005 & NEI$fips == "24510")])
SCC_frame = as.data.frame(cbind(SCC = as.character(SCC$SCC),id = 1:length(SCC$SCC)))

Baltimore_1999 = NEI[which(NEI$fips == "24510" & NEI$year %in% c(1999) & NEI$SCC %in% both_SCC),]
Baltimore_2002 = NEI[which(NEI$fips == "24510" & NEI$year %in% c(2002) & NEI$SCC %in% both_SCC),]
Baltimore_2005 = NEI[which(NEI$fips == "24510" & NEI$year %in% c(2005) & NEI$SCC %in% both_SCC),]
Baltimore_2008 = NEI[which(NEI$fips == "24510" & NEI$year %in% c(2008) & NEI$SCC %in% both_SCC),]

Baltimore_1999 = merge(Baltimore_1999,SCC_frame,by="SCC")
Baltimore_2002 = merge(Baltimore_2002,SCC_frame,by="SCC")
Baltimore_2005 = merge(Baltimore_2005,SCC_frame,by="SCC")
Baltimore_2008 = merge(Baltimore_2008,SCC_frame,by="SCC")
Baltimore_1999 = as.data.frame(Baltimore_1999)
Baltimore_2002 = as.data.frame(Baltimore_2002)
Baltimore_2005 = as.data.frame(Baltimore_2005)
Baltimore_2008 = as.data.frame(Baltimore_2008)

png("Plot2.png")
par(mfrow = c(1,1),cex = 0.6,mar = c(4,4,2,1))
plot(Baltimore_1999$id,Baltimore_1999$Emissions,col="red",pch=20,ylim = c(0,230))
points(Baltimore_2002$id,Baltimore_2002$Emissions,col="yellow",pch = 20,type="p")
points(Baltimore_2005$id,Baltimore_2005$Emissions,col="blue",pch = 20,type="p")
points(Baltimore_2008$id,Baltimore_2008$Emissions,col="green",pch = 20,type="p")
legend("topleft",col = c("red","yellow","blue","green"),legend = c("1999","2002","2005","2008"),pch = 20)
title(main = "Baltimore City")
dev.off()
