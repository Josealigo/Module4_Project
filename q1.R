#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#Using the base plotting system, make a plot showing the total PM2.5 emission from
#all sources for each of the years 1999, 2002, 2005, and 2008.


install.packages("dplyr")
library(dplyr)

if(!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")
if(!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

SCC_frame = as.data.frame(cbind(SCC = as.character(SCC$SCC),id = 1:length(SCC$SCC)))
Total_Emi_SCC_year = merge(NEI,SCC_frame,by = "SCC")
Total_Emi_SCC_year = Total_Emi_SCC_year %>% group_by(id,year) %>% summarise(TEmissions = sum(Emissions))
Total_Emi_SCC_year = as.data.frame(Total_Emi_SCC_year)

png("Plot1.png")
par(mfrow = c(2,2),mar=c(1.1,1.1,1,1),cex = 0.6,oma=c(4,4,3,1),cex.main=2.5,col.main="blue")
plot(as.numeric(as.character(Total_Emi_SCC_year$id))[which(Total_Emi_SCC_year$year=="1999")],
     Total_Emi_SCC_year$TEmissions[which(Total_Emi_SCC_year$year=="1999")],pch = 20,col = rgb(0,0,1,0.4),
     ylim = c(0,40000))
legend("top",legend = "1999")
plot(as.numeric(as.character(Total_Emi_SCC_year$id))[which(Total_Emi_SCC_year$year=="2002")],
     Total_Emi_SCC_year$TEmissions[which(Total_Emi_SCC_year$year=="2002")],pch = 20,col = rgb(0,0,1,0.4),
     ylim = c(0,40000))
legend("top",legend = "2002")
plot(as.numeric(as.character(Total_Emi_SCC_year$id))[which(Total_Emi_SCC_year$year=="2005")],
     Total_Emi_SCC_year$TEmissions[which(Total_Emi_SCC_year$year=="2005")],pch = 20,col = rgb(0,0,1,0.4),
     ylim = c(0,40000))
legend("top",legend = "2005")
plot(as.numeric(as.character(Total_Emi_SCC_year$id))[which(Total_Emi_SCC_year$year=="2008")],
     Total_Emi_SCC_year$TEmissions[which(Total_Emi_SCC_year$year=="2008")],pch = 20,col = rgb(0,0,1,0.4),
     ylim = c(0,40000))
legend("top",legend = "2008")
title(main = "Emission by year per id SCC",outer=TRUE,xlab = "id_SCC",ylab = "Emissions" )
dev.off()
