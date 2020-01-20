#Of the four types of sources indicated by the type (point, nonpoint, onroad, 
#nonroad) variable, which of these four sources have seen decreases in
#emissions from 1999–2008 for Baltimore City? Which have seen increases
#in emissions from 1999–2008? Use the ggplot2 plotting system to make 
#a plot answer this question.


install.packages("dplyr")
library(dplyr)
library(ggplot2)

if(!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")
if(!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

both_SCC = intersect(NEI$SCC[which(NEI$year== 1999 & NEI$fips == "24510")],NEI$SCC[which(NEI$year== 2008 & NEI$fips == "24510")])
both_SCC = intersect(both_SCC,NEI$SCC[which(NEI$year== 2002 & NEI$fips == "24510")])
both_SCC = intersect(both_SCC,NEI$SCC[which(NEI$year== 2005 & NEI$fips == "24510")])

Baltimore = NEI[which(NEI$fips == "24510" & NEI$SCC %in% both_SCC),]
Baltimore = Baltimore %>% group_by(fips,year,type) %>% summarise(TEmissions = sum(Emissions))
Baltimore = as.data.frame(Baltimore)

png("Plot3.png")
g <- ggplot(aes(year,TEmissions),data = Baltimore)
g2 <- g + geom_line(aes(colour = factor(type))) + labs(colour = "Type of SCC",title =  "Baltimore City")
print(g2)
dev.off()
