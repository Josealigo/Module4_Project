#Across the United States, how have emissions from coal 
#combustion-related sources changed from 1999–2008?


install.packages("dplyr")
library(dplyr)

if(!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")
if(!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

coal_scc = grep("[c|C]oal",SCC$Short.Name) 
combustion_scc = grep("[c|C]omb",SCC$EI.Sector) 
coal_combustion_scc = SCC$SCC[intersect(coal_scc,combustion_scc)]
SCC_frame = as.data.frame(cbind(SCC = as.character(SCC$SCC),id = 1:length(SCC$SCC)))

NEI_CCSCC = NEI[which(is.element(NEI$SCC,coal_combustion_scc)),]
NEI_CCSCC = NEI_CCSCC %>% group_by(SCC,year) %>% summarise(TEmissions = sum(Emissions))
NEI_CCSCC = merge(NEI_CCSCC,SCC_frame,by = "SCC")
NEI_CCSCC = as.data.frame(NEI_CCSCC)

png("Plot4.png")
g <- ggplot(aes(x=id,y=TEmissions),data = NEI_CCSCC)
g2 <- g + geom_point(aes(colour = factor(year))) + labs(colour = "Type of SCC",title =  "Emisiions by SCC per year") + ylab("Emissions") + xlab("SCC")
print(g2)
dev.off()
