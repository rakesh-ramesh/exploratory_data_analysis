NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Plot 1 - Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
png("plot1.png",width=480,height=480,units="px")
emission_total <- tapply(NEI$Emissions,NEI$year,sum)
barplot(names.arg=names(emission_total),height=emission_total, xlab="Year", ylab="Total Emissions (in Tons)",main=expression('Total PM'[2.5]*' emissions at various years'))
dev.off()

#Plot 2 - Baltimore City data
png("plot2.png",width=480,height=480,units="px")
baltimore <- subset(NEI, NEI$fips == "24510")
baltimore_emissions_total <- tapply(baltimore$Emissions, baltimore$year, sum)
barplot(names.arg=names(baltimore_emissions_total),height=baltimore_emissions_total,xlab="Year", ylab="Total Emissions (in Tons)",main=expression('Total PM'[2.5]*' emissions at various years for Baltimore City'))
dev.off()

#Plot 3 - Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen 
#decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to 
#make a plot answer this question.
png("plot3.png",width=480,height=480,units="px")
baltimore <- subset(NEI, NEI$fips == "24510")
baltimore_year_type <- aggregate(Emissions ~ year+type, baltimore, sum)
library(ggplot2)
g <- ggplot(baltimore_year_type, aes(year,Emissions,color=type))
g <- g + geom_line() + xlab("Year") + ylab("Total Emission (in Tons)") + ggtitle("Total Emissions in Baltimore City from 1999-2008")
print(g)
dev.off()

#Plot 4 - Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
png("plot4.png",width=480,height=480,units="px")
SCC_coal_list <- subset(SCC$SCC, grepl("Coal",SCC$Short.Name, ignore.case = TRUE))
coal_emissions <- subset(NEI, SCC %in% SCC_coal_list)
coal_emission_total <- tapply(coal_emissions$Emissions, coal_emissions$year, sum)
barplot(names.arg=names(coal_emission_total), height=coal_emission_total, xlab="Year", ylab="Total Emission (in Tons)", main=expression('Total Coal Related PM'[2.5]*' emissions at various years'))
dev.off()

#Plot 5 - How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
png("plot5.png",width=480,height=480,units="px")
baltimore_motor <- subset(NEI, type == "ON-ROAD" & fips == "24510")
baltimore_motor_emissions_total <- tapply(baltimore_motor$Emissions, baltimore_motor$year, sum)
barplot(names.arg=names(baltimore_motor_emissions_total),height=baltimore_motor_emissions_total,xlab="Year", ylab="Total Emissions (in Tons)",main=expression('Total PM'[2.5]*' motor vehicle emissions at various years for Baltimore City'))
dev.off()


#Plot 6 - Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
png("plot6.png",width=480,height=480,units="px")
baltimore_motor <- subset(NEI, type == "ON-ROAD" & fips == "24510")
baltimore_motor_emissions_total <- tapply(baltimore_motor$Emissions, baltimore_motor$year, sum)
LA_county_motor <- subset(NEI, type=="ON-ROAD" & fips == "06037")
LA_county_motor_emissions_total <- tapply(LA_county_motor$Emissions, LA_county_motor$year, sum)
LA_motor <- data.frame(year=names(LA_county_motor_emissions_total),total_emissions=LA_county_motor_emissions_total,location=rep("Los Angeles County",length(LA_county_motor_emissions_total)))
baltimore_motor <- data.frame(year=names(baltimore_motor_emissions_total), total_emissions=baltimore_motor_emissions_total, location=rep("Baltimore City",length(baltimore_motor_emissions_total)))
motor_LA_baltimore <- rbind(LA_motor, baltimore_motor)
g <- ggplot(motor_LA_baltimore, aes(year,total_emissions))
g <- g + geom_bar(stat="identity") + facet_grid(.~location) + xlab("Year") + ylab("Total Emissions (in Tons)") + ggtitle("Total PM 2.5 emissions at various years")
print(g)
dev.off()
