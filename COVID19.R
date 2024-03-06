#Try importing the data from the COVID-19 Model:
library(plyr)
library(dplyr)
library('bit64')
library(data.table)
setwd("/home/ross/Documents/")


mobility <- fread("US_Mobility_Report.csv")
healthData <- fread("US_counties_COVID19_health_weather_data.csv")

#Rename the sub_region_1 as state in mobility data:
mobility <- rename(mobility, c("state" = "sub_region_1", "county" = "sub_region_2"))
mobility$state <- as.character(mobility$state)
healthData$state <- as.character(healthData$state)

#Try to subset the data to get the friggin merge to work #notwinning
mobilitySubset <- subset(mobility,mobility$state == "California")
healthDataSubset <- subset(healthData, healthData$state == "California")
#Try merging the data: 
fullData <- full_join(mobilitySubset, healthDataSubset, by = "state")

#Do some EDA for the health data:
#subset data for california:
healthDataCali <- subset(healthData, healthData$state == "California")
healthDataCali$date <- as.Date(healthDataCali$date)
plot(healthDataCali$population_density_per_sqmi, healthDataCali$cases)

cor(healthDataCali$cases, healthDataCali$population_density_per_sqmi)
plot(healthDataCali$date, healthDataCali$deaths)

#Try to understand the rate of increase per county day to day. It looks like the while loop works.
caseIncrease <- rep(0, nrow(healthdataVentura))
healthdataVentura <- cbind(healthdataVentura, caseIncrease)
i = 2


while (i < nrow(healthdataVentura)) {
  caseIncrease <- healthdataVentura[i,]$cases - healthdataVentura[i-1,]$cases
  healthdataVentura[i,]$caseIncrease <- caseIncrease
  i <- i+1
  if(healthdataVentura[i,]$county != healthdataVentura[i-1,]$county){
    i <- i+1 
  }
}

nerd <- function()

healthdataVentura <- subset(healthdataVentura, select = c(1,2,3,4,5,228))
plot(healthdataVentura$caseIncrease)


#Look at data for New York:
healthDataNewYork <- subset(healthData, healthData$state == "New York")
test1 <- subset(healthDataNewYork, healthDataNewYork$date == "2020-04-16")
plot(test1$deaths, test1$population_density_per_sqmi)
cor(test1$deaths, test1$population_density_per_sqmi)
plot(healthDataNewYork$population_density_per_sqmi)

#Lets look at Los Angeles:
plot(healthDataCali$population_density_per_sqmi)
healthDataLA <- subset(healthData, healthData$county == "Los Angeles")
total <- sum(healthDataLA$cases)
plot(healthDataCali$date, healthDataCali$deaths)
plot(healthDataCali$deaths, healthDataCali$percent_65_and_over)
cor(healthDataCali$deaths, healthDataCali$percent_65_and_over)
#So it looks like the percent 65 and over is negatively correlated with deaths?
cor(healthDataCali$cases, healthDataCali$population_density_per_sqmi)

#More EDA of different factors: 
#What factors do I care about?
#Population density vs. # of cases:
plot(healthData$population_density_per_sqmi, healthData$cases)
#Ethic Breakdowns vs number of cases. 
plot(healthData$cases, healthData$percent_black)
cor(healthData$cases, healthData$percent_black)
plot(healthData$cases, healthData$num_asian)
plot(healthData$num_asian)
#Percent male vs number of cases
plot(healthData$cases, healthData$male)
#Percent age 65 and older vs number of cases and deaths.
plot(healthData$cases, healthData$percent_65_and_over)
cor(healthData$cases, healthData$percent_65_and_over)
#How is this negative....? -0.0445049
#Number of multi-unit housing vs number of cases
plot(healthData$cases, healthData$num_multi_unit_housing)
cor(healthData$cases, healthData$num_multi_unit_housing)
#Really high correlation: 0.4997955

#Do some accumulation for the # of cases vs the percent black for an area:
#Grab the last day available which has the max number of cases:
cumCases <- subset(healthData, healthData$date == "2020-04-16")
#Aggregate the data for the total population:
cumPop <- aggregate(total_population~county, data = healthData, FUN=mean)
healthData2 <- cumCases[,c(2,3,4,5,6)]
#Now merge with aggregate population data: 
healthData2 <- merge(healthData2, cumPop, by = "county")

#Number of people overcrowded vs. number of cases by county:
cumOvercrowding <- aggregate(num_overcrowding~county, data = healthData, FUN=sum)
healthData2 <- merge(healthData2, cumOvercrowding, by = "county")
names(healthData2)[6] <- "num_Overcrowding"

#Number of cases vs. number of hispanic people
cumhispanic <- aggregate(num_hispanic~county, data = healthData, FUN=mean)
healthData2 <- cbind(healthData2, cumhispanic$num_hispanic)
names(healthData2)[5] <- "num_hispanic"

#Number of white vs number of cases:
healthData$percent_insufficient_sleep
cumNonwhite <- aggregate(num_non_hispanic_white~county, data = healthData, FUN=mean)
healthData2 <- cbind(healthData2, cumNonwhite$num_non_hispanic_white)
names(healthData2)[6] <- "num_white"
plot(healthData2$num_white, healthData2$cases)
cor(healthData2$num_white, healthData2$cases)

#Lets look at percent insufficient sleep: 
cumPercentinsufficientsleep <- aggregate(percent_insufficient_sleep~county, data = healthData, FUN=mean)
healthData2 <- cbind(healthData2, cumPercentinsufficientsleep$percent_insufficient_sleep)
names(healthData2)[7] <- "percent_insufficientsleep"
plot(healthData2$cases, healthData2$percent_insufficientsleep)


