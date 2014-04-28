### Creating indicators for ReliefWeb data. ###

library(ggplot2)
library(lubridate)
library(reshape2)
library(hdxdictionary)  # special package for  HDx dictionaires.

source(collector.R)

# Collecting data.
ReportData <- GetLatestReports()
DisasterData <- GetLatestDisasters()

# Adding the continent name to the dataset
ReportData$iso3 <- toupper(ReportData$iso3)
DisasterData$iso3 <- toupper(DisasterData$iso3)

# Adding a month and year facets. 
ReportData$year <- year(ReportData$created)
DisasterData$year <- year(DisasterData$created)


source('rw.indicators.country.R')  # update this function
# rw.segment.countries(df = all.data)  # can't remember what this is about.
ReportData <- rw.indicators.country(df = ReportData, entity = 'disaster', end = 2014)
DisasterData <- rw.indicators.country(df = DisasterData, entity = 'disaster', end = 2014)


# Cleaning the dataset for export. 
# FormattedData$country.name <- hdxdictionray(FormattedData$iso3, 'iso3c', 'country.name') 
# there is one NA = Guam, territory of the United States of America. 
write.csv(FormattedData, file = 'data-summary/ReliefWeb-ALLCountries-report-2000-2014-wide.csv', row.names = TRUE)
write.csv(FormattedData, file = 'data-summary/ReliefWeb-ALLCountries-disasters-2000-2014-wide.csv', row.names = TRUE)




## Creating dataset.csv ## 
dsID <- 'reliefweb'
last_updated <- NA
last_scraped <- NA  # Add time of scrape.
name <- 'ReliefWeb'
dtset <- data.frame(dsID, last_updated, last_scraped, name)
write.csv(dtset, file = 'cps-export/dataset.csv', row.names = FALSE)

## Creating indicator.csv ## 
indID <- c('RW001', 'RW002')  # We have to create the indIDs for the indicators.
name <- c('Number of Reports', 'Number of Disasters')
units <- 'Integer'  # What units should I add here?
indic <- data.frame(indID, name, units)
write.csv(indic, file = 'cps-export/indicator.csv', row.names = FALSE)

## Creating value.csv ##
dsID <- 'reliefweb'
region <- NA  # Add the countries where the data is from. 
indID <- NA 
period <- NA  # Add the year that the data is about. 
value <- NA  # Add the values. 
is_number <- NA  # Validation test? If it is number == 1 !number == 0
# source <- NA  # I am not sure what should I add here. ReliefWeb? or a  link? 

## Using the files I've already collected. ##
## REVIEW!! 
# For disasters
disasters <- read.csv('data-summary/ReliefWeb-ALLCountries-disasters-2000-2014-long.csv')
disasters$indID <- 'RW001'
colnames(disasters)[1] <- 'value'
colnames(disasters)[2] <- 'period'
colnames(disasters)[3] <- 'region'
disasters$dsID <- 'reliefweb'
disasters$Country.Name <- NULL
disasters$source <- NA

# For reports
reports <- read.csv('data-summary/ReliefWeb-ALLCountries-report-2000-2014-long.csv')
reports$Country.Name <- NULL
reports$indID <- 'RW002'
colnames(reports)[1] <- 'value'
colnames(reports)[2] <- 'period'
colnames(reports)[3] <- 'region'
reports$dsID <- 'reliefweb'
reports$source <- NA

# For sources.
# The API doesn't serve when an organization joined ReliefWeb. 
# Therefore I am adding all organizations as of the year 2014.
sources <- read.csv('data-summary/ReliefWeb-AllSources.csv')
sources$focus <- hdxdictionary(sources$country, 'iso3c', 'hdx.focus')
sources.focus <- subset(sources, sources$focus == TRUE)
sources.table <- data.frame(table(sources.focus$country))
sources.table <- subset(sources.table, sources.table$Freq != 0)
sources.table$row.names <- NULL
sources <- sources.table

# Preparing for export
sources$indID <- 'RW003'
sources$period <- 2014
colnames(sources)[1] <- 'region'
colnames(sources)[2] <- 'value'
sources$dsID <- 'reliefweb'
sources$source <- NA

# Getting one file 
z <- rbind (disasters, reports, sources)

# Running the validation test. 
z <- is_number(z)

# Rearranging the output.

# Saving the CSV file. 
write.csv(z, file = 'cps-export/value.csv', row.names = FALSE)



## Validation test for is_number ## 
is_number <- function(df = NULL) { 
    for (i in 1:nrow(df)) { 
        if (is.numeric(df$value) == TRUE) { df$is_number[i] <- 1 }
        else { df$is_number[i] <- 0 }
    }
    return(df)
}