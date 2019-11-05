library(dplyr)
library(plyr)
library(tidyr)

fluDataRaw <- read.csv("data/fluData.csv")

fluData <- data.frame(County = fluDataRaw$County, 
                      Date = fluDataRaw$Week.Ending.Date,
                      Disease = fluDataRaw$Disease,
                      Incidents = as.integer(fluDataRaw$Count),                 
                      Coordinates = fluDataRaw$County.Centroid)

## Split coordinates into Longitude/Latitude (Double)
fluData <- fluData %>%
  separate(Coordinates, c("Latitude", "Longitude"), ", ")
fluData$Latitude <- substring(fluData$Latitude, first = 2)
fluData$Longitude <- substring(fluData$Longitude, 1, 
                               nchar(fluData$Longitude) - 1)
fluData$Latitude <- as.double(fluData$Latitude)
fluData$Longitude <- as.double(fluData$Longitude)

## Separate date into Year, Month and Day (integers)
fluData <- fluData %>%
  separate(Date, sep = "/", into = c("Month", "Day", "Year"))
fluData$Month <- as.integer(fluData$Month)
fluData$Day <- as.integer(fluData$Day)
fluData$Year <- as.integer(fluData$Year)

## Rename Influenza Labels
fluData$Disease <- revalue(fluData$Disease, 
                           c("INFLUENZA_A" = "A", 
                             "INFLUENZA_B" = "B", 
                              "INFLUENZA_UNSPECIFIED" = "Unspecified"))

## Work with consolidated data
fluDataCons <- fluData[-c(3)]
fluDataCons  <- aggregate(Incidents ~ ., fluDataCons, sum)


