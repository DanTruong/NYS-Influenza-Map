# Load requisite libraries
library(dplyr)
library(plyr)
library(tidyr)
library(forecast)
library(stats)
library(datasets)
library(dummies)

# Load in dataset file
origin <- read.csv("fluData.csv")

# Custom function to "proper"-ize string values
proper <- function(s) sub("(.)", ("\\U\\1"), tolower(s), pe=TRUE)

# Extract locations data from origin
locations <- data.frame(
    County = proper(origin$County),
    Coordinates = origin$County.Centroid
)

## Remove duplicate rows
locations <- distinct(locations)

## Separate coordinates into lat/long
locations <- locations %>% separate(Coordinates, c("Latitude", "Longitude"), ", ")
locations$Latitude <- substring(locations$Latitude, first = 2)
locations$Longitude <- substring(locations$Longitude, 1, nchar(locations$Longitude) -
                                     1)
locations$Latitude <- as.double(locations$Latitude)
locations$Longitude <- as.double(locations$Longitude)

# Extract date data from origin
dates <- data.frame(
    Season = origin$Season,
    Week = origin$CDC.Week,
    Date = as.Date(origin$Week.Ending.Date, format = "%m/%d/%Y"), 
    RawDate = origin$Week.Ending.Date
)

## Remove duplicate rows
dates <- distinct(dates)
## Create M-D-Y variable 
dates <- dates %>% separate(RawDate, sep="/", into = c("Month", "Day", "Year"))

rawData <- data.frame(
    Season = origin$Season,
    County = proper(origin$County),
    Week = as.numeric(origin$CDC.Week),
    Incidents = origin$Count,
    Date = as.Date(origin$Week.Ending.Date, format = "%m/%d/%Y"), 
    RawDate = origin$Week.Ending.Date
)
rawData <- rawData %>% separate(RawDate, sep="/", into = c("Month", "Day", "Year"))
rawData <- rawData[-c(5, 6, 7)]
rawData$Year <- as.numeric(rawData$Year)

# Aggregate flu incidents by sum
fluData <- aggregate(Incidents ~ ., rawData, sum)

# Separate 2019-2020 season
#fluData09thru19 <- fluData[!fluData$Season=="2019-2020",]
fluData09thru18 <- fluData[!fluData$Season=="2018-2019" & !fluData$Season=="2019-2020",]
fluData18thru19 <- fluData[fluData$Season=="2018-2019",]

year <- 2010

### WEEK 40 IS THE BEGINNING OF THE SEASON!!!
for(i in unique(fluData09thru18$Season)){
    for(j in unique(fluData09thru18$County)){
        for(k in 21:39){
            inputData <- data.frame(i, j, k, year, 0)
            names(inputData) <- c("Season", "County", "Week", "Year", "Incidents")
            fluData09thru18 <- rbind(fluData09thru18, inputData)
        }
    }
    year <- year + 1
}

# Sort flu data
fluData09thru18 <- fluData09thru18[order(
    fluData09thru18$Year,
    fluData09thru18$Season,
    fluData09thru18$Week
),]

arimaTable <- data.frame(County = NA, Estimated = NA, Week = NA, Year = NA)

outerStartTime <- Sys.time()

for(i in unique(fluData09thru18$County)){
    innerStartTime <- Sys.time()
    
    print(paste("Getting incidents from", i))
    incidents <- fluData09thru18$Incidents[fluData09thru18$County == i]
    
    print(paste("Converting into time-series data"))
    timeSeriesData <- ts(incidents, frequency = 52, start = c(2009, 40))
    print(paste(timeSeriesData))

    print(paste("Training ARIMA model"))
    arimData <- auto.arima(timeSeriesData)
    
    print(paste("Predicting the next 52 periods"))
    fcData <- forecast(arimData, h = 52)
    
    print(paste("Binding results to ARIMA values table"))
    week <- 40
    for(j in fcData$mean){
        inputVal <- c(i, j, as.numeric(week), 2018)
        names(inputVal) <- c("County", "Estimated", "Week", "Year")
        arimaTable <- rbind(arimaTable, inputVal)
        week <- week + 1
    }
    innerEndTime <- Sys.time()
    
    innerTotalTime <- innerEndTime - innerStartTime
    
    print(paste(i, "processing time:", innerTotalTime)) 
}

outerEndTime <- Sys.time()
outerTotalTime <- outerEndTime - outerStartTime
print(paste("Total processing time:", outerTotalTime)) 

# Remove first row of ARIMA table w/ the NAs
arimaTable <- arimaTable[-1,]
arimaTableTest <- arimaTable

# Correct the weeks designation per row
for(i in unique(arimaTableTest$Week)){
    if(i > 52){
        arimaTableTest$Year[arimaTableTest$Week==i] <- 2019
        arimaTableTest$Week[arimaTableTest$Week==i] <- as.numeric(i) - 52
    } 
}

#write.csv(arimaTableTest, file = "arimaOutput.csv")
fluData18thru19$Predicted <- 0

for(i in unique(fluData18thru19$County)){
    for(j in unique(fluData18thru19$Year)){
        for(k in unique(fluData18thru19$Week)){
            print(paste(i, j, k))
            fluData18thru19$Predicted[
                fluData18thru19$County==i &
                fluData18thru19$Year==j &
                fluData18thru19$Week==k
            ] <- arimaTableTest$Estimated[
                    arimaTableTest$County==i &
                    arimaTableTest$Year==j &
                    arimaTableTest$Week==k
                ]
                
        }
    }
}
fluData09thru18$Predicted <- NA
fluData09thru18$Error <- NA

# Round and Absolute Value Predicted amounts
fluData18thru19$Predicted <- round(abs(as.numeric(fluData18thru19$Predicted)))

fluData18thru19$Error <- as.numeric(fluData18thru19$Incidents) - as.numeric(fluData18thru19$Predicted)

fluDataFinal <- rbind(fluData09thru18, fluData18thru19)

fluDataFinal$Latitude <- NA
fluDataFinal$Longitude <- NA

for(i in unique(fluDataFinal$County)){
    fluDataFinal$Latitude[fluDataFinal$County==i] <- locations$Latitude[locations$County==i]
    fluDataFinal$Longitude[fluDataFinal$County==i] <- locations$Longitude[locations$County==i]
}

write.csv(fluDataFinal, file = "final.csv")
    


