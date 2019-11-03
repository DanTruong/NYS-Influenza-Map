## DATA SOURCE
## https://health.data.ny.gov/Health/Influenza-Laboratory-Confirmed-Cases-By-County-Beg/jr8b-6gh6

## Uncomment the below lines to install missing libraries
#install.packages("tidyr")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("zoo")
#install.packages("hydroTSM")
#install.packages("shiny")
#install.packages("dataMaid")
#install.packages("rmarkdown")
#install.packages("leaflet")
#install.packages("leaflet.extras")

library(tidyr)
library(plyr)
library(dplyr)
library(zoo)
library(hydroTSM)
library(shiny)
library(dataMaid)
library(rmarkdown)
library(leaflet)
library(leaflet.extras)

## Read in Data Set
flu.data.raw <- read.csv("Influenza_Laboratory-Confirmed_Cases_By_County__Beginning_2009-10_Season.csv")

## Properly format date variable
flu.data.raw$Date <- as.Date(flu.data.raw$Week.Ending.Date, "%m/%d/%Y")

## Separate old date var into year/month/day vars
flu.data <- flu.data.raw %>%
  separate(Week.Ending.Date, sep="/", into = c("Month", "Day", "Year"))

## Convert new calendar vars into integers
flu.data$Month <- as.integer(flu.data$Month)
flu.data$Day <- as.integer(flu.data$Day)
flu.data$Year <- as.integer(flu.data$Year)

## Recreate season variable with actual season labels
flu.data <- within(flu.data, rm(Season))
flu.data$Season <- time2season(flu.data$Date, out.fmt="seasons")

## Drop unused variables
flu.data <- within(flu.data, rm(Date))
flu.data <- within(flu.data, rm(CDC.Week))
flu.data <- within(flu.data, rm(FIPS))

## Rename influenza value types
flu.data$Disease <- revalue(flu.data$Disease, c("INFLUENZA_A"="A", 
                                                "INFLUENZA_B"="B", 
                                                "INFLUENZA_UNSPECIFIED"="Unspecified"))

## Rename season value types
flu.data$Season <- revalue(flu.data$Season, c("autumm"="Fall", 
                                                "winter"="Winter", 
                                                "spring"="Spring"))

## Split County.Centroid into separate Latitude/Longitude variables
flu.data <- flu.data %>%
  separate(County.Centroid, c("Latitude", "Longitude"), ", ")

## Remove the parentheses from both values
flu.data$Latitude <- substring(flu.data$Latitude, first = 2)
flu.data$Longitude <- substring(flu.data$Longitude, 1, nchar(flu.data$Longitude) - 1)

## Convert coordinates from character to double
flu.data$Latitude <- as.double(flu.data$Latitude)
flu.data$Longitude <- as.double(flu.data$Longitude)



flu.data <- within(flu.data, rm(Month))
flu.data <- within(flu.data, rm(Day))
flu.data <- within(flu.data, rm(Region))
#aggregate(flu.data$Count, by=flu.data$Year, FUN=sum)
#coordinates <- data.frame(County=flu.data$County, Latitude=flu.data$Latitude, Longitude=flu.data$Longitude)
#flu.data <- within(flu.data, rm(Latitude))
#flu.data <- within(flu.data, rm(Longitude))
#coordinates <- unique(coordinates)

flu.data.final <- aggregate(Count ~ ., flu.data, sum)

####### CODEBOOK GENERATION ########

## Uncomment to create codebook of flu data
#attr(flu.data$Region, "shortDescription") <- "NYS Region where the data was collected"
#attr(flu.data$County, "shortDescription") <- "County where the data was collected"
#attr(flu.data$Month, "shortDescription") <- "Month of occurrence recording"
#attr(flu.data$Day, "shortDescription") <- "Day of occurrence recording"
#attr(flu.data$Year, "shortDescription") <- "Year of occurrence recording"
#attr(flu.data$Disease, "shortDescription") <- "Distinction of Influenza Type A, B or Unspecified"
#attr(flu.data$Count, "shortDescription") <- "Amount of flu occurrences"
#attr(flu.data$Latitude, "shortDescription") <- "Latitude Coordinates of the County where the data was recorded"
#attr(flu.data$Longitude, "shortDescription") <- "Longitude Coordinates of the County where the data was recorded"
#attr(flu.data$Season, "shortDescription") <- "The season for which the flu data was recorded"
#makeCodebook(flu.data)

####### SHINY INIT CODE #############
ui <- fluidPage(
  
  ## Application title
  titlePanel("Map of Influenza Incidents in NYS 2009 - 2019"),

  ## Year selector
  selectInput(inputId = "yearVal",
              label = "Year", 
              sort(unique(flu.data$Year))
  ),
  
  ## Flu type selector
  selectInput(inputId = "diseaseVal",
              label = "Influenza Type", 
              sort(unique(flu.data$Disease))
              ),
  
  ## Season selector
  selectInput(inputId = "seasonVal",
              label = "Season", 
              sort(unique(flu.data$Season))
  ),
  
  ## Table output of selected values for flu data
  #tableOutput("dTable")
  
  ## Map output
  leafletOutput(outputId = "nysMap")
)

server <- function(input, output){
  ## Create table of flu data based on user selection
  #output$dTable <- renderTable(
  #  flu.data[flu.data$Season == trimws(input$seasonVal) & 
  #             flu.data$Year == as.integer(input$yearVal) & 
  #             flu.data$Disease == trimws(input$diseaseVal), ]
  #)
  
  output$nysMap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -76.1474, lat = 43.0481, zoom = 7) %>%
      addTiles() %>%
      #addCircles(data = flu.data, 
      #           lng = flu.data$Longitude,
      #           lat = flu.data$Latitude,
      #           radius = flu.data$Count[flu.data$Season == trimws(input$seasonVal) &
      #                                     flu.data$Year == as.integer(input$yearVal) & 
      #                                     flu.data$Disease == trimws(input$diseaseVal)] * 25)
      addCircles(data = flu.data.final, 
                  lng = flu.data.final$Longitude,
                  lat = flu.data.final$Latitude,
                  radius = flu.data.final$Count[flu.data.final$Season == trimws(input$seasonVal) &
                                               flu.data.final$Year == as.integer(input$yearVal) & 
                                               flu.data.final$Disease == trimws(input$diseaseVal)] * 10)
  })
}

shinyApp(ui = ui, server = server)
