## Load in requisite libraries
library(shiny)
library(hydroTSM)
library(tidyr)
library(dplyr)
library(plyr)
library(leaflet)
library(leaflet.extras)

####### DATASET CLEANUP #############
## Source: https://health.data.ny.gov/Health
#           /Influenza-Laboratory-Confirmed-Cases-By-County-Beg/jr8b-6gh6
fluDataRaw <- read.csv("data.csv")

## Pull in select columns from raw
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
fluData$Disease <- revalue(fluData$Disease, c("INFLUENZA_A" = "A", 
                                              "INFLUENZA_B" = "B", 
                                              "INFLUENZA_UNSPECIFIED" = "Unspecified"))

## Work with consolidated data
fluDataCons <- fluData[-c(3)]
fluDataCons  <- aggregate(Incidents ~ ., fluDataCons, sum)

####### SHINY INIT CODE #############

ui <- fluidPage(

    ## Application title
    titlePanel("Map of Influenza Incidents in NYS 2010 - 2019"),

    ## Define UI with controls for left-side dominance
    sidebarLayout(
        
        ## Sidebar of input controls
        sidebarPanel(

            ## Input for year selection
            selectInput(inputId = "yearVal",
                        label = "Year", 
                        2010:2019
            ),
            
            ## Input for month selection
            selectInput(inputId = "monthVal",
                        label = "Month", 
                        sort(unique(fluDataCons$Month))
            ),
            
            ## Input for flu type selection
            selectInput(inputId = "diseaseVal",
                        label = "Influenza Type", 
                        sort(unique(fluDataCons$Disease))
            ),
        
        ),

        ## Display map and table (debug) in the main area (to the right)
        mainPanel(
            leafletOutput(outputId = "nysMap")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    ## Render map on the interface
    output$nysMap <- renderLeaflet({
        leaflet() %>%
            
            ## Set default map view over Syracuse, NY
            setView(lng = -73.97401, lat = 42.58827, zoom = 6) %>%
            addTiles() %>%
            
            ## Add magnitude circles based on user interface values
            addCircles(
                data = fluDataCons[
                    fluDataCons$Year == as.integer(input$yearVal) & 
                    fluDataCons$Month == as.integer(input$monthVal) & 
                    fluDataCons$Disease == trimws(input$diseaseVal),
                ],
                lat = ~ Latitude,
                lng = ~ Longitude,
                radius = ~ Incidents * 50,
                popup = ~ as.character(paste0(County, " County: ", Incidents, " Flu Cases")),
                label = ~ as.character(paste0(County, " County: ", Incidents, " Flu Cases"))
            )
    })
}

## Run the application 
shinyApp(ui = ui, server = server)
