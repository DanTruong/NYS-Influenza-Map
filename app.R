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
                      Count = as.integer(fluDataRaw$Count),                 
                      Coordinates = fluDataRaw$County.Centroid)

## Create weather season variable
#fluData$Season <- time2season(fluData$Date, out.fmt = "seasons")
#fluData$Season <- revalue(fluData$Season, c("autumm" = "Fall", 
#                                            "winter" = "Winter", 
#                                            "spring" = "Spring"))

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
fluDataCons  <- aggregate(Count ~ ., fluDataCons, sum)

####### SHINY INIT CODE #############

ui <- fluidPage(

    ## Application title
    titlePanel("Map of Influenza Incidents in NYS 2009 - 2019"),

    ## Define UI with controls for left-side dominance
    sidebarLayout(
        
        ## Sidebar of input controls
        sidebarPanel(

            ## Input for year selection
            selectInput(inputId = "yearVal",
                        label = "Year", 
                        sort(unique(fluDataCons$Year))
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
            leafletOutput(outputId = "nysMap"),
            tableOutput("dTable")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    ## Render map on the interface
    output$nysMap <- renderLeaflet({
        leaflet() %>%
            
            ## Set default map view over Syracuse, NY
            setView(lng = -76.1474, lat = 43.0481, zoom = 7) %>%
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
                radius = ~ Count * 25,
                popup = ~ as.character(Count)
            )
    })
    
    ## Render table for value debugging
    output$dTable <- renderTable(
        fluDataCons[
            fluDataCons$Month == as.integer(input$monthVal) & 
            fluDataCons$Year == as.integer(input$yearVal) & 
            fluDataCons$Disease == trimws(input$diseaseVal),
        ]
    )
}

## Run the application 
shinyApp(ui = ui, server = server)
