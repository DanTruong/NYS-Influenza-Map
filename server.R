library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -76.1474, lat = 43.0481, zoom = 7) %>%
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

  output$ziptable <- DT::renderDataTable(
    fluDataCons
  )
}
