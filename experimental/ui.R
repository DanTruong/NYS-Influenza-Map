# Load requisite libraries
library(leaflet)

ui <- navbarPage("NYS Influenza Map",
  id = "nav",

  tabPanel("Map", div(
    class = "outer", tags$head(
      includeCSS("styles.css"),
      includeScript("gomap.js")
    ),

    leafletOutput("map",
      width = "100%",
      height = "100%"
    ),

    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      fixed = TRUE,
      draggable = TRUE,
      top = 60,
      left = "auto",
      right = 20,
      bottom = "auto",
      width = 330,
      height = "auto",
      h2("Data Variables"),
      selectInput("seasonVal", "Season", unique(fluData$Season)),
      selectInput("weekVal", "Week", head(unique(fluData$Week), -2))
    ),
  )),

  tabPanel("Data", DT::dataTableOutput("incTable")),
  
  tabPanel("2018-2019 Predictions", div(
    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      fixed = TRUE,
      draggable = TRUE,
      #top = 60,
      top = "auto",
      left = "auto",
      right = 20,
      bottom = 60,
      width = 330,
      height = "auto",
      h2("Data Variables"),
      selectInput("countyVal", "County", unique(fluData$County)),
      h4("Dashed line represents predicted flu cases over 33 observations")
    ),
    
    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      fixed = TRUE,
      draggable = TRUE,
      top = "auto",
      left = 20,
      right = "auto",
      bottom = 60,
      width = 330,
      height = "auto",
      h2("Mean Squared Error"),
      textOutput("mse")
    ),
    
    plotOutput("predCenter", click = "plot_click")
  )),

  conditionalPanel("false", icon("crosshair"))
)
