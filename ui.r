library(shiny)
library(leaflet)
library(shinydashboard)


navbarPage("VEDAT - Visulization for Earth observation and Data Analysis Tool", id="main",
           tabPanel("Map", leafletOutput("mymap", height=1000)),
          tabPanel("Plot"),
          dashboardBody(
            box(selectInput("features", 
                            "Features: ", 
                            c("Mean", "Median", "Standard Deviation", "Max", "Min")
            ),
            width = 12
            ),
            box(plotOutput("correlation_plot"), width = 12)
          )
)

               
              


