library(shiny)
library(leaflet)
library(DT)
library(rgdal)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(ggplot2)

shinyServer(function(input, output) {
  
  
   #to read the csv file
  mydata <- read.csv("Data/No2_data.csv",stringsAsFactors = FALSE )
  mydata <- data.frame(mydata)
  mydata$Latitude <-  as.numeric(mydata$Latitude)
  mydata$Longitude <-  as.numeric(mydata$Longitude)
 
  
  mydata <- mutate(mydata, cntnt=paste0('<strong>State:</strong> ',mydata$State,
                                        '<br><strong>Longitude:</strong> ',mydata$Longitude,
                                        '<br><strong>Latitude:</strong> ',mydata$Latitude,
                                        '<br><strong>01-Apr:</strong> ', mydata$X01.Apr,
                                        '<br><strong>02-Apr:</strong> ', mydata$X02.Apr,
                                        '<br><strong>03-Apr:</strong> ', mydata$X03.Apr,
                                        '<br><strong>04-Apr:</strong> ', mydata$X04.Apr,
                                        '<br><strong>05-Apr:</strong> ', mydata$X05.Apr,
                                        '<br><strong>06-Apr:</strong> ', mydata$X06.Apr,
                                        '<br><strong>07-Apr:</strong> ', mydata$X07.Apr,
                                        '<br><strong>08-Apr:</strong> ', mydata$X08.Apr,
                                        '<br><strong>09-Apr:</strong> ', mydata$X09.Apr,
                                        '<br><strong>10-Apr:</strong> ', mydata$X10.Apr,
                                        '<br><strong>11-Apr:</strong> ', mydata$X11.Apr,
                                        '<br><strong>12-Apr:</strong> ', mydata$X12.Apr,
                                        '<br><strong>13-Apr:</strong> ', mydata$X13.Apr,
                                        '<br><strong>14-Apr:</strong> ', mydata$X14.Apr,
                                        '<br><strong>15-Apr:</strong> ', mydata$X15.Apr,
                                        '<br><strong>16-Apr:</strong> ', mydata$X16.Apr,
                                        '<br><strong>17-Apr:</strong> ', mydata$X17.Apr,
                                        '<br><strong>18-Apr:</strong> ', mydata$X18.Apr,
                                        '<br><strong>19-Apr:</strong> ', mydata$X19.Apr,
                                        '<br><strong>20-Apr:</strong> ', mydata$X20.Apr,
                                        '<br><strong>21-Apr:</strong> ', mydata$X21.Apr,
                                        '<br><strong>22-Apr:</strong> ', mydata$X22.Apr,
                                        '<br><strong>23-Apr:</strong> ', mydata$X23.Apr,
                                        '<br><strong>24-Apr:</strong> ', mydata$X24.Apr,
                                        '<br><strong>25-Apr:</strong> ', mydata$X25.Apr,
                                        '<br><strong>26-Apr:</strong> ', mydata$X26.Apr,
                                        '<br><strong>27-Apr:</strong> ', mydata$X27.Apr,
                                        '<br><strong>28-Apr:</strong> ', mydata$X28.Apr,
                                        '<br><strong>29-Apr:</strong> ', mydata$X29.Apr,
                                        '<br><strong>30-Apr:</strong> ', mydata$X30.Apr
                                         ))
  
#leaflet map
  output$mymap <- renderLeaflet({
                    leaflet(mydata) %>% 
                     addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
                     addTiles() %>%
                     addCircleMarkers(data = mydata, lat =  ~Latitude, lng =~Longitude, radius = 3,
                                      popup = ~as.character(cntnt), 
                                      stroke = FALSE, fillOpacity = 0.8)%>%
                      fitBounds( ".", lng1=68.7,lat1=8.4,lng2=97.25,lat2=36.7 ) %>%
                      addProviderTiles(providers$OpenStreetMap)%>%
                       addEasyButton(easyButton(
                                     icon="fa-crosshairs", title="ME",
                                      onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
                      })
  
  #plot
  output$correlation_plot<-renderPlot({
    mydata<-read.csv("Data/Plot.csv")
    cumulative_data <- mydata %>% group_by(State) %>% mutate(cum_sum_mean = cumsum(Mean), 
                                                             cum_sum_median = cumsum(Median),
                                                             cum_sum_std_dev = cumsum(Std_Dev),
                                                             cum_sum_max = max(Max),
                                                             cum_sum_min = min(Min))
    
    filtered_data <- cumulative_data[!duplicated(cumulative_data$State), ]
    
    if (input$features == "Mean") {
      y_axis_value = filtered_data$cum_sum_mean
    } else if (input$features == "Median") {
      y_axis_value = filtered_data$cum_sum_median
    } else if (input$features == "Standard Deviation") {
      y_axis_value = filtered_data$cum_sum_std_dev
    }  else if (input$features == "Max") {
      y_axis_value = filtered_data$cum_sum_max
    } else {
      y_axis_value = filtered_data$cum_sum_min
    }
    
    ggplot(data.frame(filtered_data$State, y_axis_value), 
           aes(filtered_data$State, y_axis_value)) +
      geom_line() +
      geom_point() + 
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(x = "STATE NAME") + 
      labs(y = "NO2")
  })
  
  
})




 
  
  
  
  