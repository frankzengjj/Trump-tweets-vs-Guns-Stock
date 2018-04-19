#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(highcharter)
library(quantmod)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$stockPlot <- renderHighchart({
    if (input$stock == "fox") {
      dat = fox
    } else if (input$stock == "amazon") {
      dat = amazon
    } else {
      dat = facebook
    }
    
    data_flags <- data_frame(
      date = sample(trumpTw$created, size = 5),
      title = sprintf("E #%s", seq_along(date)),
      text = sprintf("An interesting event #%s in %s", seq_along(date), date)
    )
    
    
    hc <- highchart(type = "stock") %>% 
      hc_xAxis(type = 'datetime') %>% 
      hc_title(text = "Charting some Symbols") %>% 
      hc_subtitle(text = "Data extracted using quantmod package") %>% 
      hc_add_series(dat, id = "stock", type = "line")
      
   
  })
  
})
