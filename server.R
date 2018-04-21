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
    
    hc <- highchart(type = "stock") %>%
      hc_title(text = "Overall") %>% 
      hc_subtitle(text = "Data extracted using quantmod package") %>% 
      hc_add_series(dat, id = "stock", type = "line")
    
  })
  
  output$stockvstrump <- renderPlot({
    if (input$stock.1m == "fox") {
      dat = fox
    } else if (input$stock.1m == "amazon") {
      dat = amazon
    } else {
      dat = facebook
    }
    
    ggplot() + 
      geom_vline(xintercept = trumpTwText$created, col = "black") +
      geom_line(data = dat, mapping = aes(x = dat$Date_Time, y = fox$Open), col="red") 
      
  })
  
})
