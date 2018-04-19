#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(highcharter)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stocks"),
  
  
  highchartOutput("stockPlot"),
  
  hr(),
  # Sidebar with a slider input for number of bins
  fluidRow(
    column(4,
      h4("Stocks"),
      selectInput("stock",
                  "Stocks",
                  c("Fox" = "fox",
                    "Amazon" = "amazon",
                    "Facebook"= "facebook"))
    )
  )
  
  
 
    # Show a plot of the generated distribution
    
      
  
  
))
