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
  titlePanel("Trump vs Stocks"),
  
  tabsetPanel(
    tabPanel("plot1", fluid = TRUE,
             sidebarLayout(fluidPage(
               selectInput("stock.1m",
                           "Stocks",
                           c("Fox" = "fox",
                             "Amazon" = "amazon",
                             "Facebook"= "facebook"))),
               plotOutput("stockvstrump")
             )
    ),
    tabPanel("plot2", fluid = TRUE,
             sidebarLayout(fluidPage(
               selectInput("stock",
                           "Stocks",
                           c("Fox" = "fox",
                             "Amazon" = "amazon",
                             "Facebook"= "facebook"))),
              
                highchartOutput("stockPlot")
             )
      
    )
  )

  # Sidebar with a slider input for number of bins

  
    # Show a plot of the generated distribution
    
      
  
  
))
