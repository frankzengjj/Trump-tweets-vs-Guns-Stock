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
               mainPanel(
                plotOutput("stockvstrump", width = "150%"),
                highchartOutput("tweetscout", width = "150%"),
                DT::dataTableOutput("trumpTweets", width = "150%"))
               
             )
          
    ),
    tabPanel("plot2", fluid = TRUE,
             sidebarLayout(fluidPage(
               selectInput("stock",
                           "Stocks",
                           c("Fox" = "fox",
                             "Amazon" = "amazon",
                             "Facebook"= "facebook"))),
              
                mainPanel(fluidPage(
                  highchartOutput("stockPlot", width = "150%"),
                  DT::dataTableOutput("tweetSentiment", width = "150%")))
                
             )
    )
    
  )

  # Sidebar with a slider input for number of bins

  
    # Show a plot of the generated distribution
    
      
  
  
))
