#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Stock Price"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("variable", "Variable:",
                    c("Fox" = "fox",
                      "Amazon" = "amazon",
                      "Facebook" = "facebook"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         highchartOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$distPlot <- renderHighchart({
     if(input$variable == "fox") {
       dat = fox
     } else if (input$variable == "amazon") {
       dat = amazon
     } else {
       dat = facebook
     }
     highchart() %>% 
       hc_xAxis(categories = dat$Date_Time) %>% 
       hc_add_series(name = input$variable, data = dat$Open)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
