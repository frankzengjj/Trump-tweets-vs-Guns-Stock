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
library(DT)

# Define server logic required to draw a histogram

sDate <- as.Date("2018-03-20")
tp <- trumpTw %>% select(created,text,sentiment,retweetCount, favoriteCount)
tp$created = tp$created %>% as.character.Date()

trumpTw = trumpTw %>% arrange(-desc(created))
temp = trumpTw
temp$created = as.Date(temp$created)
scores = temp %>% group_by(created) %>% summarise(score = mean(sentiment))

shinyServer(function(input, output) {
  getSymbols("FOXA", from = sDate)
  getSymbols("AMZN", from = sDate)
  getSymbols("FB", from = sDate)
  output$stockPlot <- renderHighchart({
    if (input$stock == "fox") {
      dat = FOXA
    } else if (input$stock == "amazon") {
      dat = AMZN
    } else {
      dat = FB
    }
    
    
    data_flags <- data_frame(
      date = scores$created,
      title = sprintf("E #%s", seq_along(date)),
      text = sprintf("Mean sentiment score in %s is %s", date, 
                     round(scores$score[seq_along(date)], digits = 3))
    )
    
    hc <- highchart(type = "stock") %>%
      hc_title(text = "Overall") %>% 
      hc_subtitle(text = "Data extracted using quantmod package") %>% 
      hc_add_series(dat, id = "stock", type = "line")
    
    hc %>% 
      hc_add_series(data_flags, hcaes(x = date),
                    type = "flags", onSeries = "stock")
    
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
      geom_vline(xintercept = trumpTw$created, col = "black") +
      geom_line(data = dat, mapping = aes(x = dat$Date_Time, y = dat$Open), col="red")+
      ggtitle("Stock price vs Trump tweets")
      
  })
  
  output$trumpTweets <- DT::renderDataTable(
    datatable(as.data.frame(tp), 
              filter="top", selection="multiple", escape=FALSE, 
              options = list(sDom  = '<"top">lrt<"bottom">ip'))
  )
  
  output$tweetSentiment <- DT::renderDataTable(
    datatable(as.data.frame(tp %>% select(created, text, sentiment)), 
              filter="top", selection="multiple", escape=FALSE, 
              options = list(sDom  = '<"top">lrt<"bottom">ip'))
    )
  
  output$tweetscout <- renderHighchart({
    
    highchart() %>% 
      hc_xAxis(categories = as.character.Date(tp$created)) %>% 
      hc_add_series(name = "Retweet count", data = trumpTw$retweetCount, type = "line") %>% 
      hc_add_series(name = "Favorite Count", data = trumpTw$favoriteCount, type = "line") 
    
  })
})
