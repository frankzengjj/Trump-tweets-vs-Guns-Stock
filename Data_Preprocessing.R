library(twitteR)
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)
#command lines to install the exploratory package
#1. install.packages("devtools")
#2. devtools::install_github("exploratory-io/exploratory_func")
library(exploratory)
library(quantmod)
data("stop_words")

# Calling the Twitter API
consumer_key <- "z0f8WnP1dGcAogS2o7GKyp2Ui"
consumer_secret <- "pzeAdwLUkrXZvZx41ex2iSrDqMyXgCI7CIqMDPCyNrucG1wqto"
access_token <- "961467148882382849-ZxNXYtgTsDWlixwhqKIVaHDh2LUkMd6"
access_secret <- "gCXGDndWDuCGSTfLaq8TrhnWzfrUlyyCkoNxzSG3xwSRK"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

user = getUser('realDonaldTrump')
# Getting the tweets
userTl = userTimeline(user, n=100, includeRts = FALSE, sinceID = "2018-4-10")
trumpTw = userTl %>% twListToDF() %>% as_tibble()

# clean for enabling search function in data table
tex = tibble(clean_text = character())
trumpTw = trumpTw %>% mutate(obs = 1:n())
for (tw in trumpTw$text) {
  tw <- iconv(tw, to = "ASCII", sub = " ")
  tw <- gsub("rt", " ", tw)  # Remove the "RT" (retweet) so duplicates are duplicates
  tw <- gsub("@\\w+", " ", tw)  # Remove user names (all proper names if you're wise!)
  tw <- gsub("http.+ |http.+$", " ", tw)  # Remove links
  tw <- gsub("[[:punct:]]", " ", tw)  # Remove punctuation
  tw <- gsub("[ |\t]{2,}", " ", tw)  # Remove tabs
  tw <- gsub("amp", " ", tw)  # "&" is "&amp" in HTML, so after punctuation removed ...
  tw <- gsub("^ ", "", tw)  # Leading blanks
  tw <- gsub(" $", "", tw)  # Lagging blanks
  tw <- gsub(" +", " ", tw) # General spaces (should just do all whitespaces no?)
  tw <- tolower(tw)
  
  tex = tex %>% add_row(clean_text= tw)
}

tex = tex %>% mutate(obs = 1:n())
trumpTw = trumpTw %>% inner_join(tex, by = "obs") 
#trumpTw = trumpTw %>% select(-one_of("obs"))
trumpTw = trumpTw %>% mutate(sentiment = get_sentiment(trumpTw$clean_text))

# function to retrieve stock price
stockprices = function(ticker, period, days) {
  url = paste("https://www.google.com/finance/getprices?i=",period,"&p=",
              days,"d&f=d,o,h,l,c,v&df=cpct&q=",ticker, sep="")
  price = read.csv(url, skip=7, header=FALSE, stringsAsFactors = FALSE)
  start.idx = which(substring(price$V1, 1, 1) == 'a')
  end.idx = c(start.idx[-1] - 1, nrow(price))
  idx.range = paste(start.idx, ':', end.idx, sep = '')
  start.time = as.numeric(substring(price[start.idx, 1], 2))
  price[start.idx, 1] = 0
  alltimes.idx = do.call(c, lapply(seq(1, length(idx.range)),
                                   function(i) {
                                     start.time[i] + period * as.numeric(price[eval(parse(text = idx.range[i])), 1])
                                   })
  )
  price[,1] = as.POSIXct(alltimes.idx, origin = '1970-01-01', tz = 'UTC')
  colnames(price) = c('Date_Time', 'Open', 'High', 'Low', 'Close', 'Volume')
  price
}

amazon = stockprices("AMZN", 60, "2d")
facebook = stockprices("FB", 60, "2d")
fox = stockprices("FOXA", 60, "2d")
