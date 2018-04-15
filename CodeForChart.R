rm(list = ls())
setwd("~/Documents/Stat 479")
library(twitteR)
library(tidyverse)
library(tidyquant)
library(stringi)
library('quantmod')
library('xts')
library('TTR')
library(lubridate)

consumer_key = "z0f8WnP1dGcAogS2o7GKyp2Ui"
consumer_secret = "pzeAdwLUkrXZvZx41ex2iSrDqMyXgCI7CIqMDPCyNrucG1wqto"
access_token = "961467148882382849-ZxNXYtgTsDWlixwhqKIVaHDh2LUkMd6"
access_secret = "gCXGDndWDuCGSTfLaq8TrhnWzfrUlyyCkoNxzSG3xwSRK"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

user = getUser('realDonaldTrump')
userTl = userTimeline(user, n=100, includeRts = TRUE)
trumpTw = userTl %>% twListToDF() %>% as.tibble()
trumpTwText = trumpTw %>% select(text,created)
trumpTwText$created = as.POSIXct(trumpTwText$created, usetz=TRUE, tz="GMT")
force_tz(trumpTwText$created, "America/New_York")

# Get intra-day stock prices from Google Finance.
# URL format is: https://www.google.com/finance/getprices?i=[PERIOD]
#   &p=[DAYS]d&f=d,o,h,l,c,v&df=cpct&q=[TICKER]

# [PERIOD]: length of one interval in seconds (60 seconds is lowest)
# [DAYS]: data period in character string ("10d" is past 10 days)
# [TICKER]: stock ticker symbol in character string ("AAPL" is for Apple)

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
  price[,1] = as.POSIXct(alltimes.idx, origin = '1970-01-01', tz = 'EST')
  colnames(price) = c('Date_Time', 'Open', 'High', 'Low', 'Close', 'Volume')
  price
}

amazon = stockprices("AMZN", 60, "4d")
lockheed = stockprices("LMT", 60, "4d")
gm = stockprices("GM", 60, "4d")
apple = stockprices("AAPL", 60, "4d")
facebook = stockprices("FB", 60, "4d")
fox = stockprices("FOXA", 60, "1d")

amazon



ggplot() + 
  theme_classic() +
  geom_vline(xintercept = trumpTwText$created, col = "black") +
  #geom_line(data = amazon, mapping = aes(x = amazon$Date_Time, y = amazon$Open), col="#ff9900") + 
  #geom_line(data = lockheed, mapping = aes(x = lockheed$Date_Time, y = lockheed$Open), col="#005bad") +
  #geom_line(data = gm, mapping = aes(x = gm$Date_Time, y = gm$Open), col="#696969") +
  #geom_line(data = apple, mapping = aes(x = apple$Date_Time, y = apple$Open), col="orange") +
  #geom_line(data = facebook, mapping = aes(x = facebook$Date_Time, y = facebook$Open), col="lightblue") +
  geom_line(data = fox, mapping = aes(x = fox$Date_Time, y = fox$Open), col="red")

#' References
#' https://www.quantshare.com/sa-426-6-ways-to-download-free-intraday-and-tick-data-for-the-us-stock-market
#' @frederickpelchat
#'   https://github.com/frederickpelchat/quantitative-finance/blob/master/intraday-data.R



amazon
open = amazon$Open
n = length(open)
ret = open[-n]/open[-1] - 1
vol = sd(ret) * sqrt(4) * 100
#measured volatility




