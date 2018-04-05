rm(list = ls())
setwd("~/Documents/Stat 479")
library(twitteR)
library(tidyverse)
library(tidyquant)
library(stringi)
library('quantmod')
library('xts')
library('TTR')

consumer_key = "z0f8WnP1dGcAogS2o7GKyp2Ui"
consumer_secret = "pzeAdwLUkrXZvZx41ex2iSrDqMyXgCI7CIqMDPCyNrucG1wqto"
access_token = "961467148882382849-ZxNXYtgTsDWlixwhqKIVaHDh2LUkMd6"
access_secret = "gCXGDndWDuCGSTfLaq8TrhnWzfrUlyyCkoNxzSG3xwSRK"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

user = getUser('realDonaldTrump')
userTl = userTimeline(user, n=100, includeRts = TRUE)
trumpTw = userTl %>% twListToDF() %>% as.tibble()
trumpTwText = trumpTw %>% select(text,created)
trumpTwText$created = format(trumpTwText$created,usetz=TRUE, tz="EST")


substring(as.character(trumpTwText$created), first = 0, last = nchar(trumpTwText$created) - 4)
amazon

as.character(trumpTwText$created)

nchar(trumpTwText$created[1])
str(trumpTwText$created)
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
  price.dt = xts(price[,-1], as.POSIXct(alltimes.idx, origin = '1970-01-01', tz = 'GMT'))
  indexTZ(price.dt) = 'America/New_York'
  colnames(price.dt) = c('Open', 'High', 'Low', 'Close', 'Volume')
  price.dt
}

amazon = stockprices("AMZN", 60, "3d")

plot(amazon$Open, type = "l") + abline(v = trumpTwText$created, col="red")

#' References
#' https://www.quantshare.com/sa-426-6-ways-to-download-free-intraday-and-tick-data-for-the-us-stock-market
#' @frederickpelchat
#'   https://github.com/frederickpelchat/quantitative-finance/blob/master/intraday-data.R
