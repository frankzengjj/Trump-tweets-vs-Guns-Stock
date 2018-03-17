library(twitteR)
library(tidyverse)
library(tidyquant)  

consumer_key <- "z0f8WnP1dGcAogS2o7GKyp2Ui"
consumer_secret <- "pzeAdwLUkrXZvZx41ex2iSrDqMyXgCI7CIqMDPCyNrucG1wqto"
access_token <- "961467148882382849-ZxNXYtgTsDWlixwhqKIVaHDh2LUkMd6"
access_secret <- "gCXGDndWDuCGSTfLaq8TrhnWzfrUlyyCkoNxzSG3xwSRK"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

user = getUser('realDonaldTrump')
userTl = userTimeline(user, n=3200, includeRts = TRUE)
trumpTw = userTl %>% twListToDF() %>% as.tibble()

trumpTw %>% filter(str_detect(text,'guns|shooting')) %>% select(text,created)


gunstock = tq_get("AOBC", get = "stock.prices", from = "2015-09-01", to = "2018-2-10")
gunstock %>% 
  ggplot(aes(x = date, y = close)) +
  geom_line() +
  labs(title = "AOBC Line Chart", y = "Closing Price", x = "") 
