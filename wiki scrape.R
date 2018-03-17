###### my code

library("rvest")
library(tidyverse)
rm(list = ls())
setwd("~/Documents/Stat 479")
counties = "https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents"
lines = readLines(counties) # thousands of lines of HTML
pop = grep(pattern = "text-align:right", x = lines)
county.lines = lines[pop-2]
county.lines = gsub("^.*(/wiki/)", "", county.lines)
county.lines = gsub("[[:space:]]", replacement = "", x = county.lines)
county.lines = gsub("\\\".*$", replacement = "", x = county.lines)
county.urls = paste("/wiki/", county.lines, sep="")

county.sites = paste("https://en.wikipedia.org", county.urls, sep="")



### change to full data
county.urls = county.urls[1:20]

counties.df = data_frame(county = 1:length(county.urls), links = 0)
for (i in 1:length(county.urls)) {
  temp.html = readLines(paste("https://en.wikipedia.org", county.urls[i], sep=""))
  temp.links = grep(pattern = "<a href=", value = TRUE, x = temp.html)
  temp.links = gsub("^.*a href=", "", temp.links)
  temp.links = gsub(" title.*$", "", temp.links)
  for (i in 1:length(county.urls)) {
    if (temp.links[i] == county.urls[i]) {
      links = paste(links, " ", temp.links, " ", sep = "")
    }
  }
  counties.df$links[i] = links
}

####### inside loop
# 1. read in html for all counties
# 2. narrow down to just county links
# 3. put those links in something else

####### outside loop
# 4. compare links in new list to links in old list
# 5. sparse matrix based on county link graph

# 6. interpret one of the leading eigenvectors of sparse matrix



temp.links

newlines = readLines(county.sites)

find.links = function(urls) {
  county = readLines()
  
  
  for (1:length(urls)) {
    newlines = readLines(paste("https://en.wikipedia.org", urls[i], sep=""))
    county.linklines[i] = grep(pattern = urls
  }
  lines = readLines(urls)
  county.linklines = grep(pattern = urls, lines)
  county.linklines
}



######## karl's trump code
library(data.table)
library(tidyverse)
library(tidytext)
library(Matrix)
# install.packages("glmnet")
library(glmnet)
library(randomForest)


data = fread("https://raw.githubusercontent.com/bpb27/political_twitter_archive/master/realdonaldtrump/realdonaldtrump.csv") %>% as.tbl
str(data)

text_df <- data_frame(tweet = 1:nrow(data), text = data$text)
text_df$text

# this does a lot of processing! 
#  to lower, remove @ # , . 
#  often these make sense on a first cut.
#  worth revisiting before "final results"!
tt  = text_df %>% unnest_tokens(word, text)
str(tt)

# make the document-term matrix.  
#   I sometimes call this the bag-of-words-matrix.
dt = cast_sparse(tt, tweet, word)
str(dt)
dim(dt)
hist(rowSums(dt))
cs = colSums(dt)
hist(log(cs[cs>1]))


# let's take the svd to make clusters/topics 
library(rARPACK)
A = dt; Dl = Diagonal(nrow(A), 1/sqrt(rowSums(A)+10)); Dr = Diagonal(ncol(A), 1/sqrt(colSums(A)+10))
L = Dl%*%A%*%Dr
s = svds(L, k = 10)
plot(s$d[-1])
u = s$u 

plot(as.data.frame(u[sample(nrow(A),1000),]), pch = ".")

text_df[which(s$u[,2]< -.01),]$text
colnames(dt)[which(s$v[,2]> .1)]


