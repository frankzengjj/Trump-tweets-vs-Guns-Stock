###### my code


library(tidyverse)
library(data.table)
rm(list = ls())
setwd("~/Documents/Stat 479")
counties = "https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents"

# URLs for all county wikipedia pages
lines = readLines(counties) # thousands of lines of HTML
pop = grep(pattern = "text-align:right", x = lines)
county.lines = lines[pop-2]
county.lines = gsub("^.*(/wiki/)", "", county.lines)
county.lines = gsub("[[:space:]]", replacement = "", x = county.lines)
county.lines = gsub("\\\".*$", replacement = "", x = county.lines)
county.urls = paste("https://en.wikipedia.org/wiki/", county.lines, sep="")

# scrape general county links
scrape.links = function(url) {
  html = readLines(url)
  links = grep(pattern = "href=\\\"/wiki/", x = html, value = TRUE)
  links = grep(pattern = ",_", x = links, value = TRUE)
  c = gsub("^.*(/wiki/)", "", links)
  c = gsub(" ", replacement = "", x = c)
  c = gsub("\\\".*$", replacement = "", x = c)
  index = grep(pattern = "Area", x = c)
  c = c[-index]
  com = grep(pattern = ",_", x = c)
  c = c[com]
  lower = grep(pattern = "area", x = c)
  if (length(lower) > 0) {
    c = c[-lower]
  }
  colons = grep(pattern = ":", x = c)
  if (length(colons) > 0) {
    c = c[-colons]
  }
  paste("https://en.wikipedia.org/wiki/", c, sep="")
}


test = county.urls[1:9]
t = gsub("^.*/wiki/", "", test)
t = gsub("_", " ", t)
tib = as_data_frame(cbind(t, test))
a = apply(tib[2], 1, scrape.links)


j = unlist(a)
fj = factor(j)
p = c(0,cumsum(unlist(lapply(a, length))))
A = sparseMatrix(j = as.numeric(fj), p = p, x = rep(1, length(fj)))
dim(A)
colnames(A) = levels(fj)
rownames(A) = tib$t



library(rARPACK)
Dl = Diagonal(nrow(A), 1/sqrt(rowSums(A)+10)); Dr = Diagonal(ncol(A), 1/sqrt(colSums(A)+10))
L = Dl%*%A%*%Dr
s = svds(L, k = 10)

plot(s$d[-1])
u = s$u 
plot(as.data.frame(u[sample(nrow(A),1000),]), pch = ".")
