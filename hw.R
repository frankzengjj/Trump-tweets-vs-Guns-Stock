library(rvest)
library(dplyr)
con_link = "https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents"
county_pg = read_html(con_link)
tb = county_pg %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>% .[[1]] %>% html_table() 

counties = tb[,1:4]
counties = counties %>% as_tibble()
##########################################################
lines = readLines(con_link) # thousands of lines of HTML
pop = grep(pattern = "text-align:right", x = lines)
county.lines = lines[pop-2]
county.lines = gsub("^.*(/wiki/)", "", county.lines)
county.lines = gsub("[[:space:]]", replacement = "", x = county.lines)
county.lines = gsub("\\\".*$", replacement = "", x = county.lines)
county.urls = paste("/wiki/", county.lines, sep="")
county.sites = paste("https://en.wikipedia.org", county.urls, sep="")
county.sites = county.sites %>% as.array()
#county.sites = county.sites %>% as_tibble()
##########################################################

state.vec = c()
for (link in county.sites) {
  state = unlist(strsplit(link, split = ",_"))[2]
  state.vec = c(state.vec, state)
}
state.vec = state.vec %>% as_tibble()
te
sep1 = unlist(strsplit(link, split = ",_"))[1]
unlist(strsplit(sep1, "/"))[5]

##########################################################
library(stringr)

scrapeLinks = function(url){  
  # given a line of cnty, it extracts the url for a wiki page of a county
  # stolen from http://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r
  #url = paste("https://en.wikipedia.org/wiki/", 
              #sub(" ", "_", x[1]),",_", sub(" ", "_", x[2]), sep="")  # the page url's are nicely structured!
  lines = try(readLines(url), silent = T)
  if(length(lines) ==0) return(c())
  html <- paste(lines, collapse="\n")
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  links <- matched[[1]][, 2]
  return(links)
}


x =  c("Autauga_County","Alabama")
scrapeLinks(x)


test = county.sites[1:10]

splitURL = function(countyList) {
  i = 1;
  li = list()
  for (url in countyList) {
    line = unlist(strsplit(url, split = "/"))[5]
    county = line %>% strsplit(split = ",_") %>% unlist() %>% .[1]
    state = line %>% strsplit(split = ",_") %>% unlist() %>% .[2]
    countyRow = c(county,state)
    li[[i]] = countyRow
    i = i+1
  }
  return(li)
}

# you need to define allCountyNames to have rows like x
# then, this next line will take a bit of time....
a = apply(county.sites, 1, scrapeLinks)

require('Matrix')
j =(unlist(a))
fj = factor(j)
p = c(0,cumsum(unlist(lapply(a, length))))

A = sparseMatrix(j = as.numeric(fj), p = p, x = rep(1, length(fj)))
dim(A)
colnames(A) = levels(fj)
rownames(A) = cnty[1:4,1]
rownames(A) = cnty[,1]

#save(A, file = "wikiCountyGraph.RData")

