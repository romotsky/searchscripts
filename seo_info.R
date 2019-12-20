## SEO scripts
# install.packages(c("xlsx","SEMrushR", "searchConsoleR", "kwClustersR", "duplicateContentR", "majesticR", "text2vec", "eVenn", 
                 # "tm", "shiny", "httr", "googleAnalyticsR", "tidyverse", "devtools"))

# install.packages("googleAnalyticsR")
# devtools::install_github("dschmeh/seoR")

if(!require(seoR)) install.packages("seoR",
                                         repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest",
                                         repos = "http://cran.us.r-project.org")
if(!require(googleAnalyticsR)) install.packages("googleAnalyticsR",
                                     repos = "http://cran.us.r-project.org")




getwd()

newwd <- '/Users/danielromotsky/Dropbox/Portfolio/searchscripts' ## add in folder where your URL list (in csv format) lives

setwd(newwd) # set working directory

list.files()
urlfilename <- "sample_tracker.csv" #make sure the column with your urls is called "Full_page"

csv <- read.csv(urlfilename) # read in urls and format
pulledURLs <- csv %>% select(Full_page) %>% pull() 

URLs <- as.character(pulledURLs) # format

###############################
# grab title and description tags
save <- t(sapply(URLs, function(x) {
  title <- HTMLtitle(x)[1]
  # title2 <- HTMLtitle(x)[2]
  desc1 <- HTMLdescription(x)[1]
  desc2 <- HTMLdescription(x)[2] #to check for duplicate meta descriptions
  rbind(title, desc1, desc2)
}))

save <- as.data.frame(save) #format it

names(save) <- c("Title", "Description", "Description #2")

output <- "meta_output.csv" #name the file accordingly
write.csv(save, output) #outputs to csv

############################
## WEBSCRAPING ##
## inspect target Keywords using rvest package

URLs <- c("https://www.google.com/", "https://www.youtube.com/")

kws <- sapply(URLs, FUN = function(x){
  readit <- read_html(x)
  # vignette("selectorgadget")
  nodes <- html_nodes(readit, "meta") #using rvest 
  names <- html_attr(nodes, "name")
  content <- html_attr(nodes, "content")
  index <- names %in% c("keywords", "description")
  content[index]
})

kws

s <- lapply(URLs, FUN = function(y){
  name <- URLs 
  readit <- read_html(y)
  nodes <- html_nodes(readit, "meta")
  names <- html_attr(nodes, "name")
  content <- html_attr(nodes, "content")
  index <- names %in% c("description", "keywords")
  content[index]
})
  names(s) <- URLs

s_u <- unlist(s)
kw_desc <- "keywords_meta_sample.csv" # filename to save
write.csv(s_u, kw_desc)


##alt tags##
## helpful to find which images are missing alt tags!!##

nodes <- html_nodes(readit, "img") #using rvest

## loop to grab all tags and descriptions
final <- data.frame()
 for (i in 1:length(URLs)) {
  nodes <- html_nodes(read_html(URLs[i]), "img")
  values <- sapply(nodes, FUN = function(x){
    src <- html_attr(x, "src")
    alt <- html_attr(x, "alt")
    combine <- t(c(src = src, alt = alt))
   })
  values <- t(values)
  u <- rep(URLs[i], length(values[,1]))
  values <- cbind(u, values)
  final <- rbind(final, values)
  # fin <- cbind(URLs[i], combine)
 }
names(final) <- c("URL", "image", "alt tag")

head(final)

getwd()

alt_name <- "alt_tag_sample.csv" #name your file
write.csv(final, alt_name)

# other useful info
unlist(c(URLs[1], HTMLtitle(URLs[1]), HTMLtitle_length(URLs[1]), 
  responseCode(URLs[1]), HTMLdescription(URLs[1]),
  HTMLdescription_length(URLs[1]), HTMLrobots(URLs[1]),
  htag(URLs[1], hTag = "h2"), htag_count(URLs[1], hTag = "h3"),
  HTMLcanonical(URLs[1]), hrefLang(URLs[1])))

# internal links
extractLinks(URLs[1], linkType = "internal", uniqueLinks = TRUE)

# external links
extractLinks(URLs[1], linkType = "external", uniqueLinks = TRUE)

linkCount(URLs[1], linkType = "internal", uniqueLinks = FALSE) # all external, internal

## check for url in sitemap xml
url3<-"https://www.r-bloggers.com/combining-faa-and-stepwise-correlation/"
sitemap<-"https://www.r-bloggers.com/sitemap.xml"
urlInSitemap(url3, sitemap)

# keyword suggestion
keyword<-"shirt"
keywordResults(keyword, searchengine = "google")
googleSuggest(keyword, language = "en", walkThrough = FALSE)

transactionalSuggest(keyword = "t shirt", language = "en_US", page = "amazon")

pagesInIndex(URLs[1])

lastCached(URLs[1])


## even BING results!
getBingResults("shirt")

## wikipedia info
wikipediaTraffic("shirt",'2018-01-01','2018-01-10')

w3cValidate(URLs[1])

domainAge(URLs[1])

# page speed monitoring
pageSpeed(URLs[1])

#MOZ!

Access_ID<- "INSERT YOURS"
Secret_Key<- "INSERT YOURS"
mozUrlMetrics(url2, Access_ID, Secret_Key)

mozLinkMetrics(URLS[1], Access_ID, Secret_Key, Scope = "page_to_page", Limit = 10, Filter = "",Sort = "",SourceCols = "536870916",TargetCols = "536870916", LinkCols = "")


## google analytics
## you'll need to set up your own authentication!
# GA_CLIENT_ID GA_CLIENT_SECRET GA_WEB_CLIENT_ID GA_WEB_CLIENT_SECRET GA_AUTH_FILE
# ga_auth()

## get your accounts
account_list <- ga_account_list()

## account_list will have a column called "viewId"
account_list$viewId

## View account_list and pick the viewId you want to extract data from. 
ga_id <- account_list$viewId[1]

## simple query to test connection, get 10 rows
total_sessions <- google_analytics(ga_id,
                 date_range = c("2018-01-01", format(Sys.Date()-1,"%Y-%m-%d")),
                 metrics = "sessions",
                 dimensions = "date",
                 max = 10000)
library(tidyverse)
total_sessions %>% ggplot(aes(date, sessions)) + geom_point()
min(total_sessions$date[total_sessions$sessions != 0])

#enjoy!