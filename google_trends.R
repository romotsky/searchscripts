if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")

if(!require(gtrendsR)) install.packages("gtrendsR",
                                         repos = "http://cran.us.r-project.org")
## https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf

getwd()

wd <- '/Users/danielromotsky/Dropbox/Portfolio/searchscripts' # set working directory where keyword list is
setwd(wd)
getwd()
filename <- 'keywords.csv'
output_rising <- 'rising.txt'
output_trending <- 'trending.txt'

## pull in keywords from csv
keywords <- as.vector((read.csv(filelist[filelist==filename], header = TRUE, na.strings = FALSE, stringsAsFactors = FALSE)))


data("categories") # load category data
data("countries") # load country data
countries %>% filter(country_code=='US') #view list of DMAs in US
countries_rollup <- as.character(unique(factor(countries$country_code))) 

# photoshop <- gtrends(keyword = "photoshop",  ## A character vector with the actual Google Trends query keywords. Multiple keywords are possible using gtrends(c("NHL","NBA", "MLB", "MLS")).
#         geo = "", ## A character vector denoting geographic regions for the query, default to "all" for global queries. Multiple regions are possible using gtrends("NHL", c("CA", "US")).
#         time = "today 12-m",   ##A string specifying the time span of the query 
#         gprop = c("web","news", "images", "froogle", "youtube"),   ##
#         category = 0,  ##A character denoting the category, defaults to "0"
#         hl = "en-US",   ##A string specifying the ISO language code (ex.: "en-US" or "fr"). Default is "en-US". Note that this is only influencing the data returned by related topics
#         low_search_volume = FALSE) ## Logical. Should include low search volume regions?


keyword_list <- c("Adobe Analytics")
kw2 <- gtrends(keyword = keyword_list,  ## A character vector with the actual Google Trends query keywords. Multiple keywords are possible using gtrends(c("NHL","NBA", "MLB", "MLS")).
        geo = "US", ## A character vector denoting geographic regions for the query, default to "all" for global queries. Multiple regions are possible using gtrends("NHL", c("CA", "US")).
        time = "today 12-m",   ##A string specifying the time span of the query
        gprop = c("web","news", "images", "froogle", "youtube"),   ##
        category = 0,  ##A character denoting the category, defaults to "0"
        hl = "en-US",   ##A string specifying the ISO language code (ex.: "en-US" or "fr"). Default is "en-US". Note that this is only influencing the data returned by related topics
        low_search_volume = FALSE) ## Logical. Should include low search volume regions?
kw2$related_queries



## run a for loop to get rising term data for all keywords...
for (i in 1:lengths(keywords[1])) {
  kw_loop2 <-  gtrends(keyword = keywords$keywords[i],  
          geo = "US", ## A character vector denoting geographic regions for the query, default to "all" for global queries. Multiple regions are possible using gtrends("NHL", c("CA", "US")).
          time = "today 12-m",   ##A string specifying the time span of the query
          gprop = c("web","news", "images", "froogle", "youtube"),   
          category = 0, ##A character denoting the category, defaults to "0"
          hl = "en-US",  ##A string specifying the ISO language code (ex.: "en-US" or "fr"). Default is "en-US". Note that this is only influencing the data returned by related topics
          low_search_volume = FALSE) [[7]] ## Logical. Should include low search volume regions?

  if(i==1){
    kw_loop <- kw_loop2
  }
  
  else {kw_loop <- bind_rows(kw_loop,kw_loop2)
  }

}

#select only rising terms
filter(kw_loop[c(5,3,1,2)], related_queries == 'rising')

# lapply(kw_loop, class)

# export rising terms
write.table(filter(kw_loop[c(5,3,1,2)], related_queries == 'rising'), file = output_rising, row.names = FALSE, quote = FALSE, sep = "\t")

# interest
## run a for loop to get rising term data for all keywords...
for (i in 1:lengths(keywords[1])) {
  kw_loop2 <-  gtrends(keyword = keywords$keywords[i],  
                       geo = "US", ## A character vector denoting geographic regions for the query, default to "all" for global queries. Multiple regions are possible using gtrends("NHL", c("CA", "US")).
                       time = "today 12-m",   ##A string specifying the time span of the query
                       gprop = c("web","news", "images", "froogle", "youtube"),   
                       category = 0, ##A character denoting the category, defaults to "0"
                       hl = "en-US",  ##A string specifying the ISO language code (ex.: "en-US" or "fr"). Default is "en-US". Note that this is only influencing the data returned by related topics
                       low_search_volume = FALSE) [[1]] ## Logical. Should include low search volume regions?
  
  if(i==1){
    kw_loop <- kw_loop2
  }
  
  else {kw_loop <- bind_rows(kw_loop,kw_loop2)
  }
  
}


# export rising terms
write.table(kw_loop, file = output_trending, row.names = FALSE, quote = FALSE, sep = "\t")

