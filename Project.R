getwd()
setwd("~/GitHub/TheDataIncubator")

require(XML)

## cities.csv previously set up to include the 24 largest US cities
## https://github.com/mitchki/DataIncubator/blob/master/cities.csv
## and their populations per the 2010 US census
## read in the cities data as a data frame
cities <- read.csv("cities.csv")
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
cities$State <- trim(cities$State)

fileUrl <- "http://api.indeed.com/ads/apisearch?publisher=6660716131600223&q=data+analyst&l=&sort=&radius=&st=&jt=&start=&limit=100&fromage=&filter=&latlong=1&co=us&chnl=&userip=1.2.3.4&useragent=Mozilla/%2F4.0%28Firefox%29&v=2"
doc <- htmlTreeParse(fileUrl,useInternal=TRUE)

totalresults <- xpathSApply(doc,"//totalresults",xmlValue)


## Poll the Indeed API for number of current job listings
## for specified job keywords for each city in our cities dataset
## return a new dataframe containing this data
getData <- function(keyword) {
    keyword <- gsub(" ","+",keyword)
    fileUrl <- ""; Population <- NA
    Keyword <- "";City <- "";State <- ""; TotalResults <- NA
    for (i in 1:nrow(cities)) {
        pre <- "http://api.indeed.com/ads/apisearch?publisher=6660716131600223&q="
        post <- "&sort=&radius=0&st=&jt=&start=&limit=100&fromage=&filter=&latlong=1&co=us&chnl=&userip=1.2.3.4&useragent=Mozilla/%2F4.0%28Firefox%29&v=2"
        cityState <- paste(cities$City[i],",",cities$State[i],sep="")
        fileUrl <- paste(pre,keyword,"&l=",cityState,post,sep="")
        doc <- htmlTreeParse(fileUrl,useInternal=TRUE)
        totalresults <- xpathSApply(doc,"//totalresults",xmlValue)
        Keyword[i] <- keyword
        City[i] <- as.character(cities$City[i])
        State[i] <- cities$State[i]
        TotalResults[i] <- totalresults
        Population[i] <- cities$Population[i]
    }
    chartData <-
        data.frame(cbind(Keyword,City,State,                                  Population,TotalResults),stringsAsFactors=FALSE)
}

## get demand for job keywords using getData function
## Plot a simple bar chart of number of listings by city
plotDemand <- function(keyword) {
    df <- getData(keyword)
    df$TotalResults <- as.numeric(df$TotalResults)
    d <- ggplot(df, aes(x=City, y=TotalResults)) +
        geom_bar(stat="identity",fill="darkblue") +
        theme(axis.text.x  = element_text(angle=90))
    d + ggtitle(paste("Number of job listings for ",keyword))
}

plotDemand("data scientist")
plotDemand("business analyst")
plotDemand("mechanical engineer")
plotDemand("truck driver")
plotDemand("card dealer")
plotDemand("television")
