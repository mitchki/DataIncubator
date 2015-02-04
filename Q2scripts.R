
setwd("~/DataIncubator")

hits <- read.table("hits.csv",
            colClasses=c("character","factor","factor"),
            skipNul=TRUE,sep=",",header=TRUE)

str(hits)
hits$dtimes <- strptime(hits$time,"%Y-%m-%d %H:%M:%S")
save(hits,file="hits.Rda")

summary(table(hits$user))

## find average number of visits over all users
tabuser<- table(hits$user)
str(tabuser)
hist(tabuser)

format(mean(tabuser),digits=10)
"4.770513906"

tuser <- data.frame(tabuser)
summary(tuser$Freq)
## Min.   1st Qu.    Median      Mean     3rd Qu.      Max. 
## 1.000     1.000     2.000     4.771     6.000 14800.000 

repeatUser <- tabuser[tabuser > 1] 
str(repeatUser)

ruser <- (data.frame(repeatUser))
format(mean(repeatUser),digits=10)
"6.960358729"

require(plyr)
earliest<- aggregate(dtimes~user,hits,min)
latest<- aggregate(dtimes~user,hits,max)

usertimes <- merge(earliest,latest,by="user")
usertimes$span <- usertimes$dtimes.y - usertimes$dtimes.x
summary(usertimes$span)
hist(usertimes$span)
?difftime
usertimes$spanseconds <- as.numeric(usertimes$span, units="secs")
str(usertimes)
format(mean(usertimes$spanseconds),digits=10)

## calculate visits per category per user
catperuser <- aggregate(dtimes~user+category,hits,length)
avgcatsperuser <- aggregate(dtimes~user,catperuser,length)
format(mean(avgcatsperuser$dtimes),digits=10)

## calculate visits per category per user given multiple visits to the category
multvisits <- catperuser[catperuser$dtimes>1,]
format(mean(multvisits$dtimes),digits=10)
"4.56795926"

## calculate number categories visited per user 
## where user visited more than once
multcats <- aggregate(dtimes~user,multvisits,length)
format(mean(multcats$dtimes),digits=10)
"1.513405742"

## calculate number categories visited per user 
## where user visited more than one category
numcats <- multcats[multcats$dtimes>1,]
format(mean(numcats$dtimes),digits=10)
"2.547904776"

## reshape hits df to be wide by users

w_hits <- reshape(hits,direction="wide",
                  v.names=c("dtimes","category"),
                 idvar="user")
str(hits)
