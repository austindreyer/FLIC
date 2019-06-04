#### FLIC HomeBrew Functions Prototyping ####
### last updated 5/20/2019 ###

## Working to get a proper script running
test.fun <- function(a, b, ...){
  it <- c(a,b,...)
  itt <-  paste0("W", it)
  
  return(itt)
}

## how to remove multiple columns by name (even if same)

# mmt <- mt[ , -which(names(mt) %in% c(names(mt %>% select(contains("hour")))))]

## Calculating genotype means script

# mmt <- subset(mt, select = -c(hour))

# mmt <- Filter(function(x) !all(is.na(x)), mmt)

# mrmt <- data.frame(matrix(vector(),length(mmt[,1]),3,
#                          dimnames=list(c(),c('hour','mean','se'))),
#                   stringsAsFactors = F)
# mrmt$hour <- mt$hour
# mrmt$mean <- rowMeans(mmt)

# for (i in 1:length(mmt[,1])){
#   mrmt$se[i] <- sem(mmt[i,])
# }


## function to take subset data and clean it up for plotting

subdata.clean <- function(data, ...)
{
  
}


# function to add feeding measurements of individual capillary tubes for CAFE assay
CAFE_Add <- function(fdist,...)
{
  tot.dist <- sum(fdist,...)
  return(tot.dist)
}              

## Working to get a proper script running
test.fun <- function(a, b, ...){
  it <- c(a,b,...)
  itt <-  paste0("W", it)
  
  return(itt)
}


# work on function to deal with strange NAs in an experiment cropping up

#rm(dfm1.1, dfm1.2, dfm2.1, dfm2.2, dfm3.1, dfm3.2, dfm4.1, dfm4.2, dfm5.1, dfm5.2)


#tmp<-FeedingData.Licks(dfm11.190327.at, c(0,0))
#cname=paste("W",1,sep="")
#tmp<-tmp[,c("Minutes",cname)]
#min(tmp)

#dfm12.190327.at$RawData[2288055:2288077,]

#test.na <- which(is.na(tmp$Minutes))
#where <- which(is.na(tmp$Minutes))
#tail(where)
#test.sec <- seq(from =  458922.0, by =.2, length.out = 1418151)
#test.min <- seq(from =  7648.700, by =.0035, length.out = 1418151)