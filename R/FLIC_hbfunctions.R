##### FLIC HomeBrew Functions #####
### Updated 6/14/2019 ###

## contains nearly all of the functions used to handle FLIC data ##

# function to reformat all difftime class data to numeric for data binning

DFMData_numeric <- function(data, method)
{
  require(tidyverse)
  
  if(grepl("ft", method))
  {
  a <- c(3,4,6,7,8)
  b <- c(9,10)
  
  for (i in a)
  {
    data[[i]] %>% mutate_if(lubridate::is.difftime, as.numeric) -> data[[i]]
  }
  

  for(i in b)
    {
    for (j in 1:12)
      {
      if(length(data[[i]][[j]])>1)
        {
      data[[i]][[j]] %>% mutate_if(lubridate::is.difftime, as.numeric) -> data[[i]][[j]]
        }
      }
    }
  }
  
  
  else if(grepl("at", method))
  {
    a <- c(3,4,5,7,8,9)
    b <- c(10,11)
    
    for (i in a)
    {
      data[[i]] %>% mutate_if(lubridate::is.difftime, as.numeric) -> data[[i]]
    }
    
    
    for(i in b)
    {
      for (j in 1:12)
      {
        if(length(data[[i]][[j]])>1)
        {
          data[[i]][[j]] %>% mutate_if(lubridate::is.difftime, as.numeric) -> data[[i]][[j]]
        }
      }
    }
  }
  
  return(data)

}



# function to transform binned FLIC data into format for clocklab

txtclabdata <- function(data, dfm, edate, name, sdate, stime, interval)
{
  long <- length(data$Min)
  for (i in 1:12)
  {
    if (i < 10) 
    {
      id <- sprintf('C0%d',i)
    }
    else
    {
      id <- sprintf('C%d',i)
    }
    namet <- sprintf('%s%s%s%s',edate,name,dfm,id)
    named <- sprintf('%s %s %s %s   %s',edate,name,dfm,id,sdate)
    fill <- c(named, long, interval, stime)
    mt <- list(as.data.frame(data[i+2]))
    mt <- append(fill,mt)
    mt.df <- as.data.frame(do.call(rbind,mt))
    write.table(mt.df, sprintf('%s.txt',namet), row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  
}

# function to pull out wells so you can isolate genotypes 

wells.pull.min <- function(bin.data, wells, ...)
{
  wells <- c(well, ...)
  well.des <- paste0("W",wells)
  
  the.data <- select(bin.data, Min, well.des)
  
  return(the.data)
}


wells.pull <- function(bin.data, wells, ...)
{
  wells <- c(well, ...)
  well.des <- paste0("W",wells)
  
  the.data <- select(bin.data, well.des)
  
  return(the.data)
}

# raw data plots for individual flies by DFM #

ind.plot <- function (data,genotype) {
  mt <- data.frame(matrix(vector(),length(data[,1]),13,
                          dimnames=list(c(),c('hour','w1','w2','w3','w4','w5','w6','w7',
                                              'w8','w9','w10','w11','w12'))),stringsAsFactors = F)  
  
  cols <- rainbow(12)
  legend <- c('w1'=cols[1],'w2'=cols[2],'w3'=cols[3],'w4'=cols[4],'w5'=cols[5],
              'w6'=cols[6],'w7'=cols[7],'w8'=cols[8],'w9'=cols[9],'w10'=cols[10],
              'w11'=cols[11],'w12'=cols[12])
  mt[1] <- round_any(data$Min/60,0.5)
  
  for (i in 1:12) {
    mt[i+1] <- data[i+2]
  }
  
  gg <- ggplot(mt, aes(x=hour, y=w1, colour='w1'))
  gg + ggtitle(sprintf("%s",genotype)) + geom_line() + 
    #theme(panel.grid.minor=element_blank(),
    #      panel.grid.major=element_blank()) + 
    labs(x = "Time (Hours)", 
         y = "Raw data/fly"
    ) +
    scale_x_continuous(breaks=seq(0,tail(mt$hour,n=1),by=12)) +
    geom_line(data = mt, aes(x=hour, y=w2, colour='w2')) +
    geom_line(data = mt, aes(x=hour, y=w3, colour='w3')) +
    geom_line(data = mt, aes(x=hour, y=w4, colour='w4')) +
    geom_line(data = mt, aes(x=hour, y=w5, colour='w5')) +
    geom_line(data = mt, aes(x=hour, y=w6, colour='w6')) +
    geom_line(data = mt, aes(x=hour, y=w7, colour='w7')) +
    geom_line(data = mt, aes(x=hour, y=w8, colour='w8')) +
    geom_line(data = mt, aes(x=hour, y=w9, colour='w9')) +
    geom_line(data = mt, aes(x=hour, y=w10, colour='w10')) +
    geom_line(data = mt, aes(x=hour, y=w11, colour='w11')) +
    geom_line(data = mt, aes(x=hour, y=w12, colour='w12')) +
    
    scale_colour_manual(name="Wells",values=legend)
}

# normalized behavior by individual as described in Ro et al 2014 where
# behavior is normalized by dividing each 30 min bin by the average 
# amount of behavior for 30 minutes for total experiment

norm.plot <- function (data,genotype) {
  
  library(ggplot2)
  
  mt <- data.frame(matrix(vector(),length(data[,1]),13,
                          dimnames=list(c(),c('hour','w1','w2','w3','w4','w5','w6','w7',
                                              'w8','w9','w10','w11','w12'))),stringsAsFactors = F)  
  
  cols <- rainbow(12)
  legend <- c('w1'=cols[1],'w2'=cols[2],'w3'=cols[3],'w4'=cols[4],'w5'=cols[5],
              'w6'=cols[6],'w7'=cols[7],'w8'=cols[8],'w9'=cols[9],'w10'=cols[10],
              'w11'=cols[11],'w12'=cols[12])
  mt[1] <- round_any(data$Min/60,0.5)
  
  for (i in 1:12) {
    avg <- mean(data[,i+2])
    mt[i+1] <- data[i+2]/avg
  }
  
  gg <- ggplot(mt, aes(x=hour, y=w1, colour='w1'))
  gg + ggtitle(sprintf("%s",genotype)) + geom_line() + 
    #theme(panel.grid.minor=element_blank(),
    #      panel.grid.major=element_blank()) + 
    labs(x = "Time (Hours)", 
         y = "Normalized Feeding Behavior/fly") +
    scale_x_continuous(breaks=seq(from=0, to=tail(mt$hour,n=1), by=12)) +
    geom_line(data = mt, aes(x=hour, y=w2, colour='w2')) +
    geom_line(data = mt, aes(x=hour, y=w3, colour='w3')) +
    geom_line(data = mt, aes(x=hour, y=w4, colour='w4')) +
    geom_line(data = mt, aes(x=hour, y=w5, colour='w5')) +
    geom_line(data = mt, aes(x=hour, y=w6, colour='w6')) +
    geom_line(data = mt, aes(x=hour, y=w7, colour='w7')) +
    geom_line(data = mt, aes(x=hour, y=w8, colour='w8')) +
    geom_line(data = mt, aes(x=hour, y=w9, colour='w9')) +
    geom_line(data = mt, aes(x=hour, y=w10, colour='w10')) +
    geom_line(data = mt, aes(x=hour, y=w11, colour='w11')) +
    geom_line(data = mt, aes(x=hour, y=w12, colour='w12')) +
    
    scale_colour_manual(name="Wells",values=legend)
}

# group mean by DFM #

sem <- function(x) {
  sd(x)/sqrt(length(x))
}

group.plot <- function (data,genotype) {
  
  library(ggplot2)
  
  mt <- data.frame(matrix(vector(),length(data[,1]),13,
                          dimnames=list(c(),c('hour','w1','w2','w3','w4','w5','w6','w7',
                                              'w8','w9','w10','w11','w12'))),stringsAsFactors = F)  
  
  cols <- rainbow(12)
  legend <- c('w1'=cols[1],'w2'=cols[2],'w3'=cols[3],'w4'=cols[4],'w5'=cols[5],
              'w6'=cols[6],'w7'=cols[7],'w8'=cols[8],'w9'=cols[9],'w10'=cols[10],
              'w11'=cols[11],'w12'=cols[12])
  mt[1] <- round_any(data$Min/60,0.5)
  
  for (i in 1:(length(mt[1,])-1)) {
    avg <- mean(data[,i+2])
    mt[i+1] <- data[i+2]/avg
  }
  
  mmt <- subset(mt, select = -c(hour))
  nd <- as.data.frame(matrix(ncol=1, nrow=0))
  
  for (i in 1:length(mmt[1,])) {
    if(is.na(mmt[i])) {
      nd[i,1] = as.character('NA')
    } else {
      nd[i,1] = as.character('#')
    }
    
  }
  
  print(nd)
  
  mmt <- Filter(function(x) !all(is.na(x)), mmt)
  
  mrmt <- data.frame(matrix(vector(),length(mmt[,1]),3,
                            dimnames=list(c(),c('hour','mean','se'))),
                     stringsAsFactors = F)
  
  mrmt$hour <- mt$hour
  mrmt$mean <- rowMeans(mmt)
  
  for (i in 1:length(mmt[,1])){
    mrmt$se[i] <- sem(mmt[i,])
  }
  
  
  ggplot(mrmt, aes(x=hour,y=mean)) + 
    geom_errorbar(data=mrmt,aes(ymin=mean-se,ymax=mean+se), width=.1, colour='grey50') +
    geom_line() + geom_point(size=2, shape=21, fill='white') + 
    ggtitle(sprintf("%s",genotype)) +
    labs(x = "Time (Hours)", 
         y = "Normalized Feeding Behavior/fly") +
    scale_x_continuous(breaks=seq(from=0, to=tail(mt$hour,n=1), by=12))
  
}

# group  mean by genotype #

## need to create a single data frame with all data you want to look at by 
## combining one complete set of data with just columns of wells of interest ##

# data <- cbind(data,select(data,W1:W12)) example of how to create data frame of desired data

genotype.plot <- function (data,genotype) {
  
  library(ggplot2)
  
  mt <- data.frame(matrix(vector(),length(data[,1]),length(data[1,])-2))  
  
  mt$hour <- round_any(data$Min/60,0.5)
  
  for (i in 1:(length(mt[1,])-1)) {
    avg <- mean(data[,i+2])
    mt[i] <- data[i+2]/avg
  }
  
  mmt <- subset(mt, select = -c(hour))
  
  
  
  mmt <- Filter(function(x) !all(is.na(x)), mmt)
  
  
  mrmt <- data.frame(matrix(vector(),length(mmt[,1]),3,
                            dimnames=list(c(),c('hour','mean','se'))),
                     stringsAsFactors = F)
  
  mrmt$hour <- mt$hour
  mrmt$mean <- rowMeans(mmt)
  
  for (i in 1:length(mmt[,1])){
    mrmt$se[i] <- sem(mmt[i,])
    
  }
  
  ggplot(mrmt, aes(x=hour,y=mean)) + 
    geom_line() + 
    ggtitle(sprintf("%s",genotype)) +
    labs(x = "Time (Hours)", 
         y = "Normalized Feeding Activity/fly") +
    scale_x_continuous(breaks=seq(from=0, to=(tail(mt$hour,n=1)+24), by=24)) + 
    geom_errorbar(data=mrmt,aes(ymin=mean-se,ymax=mean+se), width=3)+
    geom_point(size=3, shape=21, fill='white') + 
    theme_classic() +
    theme(axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5),
          axis.title = element_text(size=18),
          axis.text.x = element_text(size=12))
}


### separate function to truncate binned data to just specific days for comparison
# only the idate, datatype, and hset arguments need be in quotes

subset.data <- function(data, idate, itime, stime, sday, eday, datatype, hset, well, ...){
  
  require(plyr)
  require(purrr)
  require(dplyr)
  
  hour <- round_any(data$Min/60,0.5)
  unnorm <- data
  unnorm[,"hour"] <- hour
  
  wells <- c(well, ...)
  well.des <- paste0("W",wells)
  
  
  mt <- data.frame(matrix(vector(),length(data[,1]),length(data[1,])))
  mt <- set_names(mt, colnames(data))
  mt[,"Interval"] <- data[,"Interval"]   
  mt[,"Min"] <- data[,"Min"]
               
for (i in 1:(length(mt[1,])-2)) {
  avg <- mean(data[,i+2])
  mt[i+2] <- data[i+2]/avg

 }

  mt[,"hour"] <- hour
  
  if (datatype == "norm")
    {
    wells.data <- mt %>% select(well.des)
    }
  else 
  {
    wells.data <- unnorm %>% select(well.des)
  }
  
    wells.data[,"hour"] <- mt[,"hour"]
    
  init.date <- as.POSIXlt(sprintf("%s %04d", idate, itime), format = "%Y-%m-%d %H%M")
  start.date <- as.POSIXlt(sprintf("%s %04d", idate, stime), format = "%Y-%m-%d %H%M")
  start.data <- ((as.numeric(start.date) - as.numeric(init.date)) + (86400*(sday-1)))/3600
  #end.data <- ((as.numeric(start.date) - as.numeric(init.date)) + (86400*(eday)))/3600
  end.data <- (eday - sday)*48
  
  if (hset == "running")
  {
  #mrmt.sub <- subset(wells.data, hour > start.data & hour < end.data)
  mrmt.fsub <- subset(wells.data, hour > start.data)
  mrmt.sub <- mrmt.fsub[1:end.data,]
  mrmt.sub$hours <- mrmt.sub$hour-head(mrmt.sub$hour, n=1)
  }
  else
  {
    mrmt.fsub <- subset(wells.data, hour > start.data)
    mrmt.sub <- mrmt.fsub[1:end.data,]
    mrmt.sub$hours <- rep(seq(0,23.5,0.5), times = (eday-sday))
  }
  #remove hour column
  
  mrmt.sub <- mrmt.sub[, !(names(mrmt.sub) %in% "hour")]
  return(mrmt.sub)
 }


## function to plot any phase shifts, by well (=genotype)

phaseshift_indfly_plot <- function(data, idate, itime, etimeS, etimeE, pday, eday, datatype, well, yhigh, by, day_col, title)
{
  # call necessary libraries
  library(stats)
  library(signal)
  library(ggplot2)
  
  # extact all data starting 6 hours prior to CT0 the first day after FLIC loaded 
  fly_data <- subset.data(data, idate, itime, etimeS, sday = 1.75, eday, datatype, hset = 'running', well)
  
  # create Butterworth filter object
  bf <- butter(2, 0.1, type = 'low', plane = 'z')
  
  # filter fly signal data using Butterworth filter
  fly_bf <- filtfilt(bf, fly_data[,1])
  
  # and add it to fly_data object
  fly_data$bf <- fly_bf
  
  # extract data for just the phase day of interest
  ## first calculate the window of data to pull based on pday
  ps <- 48*(pday-2)
  
  # then extract the 24 hours of data for desired day
  fly_pday <- fly_data[(1:48)+ps,]
  fly_pday[,2] <- rep(seq(0,23.5,0.5))
  
  # add column of 'n' and 'd' for plotting
  fly_pday$n.d <- c(rep("n", 12), rep("d", 24), rep("n", 12))
  
  # rename the data column to generic 'fly' for plotting reference to be consistant
  colnames(fly_pday)[1] <- 'fly'
  
  # create the base plotting object for ggplot
  p <- ggplot()
  
  # plot the thing, complete with error bars, no background, and axis labels
  p + 
    geom_col(data = fly_pday, 
             aes(hours, fly, fill=n.d),
             colour = "black", 
             position = position_nudge(x = 0.25)) +
    geom_line(data = fly_pday, aes(hours, bf), 
              color = 'red') +
    scale_fill_manual(values = c("n" = "black", "d" = day_col)) +
    ggtitle(sprintf("%s",title)) +
    scale_x_continuous(limits = c(-.5,24.5), 
                       breaks = seq(0,24,6), 
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,yhigh),
                       breaks = seq(0, yhigh, by=by), 
                       labels = seq(0, yhigh, by=by),
                       expand = c(0,0)) +
    theme_classic(base_size = 15) +
    theme(axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5),
          axis.title = element_text(size=18),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          legend.position = "none") +
    labs(x = "Hours", 
         y = "Feeding activity")
}

## function to pull out the difference in hours of any phase shifts, by well (=genotype)

phaseshift_indfly_time <- function(data, genotype, idate, itime, etimeS, etimeE, pday, eday, datatype, well)
{
  # call necessary libraries
  library(stats)
  library(signal)
 
  # extact all data starting 6 hours prior to CT0 the first day after FLIC loaded 
  fly_data <- subset.data(data, idate, itime, etimeS, sday = 1.75, eday, datatype, hset = 'running', well)
  
  # create Butterworth filter object
  bf <- butter(2, 0.1, type = 'low', plane = 'z')
  
  # filter fly signal data using Butterworth filter
  fly_bf <- filtfilt(bf, fly_data[,1])
  
  # and add it to fly_data object
  fly_data$bf <- fly_bf
  
  # find the peaks of the filtered data
  fly_peaks <- find_peaks(fly_bf)
  
  # make new object containing the peaks in hours of experiment starting with empty object
  mt_peaks <- NULL
  # then populate empty object with data, iterating over each peak
  for (i in 1:length(fly_peaks))
  {
    mt_peaks[i] <- fly_data$hours[fly_peaks[i]]
  }
  
  # extract data for just the phase day of interest
  ## first calculate the window of data to pull based on pday
  ps <- 48*(pday-2)
  
  # then extract the 24 hours of data for desired day
  fly_pday <- fly_data[(1:48)+ps,]
  
  # add column of just 0-24 hours for reference
  fly_pday$day <- rep(seq(0,23.5,0.5))
  
  # pull out the peaks that match the day of interest
  pday_peaks <- subset(mt_peaks, mt_peaks > min(fly_pday$hours) & mt_peaks < max(fly_pday$hours))
  
  # make empty columns for correct peak data to be added
  mt_peaks <-  setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("Mpeak", "Epeak", "genotype"))
  
  # and correct them for relative time on the day of interest
  if (length(pday_peaks) > 1)
  {
    mt_peaks[1] <- pday_peaks[1]-fly_pday[13,2]
    mt_peaks[2] <- pday_peaks[2]-fly_pday[37,2]
  } else  {
    mt_peaks[1] <- NA
    mt_peaks[2] <- pday_peaks[1]-fly_pday[37,2]
  }
  
  mt_peaks[3] <- genotype
  
# return the phase shift for the fly
  return(mt_peaks)
}

# function to compile phase shifts in hours for a genotype of a FLIC
phaseshift_genotype_time <- function(data, genotype, idate, itime, etimeS, etimeE, pday, eday, datatype, well, ...)
{
  # get the wells to be pulled
  wells <- c(well, ...)
  
  # create empty data frame to store peak data
  feed_peaks <- setNames(data.frame(matrix(ncol = 3, nrow = length(wells))), c("Mpeak", "Epeak", "genotype"))
  
  # fill data frame with the extracted peaks by DFM
  for (i in 1:length(wells))
  {
    twell <- wells[i]
    feed_peaks[i,] <- phaseshift_indfly_time(data, genotype, idate, itime, etimeS, etimeE, pday, eday, datatype, twell)
  }
  return(feed_peaks)
}

# function to extract data needed for calculation of anticipation index
AI_index_prep <- function(data, idate, itime, etimeS, pday, eday, well, ...)
{
  # extract all of the data for experiment starting 6 hours before entrainment time for calculations
  fly_data <- subset.data(data, idate, itime, etimeS, sday = 1.75, eday, "nonnorm", hset = 'running', well, ...)
  
  # extract data for just the phase day of interest
  ## first calculate the window of data to pull based on pday
  ps <- 48*(pday-2)
  
  # then extract the 24 hours of data for desired day
  fly_pday <- fly_data[(1:48)+ps,]
  
  # add column of just 0-24 hours for reference
  fly_pday$day <- rep(seq(0,23.5,0.5))
  
  return(fly_pday)
}

# function to calculate the anticipation index activity prior to environmental sift (as described in Stoleru et al. 2004)
AI_index <- function(etimeS, etimeE, genotype, data, ...)
{

  # calculate genotype averages
  g_means <- genotype.means(data, ...)
  
  # replace hour column with 0-48 for reference
  g_means[,1] <- rep(seq(0,23.5,0.5))
  
  # make empty columns for anticipation index to be added
  mt_AI <-  setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("M_AI", "E_AI", "genotype"))
  
  # calculate the morning anticipation index
  mt_AI[1] <- ((g_means[12,1]*(g_means[12,1]-g_means[11,1])*(g_means[11,1]-g_means[10,1]))/g_means[14,1])
  
  # calculate the hours between the entrainment start and end time
  e_diff <- ((etimeE-etimeS)/100)*2
  
  # calculate the evening anticipation index
  
  mt_AI[2] <- ((g_means[12+e_diff,1]*(g_means[12+e_diff,1]-g_means[11+e_diff,1])*(g_means[11+e_diff,1]-g_means[10+e_diff,1]))/g_means[14+e_diff,1])
  
  mt_AI[3] <- genotype
  
  # return the AI for the genotype
  return(mt_AI)
}

# Function to calculate anticipation phase shift (as described in Harrisingh et al. 2007)
AI_phase_score <- function(data, genotype, idate, itime, etimeS, etimeE, pday, eday, well, ...)
{
  # extract all of the data for experiment starting 6 hours before entrainment time for calculations
  fly_data <- subset.data(data, idate, itime, etimeS, sday = 0.75, eday, "nonnorm", hset = 'running', well, ...)
  
  # extract data for just the phase day of interest
  # first calculate the window of data to pull based on pday
  ps <- 48*(pday-1)
  
  # then extract the 24 hours of data for desired day
  fly_pday <- fly_data[(1:48)+ps,]
  
  # add column of just 0-24 hours for reference
  fly_pday$day <- rep(seq(0,23.5,0.5))
  
  # extract just well data for calculations
  mmt <- fly_pday[ , which(names(fly_pday) %in% c(names(fly_pday %>% select(contains("W")))))]
  
  # create empty data frame to store AI phase scores 
  mt_phase <-  setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("M_AI_phase", "E_AI_phase", "genotype"))
  
  # calculate the hours between the entrainment start and end time
  e_diff <- ((etimeE-etimeS)/100)*2
  
  # calculate AI phase scores
  for (i in 1:length(mmt))
       {
         mt_phase[i,1] <- (sum(mmt[7:11,i]))/(sum(mmt[1:11,i]))
         mt_phase[i,2] <- (sum(mmt[7:11+e_diff,i]))/(sum(mmt[1:11+e_diff,i]))
        }
  
  mt_phase[3] <- genotype
  
  return(mt_phase)
}


## Function to calculate the means of rows of data that has been 
## truncated using the subset.data() function, where means of 
## rows equals genotype means

genotype.means <- function(data, ...)
{
  mt <- bind_cols(data,...)
  mmt <- mt[ , which(names(mt) %in% c(names(mt %>% select(contains("W")))))]
  
  mmt <- Filter(function(x) !all(is.na(x)), mmt)
  
  mrmt <- data.frame(matrix(vector(),length(mmt[,1]),3,
                            dimnames=list(c(),c('hour','mean','se'))),
                     stringsAsFactors = F)
  mrmt$hour <- mt$hours
  mrmt$mean <- rowMeans(mmt)
  
  for (i in 1:length(mmt[,1]))
    {
    mrmt$se[i] <- sem(mmt[i,])
  }
  return(mrmt)
}


### modification of genotype.plot() function to make plots special for pub

genotype.plot.fig <- function (data, title, genotypecol, size=1.5, shape=21, low=0, high=4, by=1, ribbon=T) 
  {
  
  color <- color.select(genotypecol)
  
  if (ribbon == "TRUE")
    {
  ggplot(data, aes(x=hour,y=mean)) + 
    geom_line(color=color) + 
    ggtitle(sprintf("%s",title)) +
    labs(x = "Time (Hours)", 
         y = "Normalized Feeding Activity/fly") +
    scale_x_continuous(limits = c(head(data$hour, n=1)-2, 
                                  tail(data$hour,n=1)+2),
                      breaks = seq(from = head(data$hour,n=1), 
                                  to = tail(data$hour,n=1)+24, by=24),
                      labels = seq(from = head(data$hour,n=1), 
                                     to = tail(data$hour,n=1)+24, by=24)) + 
    scale_y_continuous(limits = c(low,high),
                       breaks = seq(low, high, by=by), 
                       labels = seq(low, high, by=by)) +
    geom_ribbon(data=data,aes(ymin=mean-se,ymax=mean+se), fill = color, colour=NA, alpha=0.4)+
    #scale_fill_manual(name = "genotype", values = c('red', 'blue', 'green')) +
    geom_point(size=size, shape=shape, fill=color) + 
    theme_classic() +
    theme(axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5),
          axis.title = element_text(size=18),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          legend.position = "none")
  }
   
  else {
  ggplot(data, aes(x=hour,y=mean, color = color)) + 
    geom_line() + 
    ggtitle(sprintf("%s",title)) +
    labs(x = "Time (Hours)", 
         y = "Normalized Feeding Activity/fly") +
    scale_x_continuous(limits = c(head(data$hour, n=1)-2, 
                                  tail(data$hour,n=1)+2),
                       breaks = seq(from = head(data$hour,n=1), 
                                    to = tail(data$hour,n=1)+24, by=24),
                       labels = seq(from = head(data$hour,n=1), 
                                    to = tail(data$hour,n=1)+24, by=24)) + 
    scale_y_continuous(limits = c(low,high),
                       breaks = seq(low, high, by=by), 
                       labels = seq(low, high, by=by)) +
    geom_errorbar(data=data,aes(ymin=mean-se,ymax=mean+se), width=3)+
    geom_point(size=size, shape=shape, fill=color) + 
    theme_classic() +
    theme(axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5),
          axis.title = element_text(size=18),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12))
  }
}


## color selection function

color.select <- function(gencol)
{
  if (grepl("gal4", gencol))
  {
    color = "magenta4"
  }
  else if (grepl("exp", gencol))
  {
    color = "red3"
  }
  else if (grepl("uas", gencol))
  {
    color = "black"
  }
  else 
  {
    color = gencol
  }
  return(color)
}

## Function to plot raw data and feeding events across time

Feeding_Events_Plot_Well <- function(data, datatype, well, start_min = 0, end_min = 1000000)
{
  #extracts data to be plotted, raw or baselined
  if (grepl("raw", datatype))
  {
    plot.data <- data$RawData
  }
  else if (grepl("base", datatype))
  {
    plot.data <- data$BaselineData
  }
  
  #creates full well designation by pasting a "W" before the number
  well.plot <- paste0("W", well)
  
  #creates data frame of all feeding events for selected well
  feed.data <- Feeding.Durations.Well(data, well)
  
  #truncate the full data sets to start and end time
  plot.sub <- subset(plot.data, Minutes > start_min & Minutes < end_min)
  
  if (length(feed.data)>1)
  {
  feed.sub <- subset(feed.data, Minutes > start_min & Minutes < end_min)
  
  #create upper and lower bounds of line segments for feeding event identification
  y.bottom <- rep((mean(plot.sub[,well.plot])-25), length(feed.sub$Minutes))
  y.top <- rep((mean(plot.sub[,well.plot])-5), length(feed.sub$Minutes))
  
  #create line segments to indicate when feeding events occurred
  segment.data <- data.frame(xint = as.numeric(feed.sub$Minutes), 
                             y.low = y.bottom,
                             y.up = y.top)
  
  #create title for plot
  plot.title <- deparse(substitute(data))
  
  
 
  #plot the raw data and the line segments for feeding events
    ggplot(plot.sub, aes(x = Minutes, y = plot.sub[,well.plot])) +
              labs(y = well.plot, title = plot.title) +
              geom_line(linetype = "dashed", alpha=0.4) +
              geom_segment(data = segment.data,
                aes(x = xint, xend = xint,
                y = y.low, yend = y.up),
                colour = "red")
   }
  else
    {
  #plot just the raw data if no feeding events took place (i.e. dead fly)
    plot.title <- deparse(substitute(data))
      
    ggplot(plot.sub, aes(x = Minutes, y = plot.sub[,well.plot])) +
        labs(y = well.plot, title = plot.title) +
        geom_line(linetype = "dashed", alpha=0.4)
    }
  
}

# Function to create paneled plots for all 12 wells comparing
# raw data and feeding events

Feeding_Events_DFMPlots <- function(data, start_min = 0, end_min = 100000)
{
  
  require(gridExtra)
  
  #create empty list to populate with actual plots
  plots <- list()
  
  #create empty list to populate with specific well data
  plot.sub.well <- list()
  
  #extracts data to be plotted, raw or baselined
  if (grepl("raw", datatype))
  {
    plot.data <- data$RawData
  }
  else if (grepl("base", datatype))
  {
    plot.data <- data$BaselineData
  }
  
  #create title for plot
  plot.title <- deparse(substitute(data))
  
  for (i in 1:12)
  {
  
  #creates full well designation by pasting a "W" before the number
  well.plot <- paste0("W", i)  
  
  #creates data frame of all feeding events for selected well
  feed.data <- Feeding.Durations.Well(data, i)
  
  #truncate the full data sets to start and end time
  plot.sub <- subset(plot.data, Minutes > start_min & Minutes < end_min)
  
  #extract just the data that needs to be plotted for each well
  plot.single.well <- plot.sub %>% select(Minutes, Signal = well.plot)
  plot.sub.well[[i]] <- plot.single.well
  
  
  if (length(feed.data)>1)
  {
    feed.sub <- subset(feed.data, Minutes > start_min & Minutes < end_min)
    
    #create upper and lower bounds of line segments for feeding event identification
    y.bottom <- rep((mean(plot.sub[,well.plot])-25), length(feed.sub$Minutes))
    y.top <- rep((mean(plot.sub[,well.plot])-5), length(feed.sub$Minutes))
    
    #create line segments to indicate when feeding events occurred
    segment.data <- data.frame(xint = as.numeric(feed.sub$Minutes), 
                               y.low = y.bottom,
                               y.up = y.top)
    
    #plot the raw data and the line segments for feeding events
    plots[[i]] <- ggplot(plot.sub.well[[i]], 
                         aes(x = Minutes, y = Signal)) +
      labs(y = well.plot) +
      geom_line(linetype = "dashed", 
                alpha=0.4) +
      geom_segment(data = segment.data,
                   aes(x = xint, xend = xint,
                       y = y.low, yend = y.up),
                   colour = "red")
  }
  else
  {
    #plot just the raw data if no feeding events took place (i.e. dead fly)
    
    plots[[i]] <- ggplot(plot.sub.well[[i]], #ggplot does lazy evaluation, index reference only in reference to data =...
                         aes(x = Minutes, y = Signal)) +
      labs(y = well.plot) +
      geom_line(linetype = "dashed", 
                alpha=0.4)
  }
  
  }
  
  do.call(grid.arrange, c(plots, top = plot.title))
  
}

## Function to find local maxima and minima 

find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

## Need to change bin feeding function events to call BinFeedingData.Well.Events
## rather than BinFeedingData.Well.Licks so we can correctly find feeding events (even
## though we're not actually going to be using this fuction), we are actually using the
## licks measure as Scott and Zach shared with me that it will provide more data of 
## the fly's feeding over events which will not account for the duration of each event

newbin <- function(dfm,binsize.min,range=c(0,0)){
  result<-BinFeedingData.Well.Events(dfm,1,binsize.min,range)
  
  for(i in 2:12) {
    cname=paste("W",i,sep="")
    tmp<-BinFeedingData.Well.Events(dfm,i,binsize.min,range)
    result<-data.frame(result,tmp$SumEvents)
  }
  names(result)<-c("Interval","Min",paste("W",1:12,sep=""))
  result  
}

# bin.dfm.data <- BinFeedingData.Events()

# extracts the total feeding time in seconds for a DFM by 
# entering the raw dfm data, initial date and time of when
# the experiment started, the start time of when data should
# be collected based on entrainment schedule, the start day
# and end day of data analysis, typically 2-7

feed.total <- function(data, dfm, idate, itime, stime, sday, eday, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12)
{
  init.date <- as.POSIXlt(sprintf("%s %04d", idate, itime), format = "%Y-%m-%d %H%M")
  start.date <- as.POSIXlt(sprintf("%s %04d", idate, stime), format = "%Y-%m-%d %H%M")
  start.data <- start.date + (86400*(sday-1))
  end.data <- start.data + (86400*(eday-1))
  mt <- matrix(ncol = 4, nrow = 12)
  # structure of data: exp.date | well | total.feed | genotype
  
  for (i in 1:12) 
  {
    wraw.data <- Feeding.Durations.Well(data, i)
    if(length(wraw.data) < 2) next
   
    wraw.data$Time <- init.date + (wraw.data$Minutes*60)
    wsub.data <- subset(wraw.data, Time > start.data & Time < end.data)
    wfeed.data <- sum(wsub.data$Duration)
    mt[i,] <- c(idate, dfm, sprintf("W%d",i), wfeed.data)
  }
  mt <- data.frame(mt)
  colnames(mt) <- c("exp.date", "dfm", "well", "total.feed")
  mt$genotype <- c(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12)
  mt$total.feed <- as.numeric(as.character(mt$total.feed))
  mt <- na.omit(mt)
  return(mt)
}

feed.total.dropwell <- function(data, well)
{
  data <- data[!(data$well==sprintf("%s", well)),]
  
}

# rename the genotypes for an entire dataframe based on the 
# genotype codes of 1, 2, or 3
feed.total.names <- function(data, one = "one", 
                             two = "two", 
                             three = "three",
                             four = "four")
{
  data$genotype <- as.character(data$genotype)
  data$genotype[data$genotype =="1"] <- one
  data$genotype[data$genotype =="2"] <- two
  data$genotype[data$genotype =="3"] <- three
  data$genotype[data$genotype =="4"] <- four
  
  return(data)
}



## Summary function to pull out descriptive variables from data set

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}



function(dfm,well,binsize.min,range=c(0,0)){
  tmp<-FeedingData.Events(dfm,range)
  cname=paste("W",well,sep="")
  
  tmp<-tmp[,c("Minutes",cname)]
  ## Remember that Event data include duration, but we aren't interested
  ## in that.  Set values >0 to 1.
  tmp[tmp[,cname]>1,cname]<-1
  
  m.min<-min(tmp$Minutes)
  m.max<-max(tmp$Minutes)
  
  y<-seq(m.min,m.max,by=binsize.min)
  if(y[length(y)]<m.max)
    y<-c(y,m.max)
  
  z<-cut(tmp$Minutes,y,include.lowest=TRUE)
  
  r.min<-aggregate(tmp$Minutes~z,FUN=mean)
  r.A<-aggregate(tmp[,cname]~z,FUN=sum)
  
  results<-data.frame(r.min,r.A[,2])
  names(results)<-c("Interval","Min","SumEvents")
  results
}

DFMClass
function(id,parameters) {
  if (!is.numeric(id) || !all(is.finite(id)))
    stop("invalid arguments")
  
  ## Check to determine whether the DFM object already exists
  st<-paste("DFM",id,sep="")
  found=0
  if(exists(st,where=1)) {
    data<-get(st)  
    found<-1
    if(AreParametersEqual(parameters,data$Parameters)==FALSE)
      data<-ChangeParameterObject(data,parameters)
  }
  
  ## If doesn't exist, get and create
  if(found==0) {
    
    file<-paste("DFM_",id,".csv",sep="")
    dfm<-read.csv(file,header=TRUE)  
    
    ## Get Minutes from Sample column only if ElapsedTime is not there
    if('Seconds' %in% colnames(dfm)) {
      Minutes<-dfm$Seconds/60
      dfm<-data.frame(Minutes,dfm)
    } else if(('Date' %in% colnames(dfm))&&('Time' %in% colnames(dfm))&&('MSec' %in% colnames(dfm))){
      Seconds<-GetElapsedSeconds(dfm)
      Minutes<-Seconds/60.0
      dfm<-data.frame(Minutes,Seconds,dfm)
    } else {
      stop("Time information missing from DFM data.")
    }
    
    data=list(ID=id,Parameters=parameters,RawData=dfm)
    class(data)="DFM"
    if(!is.na(FindDataBreaks(data,multiplier=4,returnvals=FALSE))){
      cat("Data lapses found. Use FindDataBreaks for details.")
      flush.console()      
    }
    data<-CalculateBaseline(data)  
    assign(st,data,pos=1)  
  }
  data 
}

# function to combine dataframes from multiple experiments by genotype for plotting
combine_days <- function(data, ...)
{
  require(dplyr)
  # pull out the hours column for averaging later
  hours <- c(rep(seq(0,23.5,.5), 5))
  
  # combine data from different experiments by genotype
  all_days <- bind_cols(data, ...)
  
  # remove dupliacte "hours" columns
  all_days <- all_days[, -grep("hour", names(all_days))]
  
  # add back in a single "hours" column for averaging
  all_days["hours"] <- hours
  
  return(all_days)
}

# function to create day/night plots of 24 hr averaged activity
day_meanbehav_plot <- function(data, yhigh = 3, by = 0.5, title)
 {
   
   require(ggplot2)
   
   # average behavioral tally of each 30 minute bin for each fly
   mean_data <- aggregate(. ~hours, data = data, mean)
   
   # the mean activity across all flies
   activity_means <- data.frame(hours = mean_data[,1], means = rowMeans(mean_data[,-1]))
   
   # add in column for night vs. day bars (to be used as grouping variable when plotting)
   activity_means$n.d <- c(rep("n", 12), rep("d", 24), rep("n", 12))
   
   # standard error function
   se <- function(x) sqrt(var(x)/length(x))
   
   # pull out just the data without the "hours" to calculate standard errors
   activity_means_fly <- mean_data[,-1]
   
   # add standard errors for each binned data group
   activity_means$se <- apply(activity_means_fly, 1, se)
   
   # create the base plotting object for ggplot
   p <- ggplot(activity_means, aes(hours, means, fill=n.d))
   
   # plot the thing, complete with error bars, no background, and axis labels
   p + 
     geom_bar(stat = "identity", 
              colour = "black", 
              position = position_nudge(x = 0.25)) +
     scale_fill_manual(values = c("n" = "black", "d" = "white")) +
     ggtitle(sprintf("%s",title)) +
     scale_x_continuous(limits = c(-.5,24.5), 
                        breaks = seq(0,24,6), 
                        expand = c(0, 0)) +
     scale_y_continuous(limits = c(0,yhigh),
                        breaks = seq(0, yhigh, by=by), 
                        labels = seq(0, yhigh, by=by),
                        expand = c(0,0)) +
     geom_errorbar(aes(ymin = means-se, ymax=means+se), 
                   width = 0.2,
                   position = position_nudge(x = 0.25)) + 
     theme_classic(base_size = 15) +
     theme(axis.line.x = element_line(color="black", size = .5),
           axis.line.y = element_line(color="black", size = .5),
           axis.title = element_text(size=18),
           plot.title = element_text(hjust = 0.5, size = 10),
           axis.text.x = element_text(size=12),
           axis.text.y = element_text(size=12),
           legend.position = "none") +
     #theme(legend.position = "none") +
     
     labs(x = "Hours", 
          y = "Normalized feeding activity") 
 }

# function to randomize fly location in FLIC monitors

FLIC_random <- function(num_fly = 12, num_gen = 3)
{
  groups <- num_fly/num_gen
  
  fly_dist <- rep(1:num_gen, each = groups) 
  
  if(num_fly > 6) 
    {
    matrix(sample(c(fly_dist), replace = FALSE), nrow=2, ncol=6)
  }
  else 
    {
      matrix(sample(c(fly_dist), replace = FALSE), nrow=1, ncol=6)
  }
}


# function to calculate the volume of food individual flies consumed in the CAFE assay
      
CAFE_Feed_Bottle <- function(vol, sdist, fdist, evap, flies)
{
  feed <- (((vol*(sdist-fdist))/sdist)-evap)/flies
  
  return(feed)
}

        
  CAFE_Feed <- function(edist, bottles, flies, fdist, ...)
{
  tot.dist <- CAFE_Add(fdist, ...)
  
  tot.edist <- edist*bottles
  
  feed.dist <- tot.dist - tot.edist
  
  fly.feed <- feed.dist/flies
  
  return(fly.feed)
}

