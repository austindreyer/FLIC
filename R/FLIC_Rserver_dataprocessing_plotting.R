#### FLIC Raw Data Processing and Plotting ####
#### updated 5/23/2018 ####

## Steps 1 - 9 below describe the process of taking data taken from the laptop
## connected to the MCU and making into ClockLab compatable files. Steps 10-11
## describe the process of plotting and extracting total feeding duration per
## fly. 

## NOTES: 
## 1. When working in R and using scripts like this one, all text
## directly following a # are comments only, will not affect any input.
## 2. There are two easy ways to run scripts from a file like this one
## into the R console below that will actually enact the code. One way
## is to highlight the text in the script and hit "cnrl + enter" (PC) or 
## "cmd + enter" (mac) and they will run in the R console below. The 
## Second way is to copy and paste the text from the script into the 
## R console below and then hitting "enter". 
## 3. The text below goes into detail about how the data are extracted
## from a DFM, but the key parts are the non-commented portions in steps 
## 2, 3, 4 and 7. Steps 1-4 can be entered exacly as is. They describe 
## the required reference to functions necessary for analysis (Step 2), 
## the calling of R packages needed for analysis (Step 3), the defining 
## of parameters needed for the analysis (Step 4).
## 4. Steps 5 and 6 describe the manner in which the DFM raw data is 
## transformed into a format that can eventually be used by ClockLab.
## Step 7 is a summation of those two steps and contains the code 
## to read in all DFMS and bin the data accordingly at once, so read
## steps 5 and 6 to understand what is happening, then use step 7 code
## to enact those steps. BE SURE TO CHANGE THE DATES/NAMES as appropriate
## for step 7 to reflect the data being handled before reading it in.
## 5. Once you understand the script in its entirety, you can
## just highlight the key portions of code in steps 2, 3, 4, 7 and 
## hit enter to get the ball rolling. Adaptive threshold will take 
## ~1.5 hrs per DFM to read in data. 


####### 1 #######
# Transfer the raw data DFM data files to be analyzed from a flashdrive or 
# the cloud drive to the appropriately named folder on your LUC login partition. 
# Set the working directory to wherever the raw data are. The setwd() command
# stands for "Set Working Directory" and will change which folder Rstudio
# will refer to when analyzing data. The data to be analyzed are the raw
# data files straight from the computer the FLIC monitors were plugged into.
# To navigate to the proper location on the computer, start by typing in
# setwd("/U") and then hit the "Tab" key to generate a list of all possible
# completions that start with the letter "U". Select "User" and then just 
# hit "Tab" again to generate a list of all subfolders under "User" etc. etc.
# all the way down to the desired folder. This is easiest way to make sure
# you have all of the paths typed in just right as only the existing paths
# are even options. You need only specify the folder the raw data are in, not
# a specific DFM.

# Example: setwd("/Users/your_luc_id/folder_name/subfolder_name/datafolder_name")

####### 2 #######
# Attach the functions from the Pletcher lab for use in analysis by 
# including the directory of the packages listed below. There are five R scripts
# that you need to have downloaded from the WD MyCloud or from the Cavanaugh
# Lab Google Drive: 1. FLICFunctions, 2. DFM.R, 3. ParameterClass.R, 
# 4. CommonChamber.R, 5. FLIC_hbfunctions.R. These five R scripts should be saved
# into a folder of your choosing on your partition, and you can read them in to your
# current workspace by referring to the location of the folder they are in 
# followed by a direct reference to the script itself (see examples below). 
# The first command is to "attach" the FLICFunctions to the workspace so the 
# functions are avialable, and then "source" the remaining scripts 

# First navigate to the directory containing the R files to be read in
setwd("~/homes/bio/FLIC/R")

attach("/homes/your_luc_id/FLIC/R/FLICFunctions", pos=2)
source("/homes/your_luc_id/FLIC/R/DFM.R")
source("/homes/your_luc_id/FLIC/R/ParametersClass.R")
source("/homes/your_luc_id/FLIC/R/CommonChamber.R")
source("/homes/your_luc_id/FLIC/R/FLIC_hbfunctions.R")

# example for server
attach("/homes/bio/FLIC/R/FLICFunctions", pos=2)
source("/homes/bio/FLIC/R/DFM.R")
source("/homes/bio/FLIC/R/ParametersClass.R")
source("/homes/bio/FLIC/R/FLIC_hbfunctions.R")

## specific instructions for reading in CommonChamber.R
# For some reason, you must be in the same directory as 
# the CommonChamber.R file for it to successfully be sourced. 
# Navigate to the correct folder using setwd() and then use the
# source() function to read it in

#Ex: setwd("/homes/bio/FLIC/R")
#Ex: source("/homes/bio/FLIC/R/CommonChamber.R")


####### 3 #######
# You then need to load the specific R packages listed below that are used
# to varying degrees in the analysis and plotting of the data. To 
# load the packages, they must first be installed in the version of R you are
# using. To install packages in Rstudio, click on the "Packages" tab in the 
# bottom right panel of RStudio -->
# then select "Install" on the top left of the tab, type in the package name
# listed below (e.g. dplyr) and then click install with dependencies. Once
# they are installed, they can be loaded using the library() command.

library(plyr)
library(tidyverse)
library(MASS)

## The package dplyr needs to be detached and then reloaded after the above
## have been loaded to correct for an internal masking problem of required 
## functions. To do this, after the above have been loaded, select the 
## "Packages" tab from the bottomright window, scroll down to dplyr, then
## uncheck and recheck the box next to dplyr. You should see confirmation 
## of it being detached and reladed in the console below.

# Also helpful to increase the memory limit of R just to be safe for reading
# in data/manipulating data. To do this, use the function memory.limit()
# and set a value that is very large

memory.limit(99999999)

####### 4 #######
# Once the packages and functions are loaded, and the working directory is
# set to the location of the raw FLIC data files, need to prepare for the
# analysis. We use the adaptive threshold method of determining a feeding
# event. Briefly, the adaptive threshold uses percentile rankings of a set
# amount of time (default 2 min) to determine if a mV peak is considered a 
# feeding event. Any peak that is >90th percentile could be a feeding event, 
# instead of a flat threshold of fixed mV level. 

# To set up the analysis, need an object called 'p' that describes the 
# parameters to be used for the analysis function. Most of the default 
# values for p can be kept, ones below are components we want to change

p <- ParametersClass.SingleWell() # refers to the number of wells included
# in the FLIC experiment, either ParametersClass.SingleWell() if 12 wells or 
# ParameterClass.TwoWell() if using 6 wells

# you can observe the parameters currently in use by typing in the variable:
# p

p <- SetParameter(p,Signal.Threshold = 20) # sets the overall baseline signal
p <- SetParameter(p,Feeding.Threshold.Value = 85) # sets the minimum mV for a peak to be considered feeding
p <- SetParameter(p,Feeding.Interval.Minimum = 40) # minimum duration of event to be called feeding
p <- SetParameter(p,Tasting.Threshold.Interval = c(10,40)) # sets low and high range of time for a mV peak to be a feeding event

p <- SetParameter(p,Use.Adaptive.Threshold = TRUE) # turns the adaptive threshold on, FALSE = off


# To avoid biasing tasting data, make sure that 
# Tasting.Threshold.Interval.High <= Feeding.Interval.Minimum.

####### 5 #######

#### !!!!!!!! Super Important!!!!!! The function used to read in the actual data, DFMClass()
#### will use any pre-existing DFM data sets that have been read into the workspace you 
#### are currently working in. Therefore, before you read in any new data, be sure that
#### there are no objects of DFM class listed in the "Environment" tab, top right -->. If
#### there are, be sure to remove them first by using the rm() function.

# To read in the actual raw data from the FLIC computer, we will use the DFMClass() function. 
# This function requires two variables to work, the DFM id (e.g. 1, 2, 3, etc.) and the
# object containing all of the paramaters to be used for the extraction of data. This is 
# the object 'p' made during step 4 above. The easiest way to do this is to have all
# of the DFMs to be read in written on their own line, and then read it in en masse with
# the binning of the data into 30 minute bins (see below). By convention, the naming of the 
# objects matches all other file names in the Cavanaugh lab: dfm#.date.information.at (at = adaptive threshold),
# for example, experiment on February 1, 2017 about cyc mutant flies in DFM1: dfm1.170201.cycmutant.at

dfm1.date.information.at <- DFMClass(DFMid,parameters) # generic syntax for the DFMClass() function

# There is a chance that there will be a few data breaks in the data somewhere as there are 
# five data points recorded per well per second for over a week. If breaks are found, you will
# get a message stating such. You can use the function
# FindDataBreads(date.information.at) to see where the breaks are at and if they are short 
# enough as to not be a problem. The "Interval" column says the length of the break.
## Inspect data by running FindDataBreaks() and it will spit out if/when there are
## gaps in the data

FindDataBreaks(dfm1.date.information.at) #example of finding data breaks

####### 6 #######

# Once the raw data has been read in using DFMClass() function, you will need
# to bin the data into bins of some set time, typically 30 minutes. To do this
# use the function BinFeedingData.Licks(date.information.at, binsize.min, range=c(0,0)) 
# where "binsize.min" = 30. Using the BinFeedingData.Licks() function because
# basing the feeding events on licks accounts for the duration of the feeding events
# as it's counting licks and not just the number of actual events which would not 
# account for the duration of those events.

# Naming convention for the binned data files is same name as for the data
# from DFMClass() function, but with "binX" in front of it (where X is bin length)
# e.g. bin30.dfm1.170201.cycmutant.at <- BinFeedingData.Licks(dfm1.170201.cycmutant.at,30)

# Example: binX.dfm1.date.information.at  <- BinFeedingData.Licks(dfm1.date.information.at,X) 


####### 7 ########

# At this point, you are ready to read in all of the DFM raw data, and then bin the
# data as desired. Using the adaptive threshold means this process will take between
# 6-12 hours depending on the computer being used. If using the lab computers, best
# to set it up to run overnight, which means you MUST unplug the computer from the 
# ethernet hard-wire internet cord so that the computer cannot remotely restart to 
# install updates overnight (which the computer will automatically do at 2 am if online)

# To read in the data en masse, use the objects listed below, adjusting the dates and names
# as necessary. Copy and paste all of it at once into the working window of RStudio, and then hit 
# enter. The anaylsis should start, and within a minute or two you should see it counting up by
# well through the data.

# As of R version 3.3 (ish), the DFM class data must first be converted to all 
# numeric data rather than difftime data, which it will default to as that is
# the data format for minutes and seconds. The binning data function will not work
# with difftime data, you will get an error if you try. To convert the data, use the function
# DFMData_numeric() which asks for two arguments, the data to be converted followed
# by the type of DFM data it is, created either using the flat threshold option ("ft"), 
# or the adaptive threshold ("at"). There is no need to name any new objects using
# this function, it will simply overwrite the old object that had difftime data
# with an exact duplicate that has numeric data in place of the difftime.

# Ex: DFMData_numeric(dfm1.180201.at, "at")

# To ensure all dates have been changed when analyzing multiple dfms at once,
# best to copy and paste a complete set of objects (e.g. DFMClass() through 
# BinFeedingData.Licks()) into a new R script and just to a "Find and Replace"
# search for the old date (e.g. 180201) replacing it with new one (180505).

dfm1.180201.at <- DFMClass(1,p) # The number here will refer to the DFM_1.csv data that is in the working directory you set at the beginning
dfm2.180201.at <- DFMClass(2,p)
dfm3.180201.at <- DFMClass(3,p)
dfm4.180201.at <- DFMClass(4,p)
dfm5.180201.at <- DFMClass(5,p)

dfm1.180201.at <- DFMData_numeric(dfm1.180201.at, "at")
dfm2.180201.at <- DFMData_numeric(dfm2.180201.at, "at")
dfm3.180201.at <- DFMData_numeric(dfm3.180201.at, "at")
dfm4.180201.at <- DFMData_numeric(dfm4.180201.at, "at")
dfm5.180201.at <- DFMData_numeric(dfm5.180201.at, "at")

bin30.dfm1.180201.at  <- BinFeedingData.Licks(dfm1.180201.at,30) # BE SURE YOU CHANGE ALL REFERENCES TO DFM NUMBER in the function BinFeedingData.Licks() and in the object name on the left
bin30.dfm2.180201.at  <- BinFeedingData.Licks(dfm2.180201.at,30)
bin30.dfm3.180201.at  <- BinFeedingData.Licks(dfm3.180201.at,30)
bin30.dfm4.180201.at  <- BinFeedingData.Licks(dfm4.180201.at,30)
bin30.dfm5.180201.at  <- BinFeedingData.Licks(dfm5.180201.at,30)

dfm6.180201.at <- DFMClass(6,p) # The number here will refer to the DFM_1 data that is in the working directory you set at the beginning
dfm7.180201.at <- DFMClass(7,p)
dfm8.180201.at <- DFMClass(8,p)
dfm9.180201.at <- DFMClass(9,p)
dfm10.180201.at <- DFMClass(10,p)

dfm6.180201.at <- DFMData_numeric(dfm6.180201.at, "at")
dfm7.180201.at <- DFMData_numeric(dfm7.180201.at, "at")
dfm8.180201.at <- DFMData_numeric(dfm8.180201.at, "at")
dfm9.180201.at <- DFMData_numeric(dfm9.180201.at, "at")
dfm10.180201.at <- DFMData_numeric(dfm10.180201.at, "at")

bin30.dfm6.180201.at  <- BinFeedingData.Licks(dfm6.180201.at,30) # BE SURE YOU CHANGE ALL REFERENCES TO DFM NUMBER in the function BinFeedingData.Licks() and in the object name on the left
bin30.dfm7.180201.at  <- BinFeedingData.Licks(dfm7.180201.at,30)
bin30.dfm8.180201.at  <- BinFeedingData.Licks(dfm8.180201.at,30)
bin30.dfm9.180201.at  <- BinFeedingData.Licks(dfm9.180201.at,30)
bin30.dfm10.180201.at  <- BinFeedingData.Licks(dfm10.180201.at,30)


# Once the data has all been read in properly, you should see 1.5X as many objects in the global 
# environment (top left panel -->) as you read in. One object for each you did read in, and 
# based on the function of DFMCLass(), a dupliacte of each DFM file. One will be names as you
# stipulated above (e.g. dfm1.170201.at), the dupliate will be named DFM and the number (e.g. DFM1).
# Some functions don't work well with the generic naming of just DFM1 or DFM3, hence the need to
# name them something unique. Still, the dupliate DFM files take up precious real estate in you
# workspace, so it is best to delete the dupliate files. To do so, use the rm() function for each
# file to delete. You can have many objects in the rm() as long as they are separated by commas.
# Alternatively, you can remove a number of objects with similar names using the following
# syntax, rm(list = ls(pattern = "text that shows up in each object to be removed"))

# Example: rm(DFM1)
# Example: rm(DFM1, DFM2, DFM3)
# Eaxmple: rm(list = ls(pattern = "DFM")) # will remove all objects with letters "DFM" in name

# You should then have a workspace with a single dfm file and a single bin file for each DFM. At this
# point you need to save the workspace by selecting "Session" then "Save Workspace As"
# and use the same naming convention for the workspace name as for the DFM's (e.g. 170201_cycmutant)

#### !!!!!!!! Super Important!!!!!! Chances are that the computer in the lab will run out of memory when
#### performing this set of reading in huge data sets. If that happens, inspect each set of data using
#### functions below to make sure they are all complete, as often it will give an error but not actually
#### cut off any of the data. If all data is accounted for, move on. If not, you will have to combine
#### workspaces, one with data from DFMs 1-3 or 4, and one with data from DFM 4 and/or 5 that couldn't fit.

#### If you need to combine multiple workspaces due to memory problems, simply save one with the
#### DFMs that could successfully be read in (e.g. 170201_cycmutant_DFM1234) and then remove all
#### of the objects related to those DFMs using rm() 
#### and that should free up enough memory to complete the read in additional DFM data. Once 
#### the remaining DFMs have been read in using DFMClass() and BinFeedingData.Licks(), save 
#### a second workspace (e.g. 170201_cycmutant_DFM5), and analyze each workspace separately as 
####vthe output .txt files for clocklab will be combined outside of RStudio.

######## 8 #######
# After the raw data has been processed by DFMClass(), you need to break down the binned data into 
# 12 .txt files that ClockLab and FaasX software can utilize. To do this, you will use the function
# txtclabdata() which will produce the .txt files neccessary for ClockLab analysis. There are a number 
# of parameters that must be input for the function to work, we'll take them one at a time. First, the 
# complete txtclabdata parameter list, then the breakdown:

# txtclabdata(data, dfm, edate, name, sdate, stime, interval)
# 1. data = the binned data to be broken up by fly. DO NOT use the dfm file as it will crash RStudio.
# The data does not need to be in quotes
# 2. dfm = the DFM number, needs to be encoded as M00# (monitor #) in quotes (e.g. "M001")
# 3. edate = the reference date of the experiment in one row of text in quotes (e.g. "170201")
# 4. name = unique three character name in quotes ("iso")
# 5. sdate = the particular date format clock lab requires of the same date as edate
# which is the calendar date with no symbols included and a space between the year, month,
# and day code. Use only the three letter month codes for all months, in quotes (e.g. "01 feb 2017")
# 6. stime = the time, in military time, that the experiment started. If not sure what that time is,
# can view the exact start time of the experiment by typing in head(dfm.data$RawData) which will show
# the top few rows of the raw data, including the time at which the first data was recorded. Round
# to nearest minute for passing to stime, in quotes (e.g. "1100")
# interval = the binning interval of the data, typically 30, in quotes (e.g. "30")

### all variables except data name should be in quotes and NO SYMBOLS ANYWHERE: dfm1 ='M001', ###
### edate='180101', name='iso', sdate='01 dec 2018', stime='1600', interval='30' ###


# Example: txtclabdata(bin30.dfm1.180101.ft20, 'M001', '180101', 'cyc', '01 jan 2018', '0200', '30') 


####### 9 #######
# Once the data have been put into the proper format for ClockLab, you 
# can then view the data in ClockLab and run the appropriate analysis
# as you would with DAM data (see analysis protocol). You will still 
# likely want to visualize the data, both rough looks and also 
# manuscript-worthy plots, that is what follows in step 10 below.

####### 10 #######

##### SUPER IMPORTANT #####

# When loading an old workspace, chances are the old workspace
# will have functions (including those for plotting) already
# read in, and they will overwrite what you had in the workspace
# before loading. That means you should always reload the appropriate
# FLIC_hbfunctions.R file before plotting things to make sure
# everything will work appropriately. 

source("/Users/your_luc_id/folder_name/subfolder_name/FLIC_hbfunctions.R")

# There are a series of plotting functions in the FLIC_hbfunctions.R
# file that you will utilize here for data visualization. They are:
# 1. ind.plot() which will produce raw data plots for each fly by DFM
# 2. norm.plot() which will produce normalized plots for each fly by DFM
# 3. group.plot() which will produce normalized plots by DFM
# 4. genotype.plot() which will produce normalized plots by data frame
# 5. genotype.plot.fig() which will produce fancy plots w/out first 24 hrs

# 1-4 above need two objects, the data to be plotted and genotype, which
# needs only be any descriptive text placed in quotes:
# e.g. ind.plot(bin30.dfm1.180101.at, 'genotype name here')

# To plot using 4-5, you will need to construct a separate data frame than
# the binned data, made of just your genotypes of interest. See notes below
# starting at genotype.plot() details for how to do this.

# To plot using 5, need to include, color, size, and shape arguments to 
# the function as a third item, e.g. mutant.plot(data, 'name', 'color', size, shape). 
# Google "R Colors", ggplot size and shape for range of options (there are many!)

## A few notes about the use of the above plotting options one by one

# ind.plot() --- this plot will look messy as it has a rainbow color for 
# each of the wells. BUT, it can let you see any super strange things going
# on in rough shot. USE BINNED DATA ONLY OF SINGLE FLIC

# norm.plot() --- this plot will show the normalized plot for each fly by
# DFM, easier to see outliers that, if present, will usually be the result
# of a fly that died and therefore the feeding events that did happen
# before death will be way way above the average (normalized) feeding level.
# Can be helpful to confirm/identify deaths. norm.plot() will also output a
# set of information for each well in the RStudio console. There will be 
# a list of wells, 1-12, with either a '#' or an 'NA' next to it. If a 
# '#' sign is there, continuous data is present fo that well, if 'NA', then
# there is data missing at somepoint, likely because of a fly death. Another
# easy way to check on the data. USE BINNED DATA ONLY OF SINGLE FLIC

# group.plot() --- same as above, but will cluster an entire DFM together.
# Not particularly helpful given that flies are randomized by genotype in 
# DFMS, but you never know. USE BINNED DATA ONLY OF SINGLE FLIC

# genotype.plot.fig() function --- plotting function that will plot just 
# the data of a single genotype, but YOU must create a new dataframe that is 
# separated by genotype. Also can use the subset.data() function to pull out
# data that covers just days 2-7 (same as clocklab analysis) so you can 
# combine data across different experiments by genotype. 
# To make the special data frame, do the following:

####### MAKING A DATA FRAME OF SUBSET OF BINNED DATA FOR PLOTTING #######


### 1 ### 
# Pull out the data of a single genotype by cross-referencing the well 
# assignments by genotype, and extracting just those wells of data for 
# each DFM using the function subset.data(). This function requires eight
# arguments:
# 1. data = binned data for a dfm
# 2. idate = initial day of experiment in "YYYY-MM-DD" format
# 3. itime = itime = starting time of experiment in miltary time with no colon, 
# 4. stime = the time at which data is to start being collected based 
# on entrainment schedule, 
# 5. sday = the start day of data analysis, 
# 6. eday = end day of data analysis, typically
# 7. datatype = either "norm" or "nonnorm" for extracting data
# that is normalized using standard procedure or not
# 8. wells = the well numbers corresponding to desired genotype

# You must save the output of subset.data() as a new object
## Ex: genotype1.dfm1 <- subset.data(bin30.dfm1.date, "2018-02-01", 1755, 0900, 2, 7, "norm", 1,2,7,8)

# If you want the entire experiment's worth of data, use an sday value of 0 and an eday value
# greater than the total number of days the experiment ran, can be ridiculously high (e.g.100)
## Ex: genotype1.dfm1 <- subset.data(bin30.dfm1.date, "2018-02-01", 1755, 0900, 0, 100, "norm", 1,2,7,8)

### 2 ###
# Once you have extracted the data for each DFM for each genotype, you then need to combine them by 
# calculating the average feeding activity for the entire genotype, which equals the means of the 
# rows of data. To do this, use the function genotype.means() which only requires you to list as many
# subsetted data frames as you want, from 1 to as many as you have. 

# You must save the output of genotype.means() as a new object
## Ex: genotype1.all <- genotype.means(genotype1.dfm1, genotype1.dfm2, genotype1.dfm3)

# Once you have the genotype means, you can then plot the data using the function
# genotype.plot.fig() which requires up to nine arguments:
# 1. data = dataframe that is the mean of an entire genotype
# 2. title = the title of the plot in quotes, "like this"
# 3. genotypecol = the color of the plot, can also pass some set
# values for the gal4 control, experimental line, and uas control
# by just putting in "gal4", "exp", or "uas" as the value of 
# genotypecol to get set color for those lines. Alternatively,
# use any R approved color in quotes
# 4. size = the size of the points, defaults to 1.5
# 5. shape = the shape of the points, from 21-25 for filled shapes, default 
# is 21 which is a filled circle
# 6. low = the minimum of y axis
# 7. high = the maximum of y axis
# 8. by = the spacing of range on y axis
# 9. ribbon = either T or F for ribbon shading of error bars or box and whisker error bars.

## Ex: genotype.plot.fig(genotype1.all, "genotype1 normalized feeding", "exp", 1.5, 21, 0, 4, 1, T)

## Do not need to actually put in values of 4-9 above if prefer to use the defaults
## Ex. genotype.plot.fig(genotype1.all, "genotype1 normalized feeding", "exp")

## Be sure once you have made your dataframes for individual genotypes of 
## interest that you SAVE YOUR WORKSPACE so you never have to make the 
## individual data frames again should you want to revisit the plots in the
## future! Before saving the workspace, remove all extra objects present in 
## the workspace using the rm() function. Recommend leaving one binned 
## data object (e.g. bin30.dfm1.171210) and one raw data object (e.g. dfm1.171210.at)
## for reference should you revisit data for plotting.


####### 11 #######
# A comparision of the total amount of time each genotype spent feeding
# is possible by summing the column of feeding duration from the 
# Feeding.Duration.Well() function. The process for doing this comparison
# starts by creating a data frame of 12 rows for each DFM with the 
# experiment date, well number, total feeding time in seconds, and 
# the genotype number code. Next, the number code is replaced by actual
# text for the appropriate genotype. A few peices of information must 
# be input at the first and second stage. The functions to be called are
# feed.total() and feed.total.names(). feed.total() will create the 
# data.frame of 12 rows and the total time for each fly. Arguments
# to be passed to the function are:
# 1. dfm.data, 
# 2. idate = initial day of experiment in "YYYY-MM-DD" format, 
# 3. dfm = dfm number, 
# 4. itime = starting time of experiment in miltary time with no colon, 
# 5. stime = the time at which data is to start being collected based 
# on entrainment schedule, 
# 6. sday = the start day of data analysis, 
# 7. eday = end day of data analysis, typically
# sday will be 2 and eday will be 7 as is the case for ClockLab analysis,
# 8. a sequence of 12 numbers corresponding to the genotype codes

# Example: ftot.180101.dfm1 <- feed.total(dfm1.180101.at, 1, "2018-01-01", 1340, 0200, 2, 7, 
# 1, 1, 2, 3, 2, 3, 3, 2, 1, 2, 1, 3)

# The trick is to get the correct idate and itime. These values are most 
# easily extracted from the RawData portion of the dfm.data object, 
# visible by typing in head(dfm.data$RawData). You will then see the first
# few rows of data for a given expeirment, including the date and time
# to use in the feed.total() function. Round time off to the nearest minute
# when passing it to the function as itime. Refer to OneNote for the proper
# genotype number assignments.

# For each DFM, you will have to cross-reference the data with the excel
# analysis files and remove any wells that include data that was not used
# in the analysis. To to this, use the function feed.total.dropwell() which
# requires two arguments: data = the newly created data file from
# feed.total you need to remove a well from and well = the well number
# with a W in front of it, all in quotes (e.g. "W11"). This function 
# can only remove one well at a time, so proceed sequentially for multiples.

# Example: ftot.170505.dfm1 <- feed.total.dropwell(ftot.170505.dfm1, "W11")

# Once the total feeding objects have been made for each DFM of a particular
# date, you can move on to another date with the same genotypes, or compile
# all of the data from the single date. To compile the objects, it is simplest
# to use the function rbind() to bind the data frames of feeding data
# on top of one another. 

# Example: ftot.170505 <- rbind(ftot.270505.dfm1, ftot.270505.dfm2,
# ftot.170505.dfm3, ftot.270505.dfm4, ftot.270505.dfm5)

# With a compiled data frame of all of the total feeding values, then
# you can replace the 1's, 2's, and 3's with the actual text version of
# the genotypes using the function feed.total.names(). To do this, 
# you give the function the data frame you will be renaming, and then
# the text for each genotype.

# Example: ftot.170505.names <- feed.total.names(ftot.170505, one = "dilp2>+", 
# two = "dilp2>kir.2.1", three = "+>kir2.1")

# Now you should have a complete data frame with all of the individual
# feeding totals in seconds per fly, with the date of their experiment,
# the dfm they came from, their well assignment, feeding time, and 
# genotype ready for data analysis by genotype. You can save your product
# in two ways: as a workspace or just the data frame as an excel .csv file.
# Saving as .csv is easier and less memory, therefore recommended. To do so
# use the function write.csv() which will produce a .csv file in the 
# working directory you are in. Two arguments passed to the function, 
# the file to be written and the file name including the extension type, plus
# the statement, row.names = FALSE to prevent the row numbers from being passed
# to the final product

# Example: write.csv(ftot.170505.names, "170505_dilp2kir2.1_feedingsum.csv", row.names = FALSE)

# After saving a data frame as a .csv, that data frame can then be read into
# RStudio by setting your working directory to match the location of the desired
# .csv and using the function read.csv() with the file name in quotes and 
# the argument header = TRUE to maintin the names of the column headings. You 
# will also need to specify a new object as the read in data frame to be able 
# to reference it for analysis

# Example: ftot.170505.names <- read.csv("170505_dilp2kir2.1_feedingsum.csv", header=TRUE)


# Sample function to randomize genotype placements for FLIC experimental setup

matrix(sample(c(1,1,1,1,1,1,2,2,2,2,2,2), replace = FALSE), nrow=2, ncol=6)
matrix(sample(c(1,1,1,1,2,2,2,2,3,3,3,3), replace = FALSE), nrow=2, ncol=6)


