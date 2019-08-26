#### FLIC Data Analysis ####
#### updated 5/22/2019 ####

## Steps 1 -  below describe the process of analyzing data produced using the 
## script for FLIC_dataprocessing_plotting. Data analysis here is kept to the
## comparisons of means using t.tests and/or ANOVAs. Also includes script
## for plotting the data to compare to the results of analysis.

## NOTES: 
## 1. When working in R and using scripts like this one, all text
## directly following a # are comments only, will not affect any input.
## 2. There are two easy ways to run scripts from a file like this one
## into the R console below that will actually enact the code. One way
## is to highlight the text in the script and hit "cnrl + enter" (PC) or 
## "cmd + enter" (mac) and they will run in the R console below. The 
## Second way is to copy and paste the text from the script into the 
## R console below and then hitting "enter". 


####### 1 #######
# Store the data to be analyzed as a new object to manipulate. To do this
# you will need to navigate to the location where the data is using the setwd() 
# command which stands for "Set Working Directory" and will change the active 
# folder Rstudio is accessing.
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

attach("/Users/your_luc_id/folder_name/subfolder_name/FLICFunctions", pos=2)
source("/Users/your_luc_id/folder_name/subfolder_name/DFM.R")
source("/Users/your_luc_id/folder_name/subfolder_name/ParametersClass.R")
source("/Users/your_luc_id/folder_name/subfolder_name/CommonChamber.R")
source("/Users/your_luc_id/folder_name/subfolder_name/FLIC_hbfunctions.R")

# for mac laptop
attach("/Users/austindreyer/Documents/R/flic/FLICFunctions", pos=2)
source("/Users/austindreyer/Documents/R/flic/DFM.R")
source("/Users/austindreyer/Documents/R/flic/ParametersClass.R")
source("/Users/austindreyer/Documents/R/flic/CommonChamber.R")
source("/Users/austindreyer/Documents/R/flic/FLIC_hbfunctions.R")

# for lab computer
attach("/Users/adreyer/Desktop/FLIC/R/FLICFunctions", pos=2)
source("/Users/adreyer/Desktop/FLIC/R/DFM.R")
source("/Users/adreyer/Desktop/FLIC/R/ParametersClass.R")
source("/Users/adreyer/Desktop/FLIC/R/CommonChamber.R")
source("/Users/adreyer/Desktop/FLIC/R/FLIC_hbfunctions.R")

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
# set to the location of the FLIC data files, need to prepare for the
# analysis. All of the FLIC data files must be saved as .csv files for them 
# to be used here. To read in a data file as a new object use the read.csv() function

# Ex: feedingdata.170505 <- read.csv("170505_dilp2kir2.1_feedingsum.csv", header = T)

####### 5 #######
# For all analyses you will be comparing groups of continuous variables (e.g. power) 
# based on some sort of categorical variable (e.g. genotype). The syntax for a
# statistical test is continuous variable first, then categorical variable(s), separated
# by a tilde ~. The function used to do the analysis is aov() and it creates an aov object, 
# so you must save the actual aov() as a new object to reference.  
# The aov() function also needs the data frame being analyzed included which means 
# you can directly reference the column names in the creation of the AOV object. 

# Ex: test.aov <- aov(power ~ genotype, data = data)

# To see the results of the ANOVA, you want to look at the summary of the object created

# Ex: summary(test.aov)

##### The summary will look something like this:##########################
#> summary(test)                                                         #  
#             Df  Sum Sq Mean Sq F value  Pr(>F)                         #
#Diet          3  155863   51954   10.81 6.43e-07 ***                    #
#  Residuals   574 2758693    4806                                       #  
#---                                                                     #  
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1        #
##########################################################################

# Giving you the degrees of freedom (Df) the sum of squares, mean squares, fvalue, and p value. 
# ANOVAs are statistical tests to compare three or more groups together. To compare two groups, 
# you should use a t.test

# When doing ANOVA, also important to do the multiple comparisons test between all groups using
# Tukey's Honest Significant Difference test. This will provide a statistical test of differences
# between each group by doing a pairwise comparison of all possible combinations. You will use
# the ANOVA object created in the Tukey function

# Ex: TukeyHSD(test.aov)


# Output will look something like this:

################################################################################################
# > TukeyHSD(test.aov)                                                                         #
# Tukey multiple comparisons of means                                                          #
# 95% family-wise confidence level                                                             #
#                                                                                              #
# Fit: aov(formula = Power ~ Genotype, data = data)                                            #
#                                                                                              #
#                                                                                              #
#  $Genotype                                      diff        lwr        upr       p adj       #                                                       
# SIFA3 x SIFD2-SIFA3 x Iso                       -33.347436 -51.948499 -14.746373 0.0001253   #
# SIFA3 x SIFD2/cyo;SIF(1-4)/TM6,Sb-SIFA3 x Iso    -9.721237 -28.205678   8.763203 0.4271465   #
# SIFA3 x SIFD2/cyo;SIF(1-4)/TM6,Sb-SIFA3 x SIFD2  23.626199   5.141758  42.110639 0.0083054   #
#                                                                                              #
################################################################################################

####### 6 #######

# The syntax for a t.test are largely the same as for an anova. No need to store the output of 
# t.test function in a new object, can just read out the results immediately 

#Ex: t.test(power ~ genotype, data = data)

###### Results:
#Welch Two Sample t-test

# data:  extra by group
# t = -1.8608, df = 17.776, p-value = 0.07939
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -3.3654832  0.2054832
# sample estimates:
#  mean in group 1 mean in group 2 
#         0.75            2.33 


####### 7 ########

# When doing statistical analysis on means, can be very helpful to visualize the data 
# in addition to getting the statistical results. Easiest way to look at means is to 
# look at box plots. Box plots show the distribution of the the data
# with the box highlighting the mean and quantiles. 

### Box Plot ###

# The syntax for boxplots is the same as for aov: boxplot(variable ~ category, data = data, xlab = "text", ylab = "text")

# To add raw data, you will need to use the points() function which requires two inputs and 
# a couple of aesthetic options. Points will overlay points to the existing plot in the Plots panel
# so be sure you have already created the boxplot. 

# The two inputs are the x and y variables to be added, which will typically be the genotype
# for the x variable and the continuous variable for the y (e.g. power or period). To add them
# you will have to specify they are coming from the a specific data set of interest using the 
# "$" symbol to direct R to specific columns.The aesthetic options are pch and col, pch dictates
# which type of symbols will be added (usually solid circles = 19), col is the color of the 
# points to be added (in quotes, usually "black") and cex dictates how large the points are 
# (usually 0.75)

# Ex: points(data$genotype, data$power, pch = 19, col = "black", cex = 0.75)


##### SUPER IMPORTANT #####

# When loading an old workspace, chances are the old workspace
# will have functions (including those for plotting) already
# read in, and they will overwrite what you had in the workspace
# before loading. That means you should always reload the appropriate
# FLIC_hbfunctions.R file before plotting things to make sure
# everything will work appropriately. 

source("/Users/your_luc_id/folder_name/subfolder_name/FLIC_hbfunctions.R")



