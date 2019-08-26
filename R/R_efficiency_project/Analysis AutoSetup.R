## updated 7/26/2019
#setup program
library(stringr) ##for adding strings to inputs

#Reading in scripts
print("There are several R scripts you need to have downloaded from the Cavanaugh Lab OneDrive. ")
print("If it is not already in, copy the location of the file with the scripts and paste as instructed below.")
 getwd() -> directory
print("Current working directory is:")
print(getwd())
readline(prompt="Enter the directory of the scripts, if it is the current working directory or already in 'scriptdir' enter '0': ") -> a
if (a != 0){
  a -> scriptdir
  str_replace_all(scriptdir, "\\\\" , "/") -> scriptdir # replaces all backslashes with forward, R does not recognize backslashes
} else {getwd() -> scriptdir}

#set wd
print("The working directory is the file R will read and write to.")
print("It should be the file location of the data you want to work with. Current working directory is:")
print(getwd())

readline(prompt="Enter working directory, if already entered write '0': ") -> directory
str_replace_all(directory, "\\\\" , "/") -> directory # replaces all backslashes with forward, R does not recognize backslashes
if (directory != 0){
setwd(directory)}
getwd() -> directory

c(scriptdir, "/FLICFunctions") -> a
str_c(a, collapse = "") -> a
attach(a, pos=2)
c(scriptdir, "/DFM.R") -> a
str_c(a, collapse = "") -> a
source(a)
c(scriptdir, "/ParametersClass.R") -> a
str_c(a, collapse = "") -> a
source(a)
c(scriptdir, "/CommonChamber.R") -> a
str_c(a, collapse = "") -> a
source(a)
c(scriptdir, "/FLIC_hbfunctions.R") ->a
str_c(a, collapse = "") -> a
source(a)
## loading in librarys
library(plyr)
library(tidyverse)
library(MASS)
library(reshape2)
##reload dplyr to establish precedence of functions
detach(package:dplyr)
library(dplyr)
## increase memory
memory.limit(99999999)
menu(c("Yes", "No"),graphics = FALSE, title = "Begin MonitorSetup?") -> a
if (a == 1L){
  c(scriptdir, "/MonitorSetup.R") -> a
  str_c(a, collapse = "") -> a
  source(a)
}
