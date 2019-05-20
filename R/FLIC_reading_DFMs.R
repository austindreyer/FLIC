#### DFM Initialization Script ####
### Updated 5/10/2019 ###

# This script is to be used when setting up a FLIC experiment and when first reading in 
# a set of raw FLIC data to the computer after an experiment. It contains two key things:
# 1. The randomization of location in each DFM by genotype for the flies
# 2. The syntax for reading in a set of DFMs for analysis. They are listed here such that
# you can use "Find" + "Replace" for the dates to expedite the process, rather than having to 
# manually remake the objects each time, as the date is the only component that changes.

##### 1 #####

# To randomly assign a fly to a well, use the function FLIC_random(num_fly, num_gen)
# 1. num_fly = the number of flies included in the DFM, either 12 or 6 (defaults to 12)
# 2. num_gen = the number of geneotypes included in the DFM (defaults to 3)

# Can just run FLIC_random() and the default values will be used. Output will be  either
# a 6X2 grid (12 flies) or 6X1 grid (6 flies) with numbers corresponding to the genotypes
# which can then be added to the OneNote entry for the experiment

# Ex: FLIC_random()

# Output:
# [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]    3    1    1    2    2    3
# [2,]    1    3    1    2    3    2

##### 2 #####

# To read in DFMs from the raw data files for data analysis, use this script,
# (referenced from FLIC_dataprocessing_plotting.R script, step 7)
# To ensure all dates have been changed when analyzing multiple dfms at once,
# best to copy and paste a complete set of objects (e.g. DFMClass() through 
# BinFeedingData.Licks()) into a new R script and just to a "Find" and "Replace"
# search for the old date (e.g. 180201) replacing it with new one (180505).

# To read in the data en masse, use the objects listed in the "Reading_DFMs.R" script 
# adjusting the dates and names as necessary. Copy and paste all of it at once 
# into the working window of RStudio, and then hit enter. The anaylsis should 
# start, and within a minute or two you should see it counting up by well through the data.

########## DFMs 1-5 v2.1 ###########
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

########## DFMs 6-10 Sable ###########
dfm6.190327.at <- DFMClass(6,p10) # The number here will refer to the DFM_1 data that is in the working directory you set at the beginning
dfm7.190327.at <- DFMClass(7,p10)
dfm8.190327.at <- DFMClass(8,p10)
dfm9.190327.at <- DFMClass(9,p10)
dfm10.190327.at <- DFMClass(10,p10)

dfm6.190327.at <- DFMData_numeric(dfm6.190327.at, "at")
dfm7.190327.at <- DFMData_numeric(dfm7.190327.at, "at")
dfm8.190327.at <- DFMData_numeric(dfm8.190327.at, "at")
dfm9.190327.at <- DFMData_numeric(dfm9.190327.at, "at")
dfm10.190327.at <- DFMData_numeric(dfm10.190327.at, "at")

bin30.dfm6.190327.at  <- BinFeedingData.Licks(dfm6.190327.at,30) # BE SURE YOU CHANGE ALL REFERENCES TO DFM NUMBER in the function BinFeedingData.Licks() and in the object name on the left
bin30.dfm7.190327.at  <- BinFeedingData.Licks(dfm7.190327.at,30)
bin30.dfm8.190327.at  <- BinFeedingData.Licks(dfm8.190327.at,30)
bin30.dfm9.190327.at  <- BinFeedingData.Licks(dfm9.190327.at,30)
bin30.dfm10.190327.at  <- BinFeedingData.Licks(dfm10.190327.at,30)

########## DFMs 11-15 Sable ###########
dfm11.190327.at <- DFMClass(11,p10) # The number here will refer to the DFM_1 data that is in the working directory you set at the beginning
dfm12.190327.at <- DFMClass(12,p10)
dfm13.190327.at <- DFMClass(13,p10)
dfm14.190327.at <- DFMClass(14,p10)
dfm15.190327.at <- DFMClass(15,p10)

dfm11.190327.at <- DFMData_numeric(dfm11.190327.at, "at")
dfm12.190327.at <- DFMData_numeric(dfm12.190327.at, "at")
dfm13.190327.at <- DFMData_numeric(dfm13.190327.at, "at")
dfm14.190327.at <- DFMData_numeric(dfm14.190327.at, "at")
dfm15.190327.at <- DFMData_numeric(dfm15.190327.at, "at")

bin30.dfm11.190327.at  <- BinFeedingData.Licks(dfm11.190327.at,30) # BE SURE YOU CHANGE ALL REFERENCES TO DFM NUMBER in the function BinFeedingData.Licks() and in the object name on the left
bin30.dfm12.190327.at  <- BinFeedingData.Licks(dfm12.190327.at,30)
bin30.dfm13.190327.at  <- BinFeedingData.Licks(dfm13.190327.at,30)
bin30.dfm14.190327.at  <- BinFeedingData.Licks(dfm14.190327.at,30)
bin30.dfm15.190327.at  <- BinFeedingData.Licks(dfm15.190327.at,30)


dfm1.190207.at <- DFMClass(1,p) # The number here will refer to the DFM_1 data that is in the working directory you set at the beginning
dfm2.190207.at <- DFMClass(2,p)
dfm3.190207.at <- DFMClass(3,p)
dfm4.190207.at <- DFMClass(4,p)
dfm5.190207.at <- DFMClass(5,p)

dfm1.190207.at <- DFMData_numeric(dfm1.190207.at, "at")
dfm2.190207.at <- DFMData_numeric(dfm2.190207.at, "at")
dfm3.190207.at <- DFMData_numeric(dfm3.190207.at, "at")
dfm4.190207.at <- DFMData_numeric(dfm4.190207.at, "at")
dfm5.190207.at <- DFMData_numeric(dfm5.190207.at, "at")

bin30.dfm1.190207.at  <- BinFeedingData.Licks(dfm1.190207.at,30) # BE SURE YOU CHANGE ALL REFERENCES TO DFM NUMBER in the function BinFeedingData.Licks() and in the object name on the left
bin30.dfm2.190207.at  <- BinFeedingData.Licks(dfm2.190207.at,30)
bin30.dfm3.190207.at  <- BinFeedingData.Licks(dfm3.190207.at,30)
bin30.dfm4.190207.at  <- BinFeedingData.Licks(dfm4.190207.at,30)
bin30.dfm5.190207.at  <- BinFeedingData.Licks(dfm5.190207.at,30)
