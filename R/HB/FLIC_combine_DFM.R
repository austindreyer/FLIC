#### Combining broking DFM Files ####
### last updated 6/19/19 ###


# To combine DFM files that are of a single experiment but had a data interruption, the goal
# is to construct one contiguous file that has a continuously increasing value for the sample column.
# To achieve this, have to read in each part of the data, and stitch them all together, 
# modifying the sample column at each step to make it continuous. Need to be careful, however,
# as each DFM file will have the same exact name based on the output from the MCU, and that is
# the same name as the final product needs to have to be correctly recognized by the 
# DFM_Class() function. CAUTION when running through this script so you don't accidentally
# overwrite an important file.

### 1 ###
# Need to set the working directory to the location of the first batch of DFM files to be read in.

# Ex: setwd("/users/desktop/FLIC/FLIC_rawdata/firstDFMdata")

### 2 ###
# Need to read in each DFM file as csv with a name indicating the order the DFM files need
# to end up in, changing the working directory as neccessary to get them all. Notice, the
# DFM file name will be the same in each directory!

# Ex: dfm1.1 <- read.csv("DFM_1.csv")

# Listed here for quick read-in
dfm1.1 <- read.csv('DFM_1.csv')
dfm2.1 <- read.csv('DFM_2.csv')
dfm3.1 <- read.csv('DFM_3.csv')
dfm4.1 <- read.csv('DFM_4.csv')
dfm5.1 <- read.csv('DFM_5.csv')

dfm6.1 <- read.csv('DFM_6.csv')
dfm7.1 <- read.csv('DFM_7.csv')
dfm8.1 <- read.csv('DFM_8.csv')
dfm9.1 <- read.csv('DFM_9.csv')
dfm10.1 <- read.csv('DFM_10.csv')

dfm11.1 <- read.csv('DFM_11.csv')
dfm12.1 <- read.csv('DFM_12.csv')
dfm13.1 <- read.csv('DFM_13.csv')
dfm14.1 <- read.csv('DFM_14.csv')
dfm15.1 <- read.csv('DFM_15.csv')


# Ex: setwd("/users/desktop/FLIC/FLIC_rawdata/secondDFMdata")
# Ex: dfm1.2 <- read.csv("DFM_1.csv")

# Listed here for quick read-in

dfm1.2 <- read.csv('DFM_1.csv')
dfm2.2 <- read.csv('DFM_2.csv')
dfm3.2 <- read.csv('DFM_3.csv')
dfm4.2 <- read.csv('DFM_4.csv')
dfm5.2 <- read.csv('DFM_5.csv')

dfm6.2 <- read.csv('DFM_6.csv')
dfm7.2 <- read.csv('DFM_7.csv')
dfm8.2 <- read.csv('DFM_8.csv')
dfm9.2 <- read.csv('DFM_9.csv')
dfm10.2 <- read.csv('DFM_10.csv')

dfm11.2 <- read.csv('DFM_11.csv')
dfm12.2 <- read.csv('DFM_12.csv')
dfm13.2 <- read.csv('DFM_13.csv')
dfm14.2 <- read.csv('DFM_14.csv')
dfm15.2 <- read.csv('DFM_15.csv')

### 3 ###
# Once all DFM files are read in, need to navigate to the directory that
# you want to save the new combined files in, then combine them using 
# the FLIC_combine_DFM() function.

# Ex: setwd("/users/desktop/FLIC/FLIC_rawdata/finalDFMdata")

# Ex: FLIC_combind_DFM(dfm1.1, dfm1.2, "DFM_1.csv")

