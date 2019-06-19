#### Anticipation behavior ####
### last updated 6/18/2019 ###


# Stoleru Anticipation Index (Stoleru et al. 2004)
# AI = (b-1(b-1-b-2)(b-2-b-3)/b+1) where bi = activity
# counts in bin i, and i = number of bin before (-i) 
# and after (+i) light switch. The morning and evening
# anticipation index of each genotype was normalized
# to that of wild type (AIwt = 1). Note, this method
# is for an anticipation index for an entire genotype, 
# therefore, averaged raw data across a geneotype 
# must be passed to the anticipation index function.

### 1 ###
# Start by assigning values for shared objects across all analyses, e.g. idate, itime
# Use the function FLIC_anticipation_objects() to do so, it will prompt user input for all
# required objects for analysis

### 2 ### 
# Need to make objects for well assignments for each dfm by genotype. Use function
# FLIC_well_objects() to create an object for each dfm that can be passed to any
# of the other FLIC functions in place of wells, ...

### 3 ### 
# Create an object of the raw data for each genotype from each DFM
# using the AI_index_prep() function which includes the following arguments:
#  function(data, idate, itime, etimeS, pday, fday, well, ...)
#  1. data = data to be subsetted, binned data only
#  2. idate = initial day of experiment in "YYYY-MM-DD" format
#  3. itime = starting time of experiment in miltary time with no colon
#  4. etimeS = the start of entrainment time (e.g. lights on)
#  5. pday = the day of data you want to analyze
#  6. fday = the last day of data collected
#  7. well, ... = the well numbers of matching genotype, numbers only

#  Ex: genotype1_dfm1_AI <- AI_index_prep(bin30.dfm1.data, '2018-02-01', 1755, 0900, 7, 8,  5,7,11,12)
#  Ex: genotype1_dfm2_AI <- AI_index_prep(bin30.dfm2.data, '2018-02-01', 1755, 0900, 7, 8, 5,7,11,12)


### 4 ### 
# Now need to calculate the AI_index by including all the DFM data by genotype using 
# the AI_index() function which includes the following arguments:
#  function(etimeS, etimeE, genotype, data, ...)
#  1. etimeS = the start of entrainment time (e.g. lights on)
#  2. etimeE = the end of entrainment time (e.g. lights off)
#  3. genotype = genotype of fly in quotes
#  4. data, ... = the objects produced using AI_index_prep() fuction

# Ex: genotype1_AI <- AI_index(0900, 2100, 'genotype1', genotype1_dfm1_AI, genotype1_dfm2_AI)


# Harrisingh Anticipation Phase Score (Harrisingh et al. 2007)
# The percentage of activity in the 6 h before light transition that occurs
# in the 3 hours before the transition 

### 1 ###
# Need to make an object that calculates the AI phase score for each fly by DFM
# using the AI_phase_score() function which includes the following arguments:
# function(data, genotype, idate, itime, etimeS, etimeE, pday, fday, well, ...)
#  1. data = data to be subsetted, binned data only
#  2. genotype = genotype of fly, in quotes
#  3. idate = initial day of experiment in "YYYY-MM-DD" format
#  4. itime = starting time of experiment in miltary time with no colon
#  5. etimeS = the start of entrainment time (e.g. lights on)
#  6. etimeE = the end of entrainment time (e.g. lights off)
#  7. pday = the day of data you want to analyze
#  8. fday = the last day of data collected
#  9. well, ... = the well numbers of matching genotype, numbers only

# Ex: genotype1_dfm1_AIps <- AI_phase_score(bin30.dfm1.data, 'genotype1', 2018-02-01', 1755, 0900, 2100, 3, 7, 5,7,11,12)
# Ex: genotype1_dfm2_AIps <- AI_phase_score(bin30.dfm1.data, 'genotype1', 2018-02-01', 1755, 0900, 2100, 3, 7, 5,7,11,12)

### 2 ###
# Then need to combine all calculated AI phase scores for all flies, should be across all genotypes 
# as that identifier is included in the tables for analysis 

# Ex: AIps_all <- bind_rows(genotype1_dfm1_AIps, genotype1_dfm2_AIps)
