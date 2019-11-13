#### FLIC_phaseshift ####
### last updated 9/19/2019 ###


# To look for a phaseshift in the timing of behavioral events, can both plot the data to see and 
# extract the differences in hours of peak activity for each fly as compared to zeitgeber time
# which is typically lights on/off (Yao et al. 2016)


### 1 ###
# Start by assigning values for shared objects across all analyses, e.g. idate, itime
# Use the function FLIC_anticipation_objects() to do so, it will prompt user input for all
# required objects for analysis (idate, itime, etimeS, etimeE, sday, fday)

### 2 ### 
# Need to make objects for well assignments for each dfm by genotype, if not already
# done so for anticipation calculations. Objects need only be entered once. Use function
# FLIC_well_objects() to create an object for each dfm that can be passed to any
# of the other FLIC functions in place of wells, ...

# Ex: FLIC_well_objects('dfm1', 'genotype1', 1,2,3,4)
# Ex: FLIC_well_objects('dfm2', 'genotype1', 1,2,3,4)

#### Plotting of individual phase shifts ####
# To plot the data, use the function phaseshift_indfly_plot() which will plot the one day of data
# as dictated by you. phaseshift_indfly_plot() includes the following arguments:
# function(data, idate, itime, etimeS, etimeE, pday, fday, datatype, well, yhigh, by, day_col, title)
#  1. data = binned data for a DFM
#  2. idate = initial day of experiment in "YYYY-MM-DD" format
#  3. itime = starting time of experiment in miltary time with no colon
#  4. etimeS = the start of entrainment time (e.g. lights on)
#  5. etimeE = the end of entrainment time (e.g. lights off)
#  6. pday = the day of data you want to plot ** Will need to enter this separately
#  7. fday = the last day of data collected
#  8. datatype = either "norm" or "nonnorm" for extracting data
#  that is normalized using standard procedure or not
#  9. well = well that you want to plot (1-12)
#  10. yhigh = max Y axis value
#  11. by = tick marks 
#  12. day_col = color of the "daytime", 'white' if actual lights on, 'grey70' if subjective day

# Ex: phaseshift_indfly_plot(bin30.dfm1.data, idate, itime, etimeS, etimeE, pday, fday, 'norm', 1, 5, 1, 'gray20')

#### Extracting individual phaseshifts from morning and evening environmental transitions ####
# To look at the actual numerical differences in peak activity use function phaseshift_indfly_time() which
# will give the phase shift in hours of a single fly for morning (M) and evening (E) transition. Function
# includes the following arguments: <- function(data, genotype, idate, itime, etimeS, etimeE, pday, fday, datatype, well)
#  1. data = binned data for DFM
#  2. genotype = genotype of fly in quotes
#  3. idate = initial day of experiment in "YYYY-MM-DD" format
#  4. itime = starting time of experiment in miltary time with no colon
#  5. etimeS = the start of entrainment time (e.g. lights on)
#  6. etimeE = the end of entrainment time (e.g. lights off)
#  7. pday = the day of data you want to plot ** Will need to enter this separately 
#  8. fday = the last day of data collected
#  9. datatype = either "norm" or "nonnorm" for extracting data
#  that is normalized using standard procedure or not
#  10. well = well that you want data for (1-12)

# Ex: fly1_PS <- phaseshift_indfly_time(bin30.dfm1.data, 'genotype1', idate, itime, etimeS, etimeE, pday, fday, 'norm', 1)

#### Extracting genotype wide phaseshifts from morning and evening environmental transistions ####
# To pull out the numerical differences in peak activity use function phaseshift_genotype_time() 
# which will produce a table of values for each fly in a genotype from a single DFM. Deviation 
# uses the above individual fly time function, but compiles across multiple flies in a DFM. Uses
# almost the same syntax except for final argument: function(data, genotype, idate, itime, etimeS, etimeE, pday, fday, datatype, well, ...)
#  1. data = binned data for DFM
#  2. genotype = genotype of fly
#  3. idate = initial day of experiment in "YYYY-MM-DD" format
#  4. itime = starting time of experiment in miltary time with no colon
#  5. etimeS = the start of entrainment time (e.g. lights on)
#  6. etimeE = the end of entrainment time (e.g. lights off)
#  7. pday = the day of data you want to analyze
#  8. fday = the last day of data collected
#  9. datatype = either "norm" or "nonnorm" for extracting data
#  that is normalized using standard procedure or not
#  10. well = set of wells that you want data for with commas separating (1-12)

#  Ex: genotype1_dfm1_PS <- phaseshift_genotype_time(bin30.dfm1.data, 'genotype1', idate, itime, etimeS, etimeE, pday, fday, 'norm', 1,5,8,10)

#### Analysis ####

# To analyze phaseshift data, will need to extract all the deviations from environmental conditions
# across all DFMs using the phaseshift_genotype_time() function, and then combine them to make a single
# table for each genotype. Easiest to do this using the bind_rows() function from dplyr (part of tidyverse package)

# Ex: phaseshift_all <- bind_rows(genotype1_dfm1_PS, genotype1_dfm2_PS, ...)

# Once all data is combined, can run an ANOVA on it

#### Evening Phaseshift Start ####

# Can also look at the evening anticipation by identifying the 2-hr window with the 
# largest shift in activity prior to lights off (Lear et al 2009).
