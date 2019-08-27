#### FLIC_meanactivity_plot ####
### last updated 6/5/2019 ###

# Script to plot the averaged activity of a genotype, need
# to collect all of the required data first, calculate the 
# average activity with error bars, and then plot it

### 1 ###
# First need to subset the data for the same days across
# which data will be averaged, accounting for the difference in 
# days necessary for the full behavior plot (i.e. 6 hours prior to 
# environmental shift is when data should start, in the example below,
# 0900 would correspond to a lights on of 1500)

# Ex: genotype1_dfm1_d2_7 <- subset.data(bin30.dfm1.date, "2018-02-01", 1755, 0900, 2, 7, "norm", "running", 1,2,3,4)
# Ex: genotype1_dfm2_d2_7 <- subset.data(bin30.dfm2.date, "2018-02-01", 1755, 0900, 2, 7, "norm", "running", 1,2,3,4)

### 2 ###
# Second need to combine the data from each DFM using the 
# combine_days() function

# Ex: genotype1_all_d2_7 <- combine_days(6, genotype1_dfm1_d2_7, genotype1_dfm2_d2_7)

### 3 ###
# Third need to plot it using the function day_meanbehav_plot() which has 
# four arguments: day_meanbehav_plot <- function(data, yhigh = 3, by = 0.5, title)
#  1. data = combined data from combine_days() function
#  2. yhigh = maximum y axis value (defaults to 3)
#  3. by = desired y axis tick mark scale (defaults to 0.5)
#  4. title = title of plot in quotes

# Ex: day_meanbehav_plot(genotype1_all_d2_7, 4, 1, 'genotype 1')