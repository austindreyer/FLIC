#### FLIC_phaseshift ####
### last updated 5/22/2019 ###


# To look for a phaseshift in the timing of behavioral events, can both plot the data to see and 
# extract the differences in hours of peak activity for each fly as compared to zeitgeber time
# which is typically lights on/off

# To plot the data, use the function phaseshift_indfly_plot() which will plot the one day of data
# as dictated by you. phaseshift_indfly_plot() includes the following arguments:
# function(data, idate, itime, etimeS, etimeE, pday, datatype, well, yhigh, by, day_col, title)
#  1. data = binned data for a DFM
#  2. idate = initial day of experiment in "YYYY-MM-DD" format
#  3. itime = starting time of experiment in miltary time with no colon
#  4. etimeS = the start of entrainment time (e.g. lights on)
#  5. etimeE = the end of entrainment time (e.g. lights off)
#  6. pday = the day of data you want to plot
#  7. datatype = either "norm" or "nonnorm" for extracting data
#  that is normalized using standard procedure or not
#  8. well = well that you want to plot (1-12)
#  9. yhigh = max Y axis value
#  10. by = tick marks 
#  11. day_col = color of the "daytime", 'white' if actual lights on, 'grey20' if subjective day
#  12. title = name of the data in quotes 'title here'

# Ex: phaseshift_indfly_plot(bin30.dfm1.data, '2018-02-01', 1755, 0900, 2100, 3, 'norm', 1, 10, 2, 'gray20', 'title')


# To look at the actual numerical differences in peak activity, 

