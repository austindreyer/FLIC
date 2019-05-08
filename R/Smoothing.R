## Smoothing Script
## Updated 3/5/2019

# LOWESS/LOESS (LOcally Weighted Scatterplot Smoothing) option
# Works by fitting a polynomial to a set of data points, weighting the middle point
# of the set more, then using that polynomial to construct a little piece of a 
# curve, with the entire curve being made as a combination of all the polynomials
# for sets of overlapping data encompassing the entire data set

library(stats)

# using the loess function: loess(formula, data, span)
# formula is in the form of predictor ~ response
# data = data
# span is a value between 0 and 1 that dictates which proportion of total
# points will used in the shifting window, something in the 0.05-0.1 range
# for more info: https://stats.stackexchange.com/questions/2002/how-do-i-decide-what-span-to-use-in-loess-regression-in-r

# want to do the loess fit on a set of genotype mean data and save it as an 
# object to pull out the period lengths later

# Ex: data.iso.loess <- loess()

# to add error lines for confidence intervals: https://stackoverflow.com/questions/22717930/how-to-get-the-confidence-intervals-for-lowess-fit-using-r


## Can also use filtering to remove points that are outside of a desired frequency
## range. The Butterworth filter is a commonly used one, can do a low-pass or 
## high-pass filter, which keeps low and high frequency data, respectively. For 
## circadian stuff, want to use low-pass as ~24 hr rythms are definitely low

# To use Butterworth filter, need to create the actual filter object, and then 
# can plot it using a forward and reverse filtering option which imposes the created
# filter on the data.

# First create the Butterworth filter

library(signal)

# bf <- butter(n, W, type, plane):
# n = polynomial order, default 2 (which is commonly used)
# W = critical frequencies of filter, value of ~.1 works well
# type = 'low', for low-pass
# plane = 'z', for digital filter

bf <- butter(2, 0.1, type = 'low', plane = 'z')

# then need to apply filter to data, using filtfilt() function which
# is a forward and reverse filtering protocol

data.bf <- filtfilt(bf, ydata)

# bf = filter just made
# ydata = the data to be filtered, and only that data. So for activity response
# for feeding, just the mean data from clocklab by genotype

# then can plot to inspect results, first plot the actual data, then add the 
# filtered response as a line using lines() function

lines(xdata, data.bf, col = 'red')

# more info for reference: https://stackoverflow.com/questions/7105962/how-do-i-run-a-high-pass-or-low-pass-filter-on-data-points-in-r/49905340#49905340
# http://www.cs.sfu.ca/~ggbaker/data-science/content/filtering.html
# 