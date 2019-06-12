#### FLIC_clocklab_protocol ####
### Last updated 6/11/2019 ###

## The following protocol is also available in the FLIC_Protocols file in the
## "Protocols" folder of the OneDrive -> Feeding Project directory


# ClockLab Analysis 

### 1 ###
# Go to the ClockLab software and open the text files created in R using txtclabdata() in the 
# Feeding Project>FLIC_Data folder (just clicking on the first one will open all of them). 
# NOTE: if asked, select open individual files

### 2 ###
# Create a new subfolder for your analysis in your personal “/Feeding Project/ClockLab_Analysis” folder 
# (example: 18_0724_siftrpa1_FLIC). 

### 3 ###
## Actogram settings ##
# Adjust the settings of both the actogram (open by default) and the periodogram (opened by selecting Analyses>Periodogram). 
# For the actogram, Double Plot and Zero Lines (both found under Settings) should be selected. If the experiment 
# included a temperature shift, the start date should be day 3 and the end date should be day 8. If not, the start 
# date should be day 2 and the end date should be day 7. Both numbers under Hour should match the start of the day
# based on the entrainment schedule of the flies in the experiment. Also need to consider daylight savings time, 
# making the start time one hour earlier than actual time if the experiment was run during daylight savings time, 
# because the data laptops and incubators are not adjusted for daylight savings time. 
# Other settings for actogram: Tau=24, Bin=30, Type=Scale, and Max=10. When actogram settings are set, use the 
# "Snipping Tool" to take a screenshot of the left side of the panel, including the file name at the top
# of the window, and the settings themselves, for future reference should there be any discrepencies for what the 
# settings were during analysis. This image should be saved in the folder created in step 2

## Periodogram settings ##
#For the periodogram, Analysis should be Chi Squared, Start=16, End=32, and Significance=0.01. When periodogram
# settings are set, use the "Snipping Tool" to take a screenshot of the left panel, including the file name at the top
# of the window, the settings, and the data for that particular fly for future reference. This image should be 
# saved in the folder created in step 2

### 4 ###
# Export the analyzed data to Excel by pressing Export>Batch Export Table Data from the Periodogram window and selecting 
# the text files of all the wells from the appropriate FLIC_Data folder by clicking the first text file, holding “Shift”, 
# and clicking the last text file. 

### 5 ### 
# Open the FLIC_Data_analysis_template, which can be found in the OneDrive (Feeding Project). There are three tabs in the template
# file, from left to right: 1. date_genotype, 2. date_genotype.csv, 3. genotype_data. They are populated during the analysis first
# into the genotype_data tab, then the date_genotype tab, then the date_genotype.csv tab, and described below in that order

## genotype_data tab ##
# Copy the exported data to the genotype_data tab in its entirety. Save this file with the appropriate name (Ex: 18_0724_siftrpa1_FLIC_Data) 
# to the subfolder within the ClockLab_Analysis made in step 2 above to avoid inadvertently overriding the template. 

## date_genotype tab ##
# 1 #
# In the date_genotype tab, you start by filling in the genotypes column (column A), date (column B), and monitors type used for 
# the experiment (column Q) as either “v2.1” or “Sable”. 

# 2 #
# Using the OneNote entry of the experiment for reference, copy the data from the proper wells (only the columns from Filename to Chi^2 2) 
# from the Periodogram_Table Data.csv file that was produced via Batch Export Table Data in ClockLab and place them under their 
# respective genotypes. To copy data, it easiest to use the temporary Excel file output from ClockLab rather than the separate tab 
# on the new analysis file. To select genotypes by each DFM, highlighting the appropriate columns for the first fly
# then press and hold "Ctrl" and highlight the next set of columns for the next fly, etc. Selecting multiple flies in this way
# can be cumbersome as it is difficult to undo an accidental selection of a wrong fly, so recommend only doing one DFM worth 
# of flies at a time rather than large groups of flies to avoid reselecting multiple times.Once all flies of a given genotype from
# a DFM are are selected, hit “C” (because you should already be holding down "Ctrl") to copy the files. Move to the 
# genotype_data tab of the analysis file and then hit “Ctrl” + “V” to paste the rows, which will be pasted together with no row
# spaces between rows even if they were spaced out when copied. This is the advantage of using "Copy" rather than "Cut" to move
# data, "Cut" will not paste the rows together.

# Return to the temporary Excel file from ClockLab and with the recently copied rows still selected, hit “Delete” to remove the 
# data from the file so you can confirm you are taking all the data as you go, and avoid confusion.  

# Repeat for each DFM and each genotype, by the end you should have no rows left in the temporary Excel file from ClockLab, 
# confirming successful migration of all data. Once finished, format all pasted cells to be font Arial size 10 with All Borders. 

### 6 ###
# Once the data have been moved to the new analysis file, inspect the actogram data in ClockLab by using Ctrl+N to move forward 
# and Ctrl+L to move backward in order to identify any dead flies, empty wells, or abnormalities in feeding behavior. 

# Dead flies should be marked as “Dead” in the "Dead column "Fly Status" column of the Excel sheet made in step 2, and their 
# values in the Period and Power columns (columns N&O) should be deleted. Wells without any data should have their entire row of data 
# deleted from colmun F to O, but leave the File Name column for future reference of the missing fly. This flies should also 
# be marked as "Exclude" in the "Fly Status" column to avoid including them in Rhythm percentage calculation (column P). Wells 
# that have strange data in actogram (e.g. data gets significantly stronger or weaker over the course of the experiment) should
# have a short description of the issue added in the "Notes" column (column R) and also be maked as "Exclude" with the values
# for the Period and Power columns removed.

# Each fly should be determined as either rhythmic or arrhythmic, indicated by a Y or N in the Rhythmic column (column P).  
## FLIC v2.1 monitors ##
# Using the FLIC v2.1 monitors, the fly is considered rhythmic if its Power is 10 or greater, and it is considered 
# arrhythmic if its Power is less than 10.  
## FLIC Sable monitors ##
# Using the Sable monitors, the fly is considered rhythmic if its Power is 25 or greater, and it is considered arrhythmic 
# if its Power is less than 25. 

# If the fly’s Power is less than 0, change the value to 0. For any fly that is classified as arrythmic, delete the Period value
# for that fly (column N) because an arrythmic fly should not have it's period included in the genotype mean.

## date_genotype.csv tab ##
# Copy all data from the main tab to the csv tab, excluding the empty rows, and those with labels and summary totals. 
# and save the csv tab as a “CSV (Comma delimited) (*.csv)” file. Excel will warn you that only the active tab can be 
# saved as a .csv, that is fine because we are saving the tab as a .csv to be read in to R for analysis later and
# only want that one tab's worth of data.

### 7 ###

# After the .csv tab has been saved, select the genotype_date tab and be sure to save the enitre FLIC_Data file as an 
# .xlsx file again to ensure all three tabs are saved for future reference. The Periodogram_Table Data file exported 
# from ClockLab does not need to be saved. 

# Copy the entire FLIC_Data file to the OneDrive (Feeding Project>Clocklab_Analysis). 