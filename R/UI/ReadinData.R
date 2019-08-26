## updated 7/26/2019

#Reads in raw data

#sets up var "firstdfm" and "lastdfm" to read in 5 DFM's at a time and begins a loop for each reading
startdfm -> firstdfm
for (j in 0:runnumber){
(firstdfm+4) -> lastdfm
if (lastdfm > enddfm){lastdfm <- enddfm} #ensures the final DFM does not exceed last user input DFM
  if (firstdfm > enddfm){firstdfm <- enddfm}
##Reads in raw data across all selected DFMs
if (monitor == "sable") {
  for(i in firstdfm:lastdfm){
  DFMClass(i,p10)
  print(i)
  }
} else if (monitor == "v2") {
  for(i in firstdfm:lastdfm){
  DFMClass(i,p)
  print(i)
  }
}
##Checks for databreaks
for(i in firstdfm:lastdfm){
  c("DFM", i) -> a
  str_c(a, collapse = "") -> a
  get(a) -> a
  FindDataBreaks(a)
}


##Bins data
for(i in firstdfm:lastdfm){
  #Here a is the name to be assigned to the unbinned numeric data, b is the data itself. c will be temprarily used to hold the data while the name of b is used to remove the DFM
  c("dfm", i, ".", date, ".", id, ".", threshold) -> a
  str_c(a, collapse = "") -> a
  c("DFM", i) -> b
  str_c(b, collapse = "") -> b
  get(b) -> c

  #Changes all time information to numeric
  b <- DFMData_numeric(c, threshold)
  
  
  #Extracts start time from the dfm data
  as.vector(b$RawData$Time[1]) -> starttime
  #Removes : from the timestamp
  gsub(':', '', starttime) -> starttime
  #rounds the time
  as.numeric(starttime) -> starttime
  round(starttime, -2) -> starttime
  substr(starttime, 1, 4) -> starttime
  
  #Adds a dfmX.date.id.threashold object for post analysis reference
  assign(paste(a), b)
  
  Feeding_Events_DFMPlots(b,"raw")
  
  #now a is used to temporarily hold the binned data before being renamed to whatever 'c' is

  a <- BinFeedingData.Licks(b, binlength)
  #here c is the name to be assigned to the binned data
  c("bin", binlength, ".", "dfm", i, ".", date, ".", id, ".", threshold) -> c
  str_c(c, collapse = "") -> c
  assign(paste(c), a) #assigns a or the binned data to a new object with the name of c, these steps are not strictly necessary but reproduce the formatting previously used
  
  #Here b is used to create the 'M0xx id needed by clocklab
  str_pad(i, 3, side="left", pad="0") -> b
  c("M", b) -> b
  str_c(b, collapse = "") -> b
  
  get(c) -> c
  #Now we create the text files to be used by clocklab
  txtclabdata(c, b, date, abbrid, fulldate, starttime, binlength)
  c("DFM", i) -> b
  str_c(b, collapse = "") -> b
  
  #Removes the DFM
  rm(list = b)
}
rm(a, c)
#Saves the workspace
c(directory, "/", date, "_", id, "_DFM" , firstdfm,"-",lastdfm, ".RData") -> a
str_c(a, collapse = "") -> a
save.image(a)
#Cleans up the global enviornment for next run to free up space
if (runnumber != j){
  for(k in firstdfm:lastdfm){
    c("dfm", k, ".", date, ".", id, ".", threshold) -> a
    str_c(a, collapse = "") -> a
  rm(list = a)
  c("bin", binlength, ".", "dfm", k, ".", date, ".", id, ".", threshold) -> c
  str_c(c, collapse = "") -> c
  rm(list = c)
                       }
}
firstdfm <- (lastdfm+1)
}
