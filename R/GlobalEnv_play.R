myfunc <- function(data, well) {
  thing <- deparse(substitute(data))
  
  things <- strsplit(thing, '[.]')
  
  res <- lapply(things, function(ch) grep("dfm", ch))
  
  out <- things[[1]][res[[1]]]
  
  welld <- paste0("W", well)
  title <- paste(out, welld, sep = '_')
 
  
}

# extract name from inputs

things <- deparse(sustitute(data))

dfm_wells <- function()
{
  dfm <- readline('Enter the dfm number (e.g. dfm1): ')
  gtype <- readline('Enter the genotype using letters only (e.g. pdfiso): ')
  wells <- readline('wells: ')
  wells <- as.numeric(unlist(strsplit(wells, ',')))
  name <- paste(gtype, dfm, 'wells', sep = '_')
  assign(name, wells, envir = .GlobalEnv)
}


## function to make objects for each genotype's well assignments

FLIC_well_objects <- function(dfm, genotype, wells, ...) 
{
  
  wells
  wells <- c(wells, ...)
  #well.des <- paste0("W",wells)
  
  for (i in 1:(length(wells))) 
       {
         name <- paste(genotype, dfm, 'wells', sep = '_')
         assign(name, wells, envir = .GlobalEnv)
        }
}

# function to assign object values for FLIC analysis functions

FLIC_objects <- function()
{
  # warning about overwriting old objects
  invisible(readline(prompt=" WARNING: this function will write values for R objects to the global environment,
        overwriting anything with the same name.
        Press [enter] to continue
        Press [esc] to exit"))
  
  # initial date of experiment
  initial_date <- readline('Enter value of idate in yyyy-mm-dd format: ')
  assign('idate', initial_date, envir = .GlobalEnv)
  
  # start time of total experiment 
  initial_time <- readline('Enter value of itime (start time of experiment) in military time: ')
  initial_time <- as.numeric(initial_time)
  assign('itime', initial_time, envir = .GlobalEnv)
  
  # entrainment start time
  entrain_time <- readline('Enter value of stime (beginning of entrainment schedule) in military time: ')
  entrain_time <- as.numeric(entrain_time)
  assign('stime', entrain_time, envir = .GlobalEnv)
  
  # first day of data desired for analysis
  start_day <- readline('Enter value of start day for data analysis: ')
  start_day <- as.numeric(start_day)
  assign('sday', start_day, envir = .GlobalEnv)
  
  # last day of data desired for analysis
  end_day <- readline('Enter value of last day for data analysis: ')
  end_day <- as.numeric(end_day)
  assign('eday', end_day, envir = .GlobalEnv)
  
  # the type of data to extract
  d_type <- readline('Enter the type of data to extract, norm or nonnorm: ')
  assign('datatype', d_type, envir = .GlobalEnv)
  
  # the time scale to use
  h_set <- readline('Enter the timescale desired, either continuous hours (running) or sets of 24 hours (daily): ')
  assign('hset', h_set, envir = .GlobalEnv)
  
}


Anticipation_objects <- function()
{
  # warning about overwriting old objects
  invisible(readline(prompt=" WARNING: this function will write values for R objects to the global environment,
                     overwriting anything with the same name.
                     Press [enter] to continue
                     Press [esc] to exit"))
  
  # initial date of experiment
  initial_date <- readline('Enter value of idate in yyyy-mm-dd format: ')
  assign('idate', initial_date, envir = .GlobalEnv)
  
  # start time of total experiment 
  initial_time <- readline('Enter value of itime (start time of experiment) in military time: ')
  initial_time <- as.numeric(initial_time)
  assign('itime', initial_time, envir = .GlobalEnv)
  
  # entrainment start time
  entrain_time <- readline('Enter value of stime (beginning of entrainment schedule) in military time: ')
  entrain_time <- as.numeric(entrain_time)
  assign('etimeS', entrain_time, envir = .GlobalEnv)
  
  # entrainment end time
  entrain_timeE <- readline('Enter value of stime (beginning of entrainment schedule) in military time: ')
  entrain_timeE <- as.numeric(entrain_timeE)
  assign('etimeE', entrain_timeE, envir = .GlobalEnv)
  
  #  day of data desired for analysis
  start_day <- readline('Enter value of start day for data analysis: ')
  start_day <- as.numeric(start_day)
  assign('pday', start_day, envir = .GlobalEnv)
  
  # last day of data desired for analysis
  end_day <- readline('Enter value of last day for data analysis: ')
  end_day <- as.numeric(end_day)
  assign('fday', end_day, envir = .GlobalEnv)
  
}

# function to loop analysis functions over a set of data

genotype_apply <- function(genotype, )