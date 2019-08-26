## updated 7/26/2019
# Sets up FLIC data analysis
menu(c("Sable", "v2.1"),graphics = FALSE, title = "Monitor type:") -> monitor ##SABLE's and v2's are handled differently
menu(c("6", "12"),graphics = FALSE, title = "Number of wells (typically 12):") -> menu

##v2 SETUP:
if (monitor == 2L){
  monitor <- "v2"
  if (menu == 2L) {p <- ParametersClass.SingleWell()} else {p <- ParametersClass.TwoWell()}
  
  
  menu(c("Default", "Manual"),graphics = FALSE, title = "Setup:") -> menu
  
  if (menu == 1L){
  p <- SetParameter(p,Signal.Threshold = 20) # sets the baseline signal that everything of interest must be above
  p <- SetParameter(p,Feeding.Threshold.Value = 85) # sets the minimum mV for a peak to be confirm feeding
  p <- SetParameter(p,Feeding.Interval.Minimum = 40) # minimum mV for event to be a possible feeding
  p <- SetParameter(p,Tasting.Threshold.Interval = c(10,40)) # sets low and high range of time for a mV peak to be a tasting event (independent of feeding)
  
  p <- SetParameter(p,Use.Adaptive.Threshold = TRUE) # turns the adaptive threshold on, FALSE = off
  threshold <- "at"
  } else {
    readline(prompt="Signal Threshold (Default 20): ") -> a
    a <- strtoi(a, base = 0L)
    p <- SetParameter(p,Signal.Threshold = a) # sets the baseline signal that everything of interest must be above
    readline(prompt="Feeding Threshold Value (Default 85): ") -> a
    a <- strtoi(a, base = 0L)
    p <- SetParameter(p,Feeding.Threshold.Value = a) # sets the minimum mV for a peak to be confirm feeding
    readline(prompt="Feeding Interval Minimum (Default 40): ") -> a
    a <- strtoi(a, base = 0L)
    p <- SetParameter(p,Feeding.Interval.Minimum = a) # minimum mV for event to be a possible feeding
    readline(prompt="Tasting Threshold Interval Min (Default 10): ") -> a
    a <- strtoi(a, base = 0L)
    readline(prompt="Tasting Threshold Interval Max (Default 40): ") -> b
    b <- strtoi(b, base = 0L)
    p <- SetParameter(p,Tasting.Threshold.Interval = c(a,b)) # sets low and high range of time for a mV peak to be a tasting event (independent of feeding)
    
    readline(prompt="Use Adaptive Threshold (Default TRUE): ") -> a
    p <- SetParameter(p,Use.Adaptive.Threshold = TRUE) # turns the adaptive threshold on, FALSE = off
    # Sets a marker for analysis threashold type
    if (a == "FALSE") {
      threshold <- "ft"
    } else {
      threshold <- "at"
    }
    
    }
} else { ##Sable Setup
  monitor <- "sable"
  if (menu == 2L) {p10 <- ParametersClass.SingleWell()} else {p10 <- ParametersClass.TwoWell()}
  
  
  menu(c("Default", "Manual"),graphics = FALSE, title = "Setup:") -> menu
  
  if (menu == 1L){
  p10 <- SetParameter(p10,Signal.Threshold = 2) # sets the baseline signal that everything of interest must be above
  p10 <- SetParameter(p10,Feeding.Threshold.Value = 15) # sets the minimum mV for a peak to be confirm feeding
  p10 <- SetParameter(p10,Feeding.Interval.Minimum = 5) # minimum mV for event to be a possible feeding
  p10 <- SetParameter(p10,Tasting.Threshold.Interval = c(2,5)) # sets low and high range of time for a mV peak to be a tasting event (independent of feeding)nt
  p10 <- SetParameter(p10, Feeding.Minevents = 4)
  
  p10 <- SetParameter(p10,Use.Adaptive.Threshold = TRUE) # turns the adaptive threshold on, FALSE = off
  threshold <- "at"
  } else {
    readline(prompt="Signal Threshold (Default 2): ") -> a
    a <- strtoi(a, base = 0L)
    p10 <- SetParameter(p10,Signal.Threshold = a) # sets the baseline signal that everything of interest must be above
 
    readline(prompt="Feeding Threshold Value (Default 15): ") -> a
    a <- strtoi(a, base = 0L)
    p10 <- SetParameter(p10,Feeding.Threshold.Value = a) # sets the minimum mV for a peak to be confirm feeding
    
    readline(prompt="Feeding Interval Minimum (Default 5): ") -> a
    a <- strtoi(a, base = 0L)
    p10 <- SetParameter(p10,Feeding.Interval.Minimum = a) # minimum mV for event to be a possible feeding
    
    readline(prompt="Tasting Threshold Interval Min (Default 2): ") -> a
    a <- strtoi(a, base = 0L)
    readline(prompt="Tasting Threshold Interval Max (Default 5): ") -> b
    b <- strtoi(b, base = 0L)
    p10 <- SetParameter(p10,Tasting.Threshold.Interval = c(a,b)) # sets low and high range of time for a mV peak to be a tasting event (independent of feeding)
    
    readline(prompt="Feeding Minevents (Default 4): ") -> a
    a <- strtoi(a, base = 0L)
    p10 <- SetParameter(p10, Feeding.Minevents = a)
    
    readline(prompt="Use Adaptive Threshold (Default TRUE): ") -> a
    # Sets a marker for analysis threashold type
    if (a == "FALSE") {
      threshold <- "ft"
      } else {
        threshold <- "at"
    }
    p10 <- SetParameter(p10,Use.Adaptive.Threshold = a) # turns the adaptive threshold on, FALSE = off
  }
}
rm(menu, a, b) ##Clears Temporary Variables
menu(c("Yes", "No"),graphics = FALSE, title = "Run DateNameDetector?") -> a
if (a == 1L){
  c(scriptdir, "/DateNameDetector.R") -> a
  str_c(a, collapse = "") -> a
  source(a)
}
