# updated 7/26/2019

print("Warning! This script takes hours to run and is very tasking on the computer. Press Esc to quit.")
readline(prompt="Start DFM number (if data goes from DFM 11 through 15 enter 11): ")->startdfm
readline(prompt="End DFM number (if data goes from DFM 11 through 15 enter 15): ")->enddfm
as.numeric(startdfm)->startdfm
as.numeric(enddfm)->enddfm


if ((enddfm - startdfm) > 5){
  print("Warning! you are about to attempt to read in >5 DFM's. This program will automatically")
  print("break up the data into groups of 5 DFMs per save file in the working directory to prevent crashes.")
  menu(c("Yes", "No"), graphics = FALSE, title = "Would you like to proceed?") -> menu
  if (menu == 2L){
    c(scriptdir, "/ReadinUI.R") -> a
    str_c(a, collapse = "") -> a
    source(a)
  }
}
(enddfm - startdfm) %/% 5 -> runnumber


readline(prompt="Bin Length in minutes (typically 30):") -> binlength
binlength <- strtoi(binlength, base = 0L)

##setting up a monitor identifier if one doesn't exsist
if (!exists("monitor")) {0 -> monitor} 
if (any(monitor != "sable") && any(monitor != "v2")) {
  menu(c("Sable", "v2.1"),graphics = FALSE, title = "Monitor type:") -> monitor
  if ((monitor == 1L)){
    monitor <- "sable"
  } else {
    monitor <- "v2"
  }
}
c(scriptdir, "/ReadinData.R") -> a
str_c(a, collapse = "") -> a
source(a)
