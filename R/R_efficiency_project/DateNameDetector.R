## updated 7/26/2019
#Date and experement ID detector
#Calls file name from working directory
getwd() -> directory
a = basename(directory)
#Attempts to remove all common obstructive characters 
gsub('_', '', a) -> a
gsub(' ', '', a) -> a
#Extracts the first number from the pruned name
parse_number(a) -> b
#Checks with the user for confirmation 
print(b)
menu(c("Yes", "No"),graphics = FALSE, title = "Is this the correct date?") -> menu
if (menu == 1L){
  b -> date
} else {readline(prompt="Input experiment start date (yymmdd ex: February 1st 2017 would be 170201): ") -> date} ##Manual override
#Adds long version of date for later processing 
toString(date) -> b
as.Date(b, format = "%y%m%d") -> b
format(b, "%d %b %Y") -> fulldate



#Removes numbers
gsub('[0-9]+', '', a) -> a
#Checks with the user for confirmation
print(a)
menu(c("Yes", "No"),graphics = FALSE, title = "Is this the correct experement id?") -> menu
if (menu == 1L){
  a -> id 
} else {readline(prompt="Experiment identifier (no spaces or special characters. ex: cyc mutant flies would be cycmutant): ") -> id} ##Manual override

substr(a, 1, 3) -> a
print(a)
menu(c("Yes", "No"),graphics = FALSE, title = "Is this the correct experement 3 letter id?") -> menu
if (menu == 1L){
  a -> abbrid 
} else {readline(prompt="3 letter experiment identifier (no spaces or special characters. ex: cyc mutant flies would be cyM): ") -> abbrid} ##Manual override


menu(c("Yes", "No"),graphics = FALSE, title = "Run ReadinData?") -> a
if (a == 1L){
  c(scriptdir, "/ReadinUI.R") -> a
  str_c(a, collapse = "") -> a
  source(a)
}