################################################################################
#############          NES - LTER Cruise           #############################
#############             JUN-2024                 #############################
#############         STATION METADATA             #############################
## by: Alexandra Cabanelas 
################################################################################
# this script retrieves station metadata for all cruises from RESTAPI 


## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(tidyverse)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

file_urls <- c(
  'https://nes-lter-data.whoi.edu/api/stations/en608.csv',
  'https://nes-lter-data.whoi.edu/api/stations/en617.csv',
  'https://nes-lter-data.whoi.edu/api/stations/en627.csv',
  'https://nes-lter-data.whoi.edu/api/stations/en644.csv',
  'https://nes-lter-data.whoi.edu/api/stations/en649.csv',
  'https://nes-lter-data.whoi.edu/api/stations/en655.csv',
  'https://nes-lter-data.whoi.edu/api/stations/en657.csv',
  'https://nes-lter-data.whoi.edu/api/stations/en661.csv',
  'https://nes-lter-data.whoi.edu/api/stations/en668.csv',
  'https://nes-lter-data.whoi.edu/api/stations/at46.csv',
  'https://nes-lter-data.whoi.edu/api/stations/en687.csv',
  'https://nes-lter-data.whoi.edu/api/stations/en695.csv',
  'https://nes-lter-data.whoi.edu/api/stations/hrs2303.csv',
  'https://nes-lter-data.whoi.edu/api/stations/en706.csv',
  #'https://nes-lter-data.whoi.edu/api/stations/en712.csv', # missing
  #'https://nes-lter-data.whoi.edu/api/stations/en715.csv', # missing 
  'https://nes-lter-data.whoi.edu/api/stations/ar77.csv',
  'https://nes-lter-data.whoi.edu/api/stations/ar32.csv',
  'https://nes-lter-data.whoi.edu/api/stations/ar38.csv',
  'https://nes-lter-data.whoi.edu/api/stations/ar61b.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/stations/ar28b.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/stations/ar31a.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/stations/ar34b.csv', #ring net only
  'https://nes-lter-data.whoi.edu/api/stations/ar39b.csv' #ring net only
  #'https://nes-lter-data.whoi.edu/api/stations/AR63.csv'
)

# OOI? no bongo, no ring :
# AR34A, AR22, AR24a, AR24c, AR28A, AR31C, AR39A, AR44, AR48A, AR48B


read_and_add_cruise <- function(url) {
  cruise_name <- toupper(gsub(".csv", "", basename(url)))
  data <- read.csv(url)
  data$cruise <- cruise_name
  return(data)
}

cruiseDat <- lapply(file_urls, read_and_add_cruise)

combined_data <- do.call(rbind, cruiseDat)
