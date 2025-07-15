# get meta data for what to scrape...
# 
# i want to scrape Keller Williams realty offices for contact info for each office;
# final site with contacts look like so, https://kwaustin.kw.com/our-leaders
# the "kwaustin" changes based on the location... this can be obtained by going
# to each location and navigating to the “Visit Website” button (has a href)
#
# https://locations.kw.com/ has a map of offices, each office has a # like so;
# https://locations.kw.com/location/1, but not all numbers are used so first we
# need to get all the numbers will will need (office locations)

library(httr)
library(purrr)
library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))
app_path <<- getwd()

# read in function to get url from location
function_path <- paste0(getwd(),"/getUrls_scrape.R")
source(function_path)

# creates csv of valid sites (status 200)
check.validURLS_toCSV(id_start = 1, id_stop = 3000)
check.validURLS_toCSV(id_start = 3000, id_stop = 6000)

# Read in valid sites to scrape
file_name <- paste0(getwd(),"/valid_locations_kwa_1_3000.csv")
web_siteinfo <- read.csv(file_name)

# function_path <- paste0(getwd(),"/html_js_scrape.R")
# source(function_path)
# this bumps download.file, curl, httr, etc. to 60 seconds
options(timeout = 80)



site=1
while (site <= 100) {
  scrape_leaders(web_siteinfo$url[site])
  cat(site," complete... \n")
  Sys.sleep(5)
  site=site+1
}

# clean up files
list.files(paste0(getwd(),"/kw_data/"))
