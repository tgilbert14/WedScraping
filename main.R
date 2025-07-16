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
#check.validURLS_toCSV(id_start = 1, id_stop = 3000)
#check.validURLS_toCSV(id_start = 3000, id_stop = 3100)

# # Read in valid sites to scrape
# file_name <- paste0(getwd(),"/testingAndBackups/valid_locations_kwa_1_3000.csv")
# web_siteinfo <- read.csv(file_name)
# 
# # Read in valid sites to scrape
# file_name_2 <- paste0(getwd(),"/testingAndBackups/url_locations.csv")
# state_info <- read.csv(file_name_2)
# names(state_info)[names(state_info) == "URL"] <- "url"

# # megre and clean up data
# location_data <- left_join(web_siteinfo, state_info)
# location_data <- location_data %>%
#   select(!c("X","id"))

# write.csv(location_data, "locationData.csv")

# function_path <- paste0(getwd(),"/html_js_scrape.R")
# source(function_path)
# this bumps download.file, curl, httr, etc. to 60 seconds
#options(timeout = 80)

# read in location urls and clean up
file_name_3 <- paste0(getwd(),"/locationData.csv")
kwa_loc <- read.csv(file_name_3)
kwa_loc_clean <- kwa_loc %>% 
  filter(!is.na(url))

# read in function to scrape locations
function_path_2 <- paste0(getwd(),"/html_js_scrape.R")
source(function_path_2)

site=1
#while (site <= 5) {
while (site <= length(kwa_loc_clean$url)) {
  
  scrape_leaders(loc_page_url = kwa_loc_clean$url[site], location_data = kwa_loc_clean)
  cat(sprintf("✓ %d: Scraped %s\n", site, kwa_loc_clean$url[site]))

  Sys.sleep(1)
  site=site+1
}

#list.files(paste0(getwd(),"/kw_data/"))
