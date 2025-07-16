
library(httr)
library(purrr)
library(dplyr)

# Function to check one location ID
check_location <- function(id, base_url = "https://locations.kw.com/location/") {
  url <- paste0(base_url, id)
  
  # wrap in tryCatch to avoid hard errors
  res <- tryCatch(
    HEAD(
      url,
      user_agent("Chrome/5.0 (compatible; R script)"),
      timeout(15)            # 15-second timeout
    ),
    error = function(e) e   # return the error object if something goes wrong
  )
  
  # if we got an error, status = NA, else extract status_code()
  status <- if (inherits(res, "error")) NA_integer_ else status_code(res)
  
  # return a one-row tibble
  tibble(
    id     = id,
    url    = url,
    status = status
  )
}

check.validURLS_toCSV <- function(id_start, id_stop) {
  # Sweep over a range of IDs
  location_ids <- id_start:id_stop
  
  results <- map_dfr(location_ids, check_location)
  
  # Filter only the “200 OK” ones
  valid_locations <- results %>%
    filter(status == 200)
  
  write.csv(valid_locations, paste0(getwd(),"/testingAndBackups/valid_locations_kwa_",id_start,"_",id_stop,".csv"))
  return(valid_locations)
}

#check.validURLS_toCSV(1,100)




