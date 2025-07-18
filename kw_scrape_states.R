

## -----------------------------------------------------------------------------

#library(chromote)
library(rvest)
library(tibble)
library(stringr)
library(httr)
#library(purrr)
library(dplyr)


# Read in valid sites to scrape
file_name <- paste0(getwd(),"/testingAndBackups/valid_locations_kwa_1_3000.csv")
web_siteinfo <- read.csv(file_name)
# To store results
results <- tibble(URL = character(), State = character())

i=1
while (i <= length(web_siteinfo$url)) {
  cat("Scraping:", web_siteinfo$url[i], "\n")
  
  call <- web_siteinfo$url[i]
  
  # Use RETRY to fetch the page with up to 3 attempts
  res <- RETRY(
    "GET",
    call,
    timeout(10),         # up to 10s per attempt
    times = 3,           # try up to 3 times
    pause_base = 2,      # start with 2s between retries
    pause_cap  = 5,      # no more than 5s pause
    terminate_on = c(404)  # skip retries for 404s
  )
  
  if (status_code(res) == 200) {
    # Parse response as HTML
    page <- read_html(res)
    
    # Extract the state value
    state <- page %>% 
      html_nodes(".kw-locationHero-content__grid-padded .svelte-1kg2epi:nth-child(3)") %>% 
      html_text(trim = TRUE)
    
    # Save state and url to dataframe
    state <- if (length(state) > 0) state else NA_character_
    
    cat("→ State:", state, "\n")
    
    # Append to result tibble
    results <- bind_rows(results, tibble(URL = call, State = state))
    
  } else {
    warning("Failed to fetch: ", call, " — status ", status_code(res))
  }
  
  # Move on to the next one
  i = i+1
}

#View(results)
#write.csv(results, "url_locations.csv")

