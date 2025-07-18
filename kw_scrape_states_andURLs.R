

## -----------------------------------------------------------------------------

library(chromote)
library(rvest)
library(tibble)
library(stringr)
library(httr)
library(purrr)
library(dplyr)
library(readr)

# Read in valid sites to scrape
file_name <- paste0(getwd(),"/testingAndBackups/valid_locations_kwa_1_3000.csv")
web_siteinfo <- read.csv(file_name)
# To store results
results <- tibble(URL = character(), State = character())

i=1
while (i <= length(web_siteinfo$url)) {
  cat("Scraping:", web_siteinfo$url[i], "\n")
  cat("→ Working on: ", i," of ",length(web_siteinfo$url)," (",round((i/length(web_siteinfo$url))*100,0),"%) \n",sep = "")

  call <- web_siteinfo$url[i]
  
  # Use RETRY to fetch the page with up to 3 attempts
  res <- RETRY(
    "GET",
    call,
    timeout(10),         # up to 10s per attempt
    times = 3,           # try up to 3 times
    pause_base = 3,      # start with 3s between retries
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
    
    website_check <- page %>% 
      html_nodes(".kw-locationHero-content__grid-row1 a") %>% 
      html_text(trim = TRUE)
    
    if (length(website_check) == 0) {
      # if cant find website button
      cat("X State:", state, " - website not found...\n")
    }
    
    if (length(website_check) > 0) {
      # Extract the url value -->
      b <- ChromoteSession$new()
      on.exit(b$close(), add = TRUE)
      
      Sys.sleep(0.2)
      
      # Enable Chrome DevTools domains
      b$Page$enable()
      b$Runtime$enable()
      b$DOM$enable()
      
      # 1) Navigate to the location page & wait for the hero link
      b$Page$navigate(call)
      
      hero_sel = ".kw-locationHero-content__grid-row1 a"
      timeout     = 60
      poll_delay  = 2
      
      Sys.sleep(0.2)
      
      start <- Sys.time()
      repeat {
        ready <- b$Runtime$evaluate(
          sprintf("Boolean(document.querySelector('%s'))", hero_sel)
        )$result$value
        if (ready) break
        if (as.numeric(Sys.time() - start, "secs") > timeout) {
          
          stop("Timed out waiting for link on ", call)
        }
        Sys.sleep(poll_delay)
      }
      
      Sys.sleep(1)
      
      # 2) Grab the base URL from the hero link
      js_fetch_hero <- sprintf(
        "document.querySelector('%s').href", hero_sel
      )
      base_site <- b$Runtime$evaluate(js_fetch_hero, returnByValue = TRUE)$result$value
      
      # Save state and url to dataframe
      #state <- if (length(state) > 0) state else NA_character_
      
      cat("→ State:", state, "\n")
      
      # Append to result tibble
      results <- bind_rows(results, tibble(URL = base_site, State = state))
      
    } else {
      warning("Failed to fetch: ", call, " — status ", status_code(res))
    }
    
    # Move on to the next one
    i = i+1
    }
    
}

View(results)
write_csv(results, "url_locations_Update.csv")
#write_csv(results, "url_locations_Update.csv")

# clean up processed data
# 0) load everything we’ll need
library(fs)
library(writexl)
# —————————————————————————————
# 1) extract two-letter codes from your scraped “State” strings
#    (these look like “City, TX 12345”)
locs <- results$State

# grab the part after the comma, then strip off the ZIP
stzip <- str_split(locs, ",\\s*", simplify = TRUE)[,2]
abrState <- str_remove(stzip, "\\s+.*")

state_lookup <- tibble(
  State    = locs,
  abrState = abrState
)

updated_links <- results %>%
  left_join(state_lookup, by = "State", relationship = "many-to-many") %>% 
  arrange(abrState, State, URL)

updated_links$abrState[updated_links$State == "Cincinnati,, OH 45236"] <- "OH"

updated_links <- updated_links %>% 
  arrange(abrState, State, URL)

#View(updated_links)

updated_links$URL <- paste0(updated_links$URL,"our-leaders")

locations <- unique(updated_links$abrState)

x=1
while (x <= length(locations)) {
  
  out_base <- paste0("kw_data_updated/")
  
  # # if does not exist, create
  # if(!dir.exists(out_base)) {
  #   dir_create(out_base)
  # }
  
  by_state <- updated_links %>% 
    filter(abrState == locations[x])
  
  #path <- paste0(getwd(),"/",out_base,"/contacts_",locations[x],".csv")
  path <- paste0(getwd(),"/",out_base,"/contacts_",locations[x],".xlsx")
  
  #write_csv(by_state, path)
  write_xlsx(by_state, path)

  cat("Files for state", locations[x], "→", out_base, "\n")
  x=x+1
}





