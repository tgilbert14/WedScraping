

## -----------------------------------------------------------------------------

library(chromote)
library(rvest)
library(tibble)
library(stringr)
library(httr)
library(purrr)
library(dplyr)


scrape_leaders <- function(loc_page_url, location_data,
                           hero_sel    = ".kw-locationHero-content__grid-row1 a",
                           avatar_sel  = ".AvatarCard-bio-full-name",
                           timeout     = 30,
                           poll_delay  = 3) {
  # # testing
  # loc_page_url <- "https://locations.kw.com/location/10"
  
  b <- ChromoteSession$new()
  on.exit(b$close(), add = TRUE)
  
  Sys.sleep(0.2)
  
  # Enable Chrome DevTools domains
  b$Page$enable()
  b$Runtime$enable()
  b$DOM$enable()
  
  # 1) Navigate to the location page & wait for the hero link
  b$Page$navigate(loc_page_url)
  
  Sys.sleep(0.2)
  
  start <- Sys.time()
  repeat {
    ready <- b$Runtime$evaluate(
      sprintf("Boolean(document.querySelector('%s'))", hero_sel)
    )$result$value
    if (ready) break
    if (as.numeric(Sys.time() - start, "secs") > timeout) {
      stop("Timed out waiting for hero link on ", loc_page_url)
    }
    Sys.sleep(poll_delay)
  }
  
  Sys.sleep(1)
  
  # 2) Grab the base URL from the hero link
  js_fetch_hero <- sprintf(
    "document.querySelector('%s').href", hero_sel
  )
  base_site <- b$Runtime$evaluate(js_fetch_hero, returnByValue = TRUE)$result$value
  
  # 3) Construct the leaders URL and HEAD‐check it
  leaders_url <- paste0(sub("/$", "", base_site), "/our-leaders")
  
  # 3a) Retry the HEAD request up to 3 times
  head_resp <- RETRY(
    "HEAD", 
    leaders_url,
    timeout(5),       # give each try up to  5s to connect & fetch headers
    pause_base = 1,   # wait 1s before 1st retry
    pause_cap  = 5,   # max 5s between retries
    times      = 3    # try HEAD up to 3 times
  )
  
  # 3b) Check status as before
  if (status_code(head_resp) != 200) {
    return(tibble())  # no leaders page → empty tibble
  }
  
  # if (httr::status_code(httr::HEAD(leaders_url, timeout(5))) != 200) {
  #   return(tibble())  # no leaders page
  # }
  
  Sys.sleep(0.2)
  
  # 4) Navigate to the leaders page & wait for avatars
  b$Page$navigate(leaders_url)
  start <- Sys.time()
  repeat {
    loaded <- b$Runtime$evaluate(
      sprintf("Boolean(document.querySelector('%s'))", avatar_sel)
    )$result$value
    if (loaded) break
    if (as.numeric(Sys.time() - start, "secs") > timeout) {
      stop("Timed out waiting for avatars on ", leaders_url)
    }
    Sys.sleep(poll_delay)
  }
  
  # 5) Pull the fully‐rendered HTML
  html <- b$Runtime$evaluate(
    "document.documentElement.outerHTML", returnByValue = TRUE
  )$result$value
  page <- read_html(html)
  
  # 6) Scrape the fields
  full_names <- page %>% html_nodes(avatar_sel) %>% html_text(trim = TRUE)
  first_name <- word(full_names, 1)
  last_name  <- word(full_names, -1)
  titles     <- page %>% html_nodes(".AvatarCard-bio-title") %>% html_text(trim = TRUE)
  links      <- page %>% html_nodes(".AvatarCard-link")      %>% html_attr("href")
  numbers    <- page %>% html_nodes(".global-text-16-bold")      %>% html_attr("href")
  
  # phone should be 1st (odd)
  odds <- seq(1, length(full_names), by = 2)
  # email should be 2nd (even)
  evens <- seq(2, length(full_names), by = 2)
  
  original_contact_count = length(full_names)
  
  i=1
  while (i <= original_contact_count) {
    # if 1st value is mail, get rid of it... 
    if ( grepl("mailto:", numbers[odds[i]]) ) {
      # means missing phone number - get rid of data
      full_names <- full_names[full_names != full_names[i]]
      first_name <- first_name[first_name != first_name[i]]
      last_name <- last_name[last_name != last_name[i]]
      titles <- titles[full_names != titles[i]]
      links <- links[full_names != links[i]]
      numbers <- numbers[numbers != numbers[odds[i]]]
    }
    i=i+1
  }
  
  phones <- numbers[grep("tel:",numbers)]
  emails <- numbers[grep("mailto:",numbers)]

  # get rid of extra contacts
  num_wanted <- length(phones)-1
  
  # Final clean up -->
  phones <- phones %>% 
    sub("^tel:", "", .) %>%    # "tel:512.699.3425"
    gsub("\\D", "", .)         # "5126993425"
  # Pad 10-digit numbers with leading "1"
  phones_clean <- ifelse(nchar(phones) == 10, paste0("1", phones), phones)
  
  emails_clean <- emails %>% 
    sub("^mailto:", "", .)
  
  # Dealing with strange symbols and characters
  titles_cleaned <- titles %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%               # fix encoding
    str_remove_all("[?!#$%^]") %>%                                  # remove listed symbols
    str_squish()
  
  # # if there are contacts
  # if (length(first_name) > 1) {
  #   
  # }

  # 7) Return as tibble
  data <- tibble(
    `First Name`      = first_name[1:num_wanted],
    `Last Name`       = last_name[1:num_wanted],
    `Email`           = emails_clean[1:num_wanted],
    `Phone Number`    = phones_clean[1:num_wanted],
    `Company Name`    = sub("^https?://([^\\.]+)\\.kw\\.com.*$", "\\1", base_site),
    `Lead Status`     = "New",
    `Lifecycle Stage` = "Lead",
    `Buying Role`     = "",
    `Contact owner`   = "Andrew Gilbert",
    `Job title`       = titles_cleaned[1:num_wanted]
    #ProfileLink       = links,
    #Source            = leaders_url,
  )
  
  # Checking location data for url to place in correct folder dir
  site <- location_data[which(location_data$url == loc_page_url),]
  site_trim <- str_trim(sub("\\d+", "", site$State))
  site_folder <- gsub(", ", "_", site_trim)

  # Save and sort in directory
  office <- sub("^https?://([^\\.]+)\\.kw\\.com.*$", "\\1", base_site)
  office_clean_raw <- office %>% 
    str_remove_all("[?!#$%^]") %>%                                  # remove listed symbols
    str_squish()
  office_clean <- gsub("\\.","_",office_clean_raw)
  # for special case
  office_clean <- sub("https://","",office_clean)
  office_clean <- gsub("/","",office_clean)
  
  state_code <- substr(site_folder,nchar(site_folder)-1,nchar(site_folder))
  
  # Check if both characters are uppercase A-Z for state - make state folder -->
  if (grepl("^[A-Z]{2}$", state_code)) {
    # Create folder if it doesn't exist
    if (!dir.exists(paste0("kw_data/",state_code))) {
      dir.create(paste0("kw_data/",state_code), recursive = TRUE)
    }
    # Then create sub-folder for site
    if (!dir.exists(paste0("kw_data/",state_code,"/",site_folder))) {
      dir.create(paste0("kw_data/",state_code,"/",site_folder), recursive = TRUE)
    }
    write.csv(data, paste0("kw_data/",state_code,"/",site_folder,"/contacts_",office_clean,".csv"))
    # Else no state to put in folder -->
  } else {
    # Create folder if it doesn't exist - no state found - create folder for location
    if (!dir.exists(paste0("kw_data/",site_folder))) {
      dir.create(paste0("kw_data/",site_folder), recursive = TRUE)
    }
    write.csv(data, paste0("kw_data/",site_folder,"/contacts_",office_clean,".csv"))
  }
  
}

# t <- str_trim(substr(Sys.time(),1,19))
# code <- gsub(":","_",t)
# code <- gsub(" ","_",code)
# write.csv(data, paste0("kw_data/contacts_",office,"_",code,".csv"))
