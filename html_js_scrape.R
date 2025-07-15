

## -----------------------------------------------------------------------------

library(chromote)
library(rvest)
library(tibble)
library(stringr)
library(httr)
library(purrr)
library(dplyr)

scrape_leaders <- function(loc_page_url,
                           hero_sel    = ".kw-locationHero-content__grid-row1 a",
                           avatar_sel  = ".AvatarCard-bio-full-name",
                           timeout     = 30,
                           poll_delay  = 2) {
  # # testing
  # loc_page_url <- "https://locations.kw.com/location/10"
  
  b <- ChromoteSession$new()
  on.exit(b$close(), add = TRUE)
  
  # Enable Chrome DevTools domains
  b$Page$enable()
  b$Runtime$enable()
  b$DOM$enable()
  
  # 1) Navigate to the location page & wait for the hero link
  b$Page$navigate(loc_page_url)
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
  email <- numbers[grep("mailto:",numbers)]

  # get rid of extra contacts
  num_wanted <- length(phones)-1
  
  # final clean up
  phones <- phones %>% 
    sub("^tel:", "", .) %>%    # "tel:512.699.3425"
    gsub("\\D", "", .)         # "5126993425"
  
  #b$close()
  
  # 7) Return as tibble
  data <- tibble(
    `First Name`      = full_names[1:num_wanted],
    `Last Name`       = last_name[1:num_wanted],
    `Email`           = emails[1:num_wanted],
    `Phone Number`    = phones[1:num_wanted],
    `Company Name`    = sub("^https?://([^\\.]+)\\.kw\\.com.*$", "\\1", base_site),
    `Lead Status`     = "New",
    `Lifecycle Stage` = "Lead",
    `Buying Role`     = "",
    `Contact owner`   = "Andrew Gilbert",
    `Job title`       = titles[1:num_wanted]
    #ProfileLink       = links,
    #Source            = leaders_url,
  )
  t <- str_trim(substr(Sys.time(),1,19))
  code <- gsub(":","_",t)
  code <- gsub(" ","_",code)
  
  office <- sub("^https?://([^\\.]+)\\.kw\\.com.*$", "\\1", base_site)
  write.csv(data, paste0("kw_data/contacts_",office,"_",code,".csv"))
}
