

## -----------------------------------------------------------------------------

#library(chromote)
library(rvest)
library(tibble)
library(stringr)
#library(httr)
#library(purrr)
library(dplyr)


call <- "https://locations.kw.com/location/10"
page <- read_html(call)
state <- page %>%
  html_nodes(".kw-locationHero-content__grid-padded .svelte-1kg2epi:nth-child(3)") %>%
  html_text(trim = TRUE)


