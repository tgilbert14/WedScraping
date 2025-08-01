
# install and load ->
library(gmailr)
library(tidyverse)

# set up google cloud console
#https://console.cloud.google.com/welcome/new?inv=1&invt=Ab4U1Q

# Create a new project/my first project -->
# Enable gmail API under - “APIs & Services” (on left side of screen)
# Go to Credentials and create an OAuth 2.0 Client ID
#  >Choose Desktop App
#  >Download the .json file (this is your credentials file)
#  >Save it somewhere safe, like: "~/gmailr_credentials.json"
# Make sure your GmailAPI is enabled ->
#  >https://console.cloud.google.com/apis/library/gmail.googleapis.com?project=striped-guard-467715-v9&inv=1&invt=Ab4U8A

myEmail <- "shrimplytimbo@gmail.com"
toEmail <- "tsglbert35@hotmail.com"

# authenticate -> this will open a browser to log in and authorize access
gm_auth_configure(path = paste0(getwd(),"/developingCode/gmailr_credentials.json"))
#gm_auth(email = myEmail)
gm_auth(email = myEmail, cache = paste0(getwd(),"/developingCode/cache"))

# between R sesseion - 1. Yes to store cache
# Use -> Esc abort
# email account using should be added to the testers list, hit Continue


# need to add an unsubscribe link if mass sending

# if responds, send no more emails...
email <- gm_mime() %>%
  gm_to(toEmail) %>%
  gm_from(myEmail) %>%
  gm_subject("Test!") %>%
  gm_text_body("Hey, intereted in a demo?")

gm_send_message(email)

# if not respond ->
email <- gm_mime() %>%
  gm_to(toEmail) %>%
  gm_from(myEmail) %>%
  gm_subject("BrokerBot Update") %>%
  gm_text_body("something about a new feature or something...")

Sys.sleep(10)

