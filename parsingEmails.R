library(tidyverse)

# get emails for east coat ->
path <- paste0(getwd(),"/kw_data/MERGED/USA")
file_names <- dir(path)
files <- file_names[grep("_contacts.csv", file_names)]

i=1
while (i <= length(file_names)) {
  
  if (i == 1) {
    data <- read_csv(paste0(path,"/",files[i]))
  } else {
    data_2 <- read_csv(paste0(path,"/",files[i]))
    data <- rbind(data, data_2)
  }
  i=i+1
}

View(data)
data$Email

data_filtered <- data %>%
  filter(str_detect(`Job title`, regex("Principal|Owner|CEO|Operations|MC", ignore_case = TRUE))|
  str_starts(Email, "klrw") |
  str_starts(`Job title`, "Market")) %>% 
  arrange(Email)

data_cleaned <- data_filtered %>% 
  select(`First Name`, `Last Name`, Email, `Job title`)

data_final <- unique(data_cleaned) %>% 
  filter(!is.na(Email))

write_csv(data_final, "EastCoastEmailContacts_KWA.csv")

  
  
  