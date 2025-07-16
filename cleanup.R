# clean up processed data

library(readr)
library(dplyr)

# back up data 1st...
# then clean

# All CSV file paths (recursive)
files <- list.files(
  path       = "kw_data",
  pattern    = "\\.csv$",
  recursive  = TRUE,
  full.names = TRUE
)

for (f in files) {
  df <- read_csv(f)
  # Drop the very first column
  df_clean <- df[, -1, drop = FALSE]
  # Append row for location data
  branch <- basename(dirname(f))
  df_clean$`Company Name` <- paste0(df_clean$`Company Name`,"-",branch)
  # Overwrite the original file (no row.names)
  write.csv(df_clean, f, row.names = FALSE)
}


# 0) Load required packages
library(tidyverse)   # for readr, dplyr, purrr, stringr
library(fs)          # for path manipulation

# 1) List all CSVs recursively
all_files <- dir_ls("kw_data", recurse = TRUE, regexp = "\\.csv$")

# 2) Extract state code: it's the subfolder under kw_data/
#    e.g. "kw_data/WA/Seattle_WA/..." → "WA"
state_codes <- all_files %>%
  path_rel(start = "kw_data") %>%   # drop "kw_data/" prefix
  path_split() %>%                  # split into path components
  map_chr(~ .x[1])                  # first element is the state

# 3) Create a tibble of file paths + state
files_df <- tibble(
  file  = all_files,
  state = state_codes
)

# 4) Ensure output directory exists
out_base <- "kw_data/merged"
dir_create(out_base)

# 5) For each state, read & bind its files, then write
files_df %>%
  group_by(state) %>%
  group_walk(~ {
    state_name <- .y$state
    paths      <- .x$file
    
    # Read and bind all CSVs for this state
    combined <- map_dfr(paths, read_csv, col_types = cols())
    
    # Write merged CSV
    out_path <- path(out_base, paste0(state_name, "_all_contacts.csv"))
    write_csv(combined, out_path)
    
    cat("Merged", length(paths), "files for state", state_name,
        "→", out_path, "\n")
  })
