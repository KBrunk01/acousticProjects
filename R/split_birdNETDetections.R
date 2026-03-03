
## Heimdalen PAM 2025 data - full birdNET run in Feb 2026
## Splits all birdNET detections from the HEIM_PAM 2025 data into 
## separate CSVs for each species

library(tidyverse)

bn <- read_tsv("C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/BirdNET/HEIM_PAM_2025_birdNET_Feb2026/BirdNET_SelectionTable.txt")

# split by species
species_list <- bn %>%
  group_by(`Common Name`) %>%
  group_split()

# create names
sp_names <- bn %>%
  group_keys(`Common Name`) %>%
  pull(`Common Name`)

# Assign the names to the list
full_sp_list <- species_list %>%
  set_names(sp_names)

names(full_sp_list)

# Write separate CSV for each species
path <- "C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/BirdNET/HEIM_PAM_2025_birdNET_Feb2026/speciesSelections/"

for (name in names(full_sp_list)) {
  file_path <- paste0(path, name, ".csv") # Creates "df1.csv", "df2.csv", etc.
  write.csv(full_sp_list[[name]], file = file_path, row.names = FALSE) #
}
