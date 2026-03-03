
# Bluethroat segments list for validation
setwd("C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/BirdNET/HEIM_PAM_2025_birdNET_Feb2026/Segments/Bluethroat")

# List all bluethroat segments
fileList <- list.files(pattern = "*.wav", recursive = TRUE)

bluethroat <- data.frame(Species = "Bluethroat",
                         Site = substr(fileList, 19,24),
                         fileName = fileList,
                         confScore = as.numeric(substr(fileList, 1, 5)),
                         validation = NA,
                         notes = NA
                         )

write.csv(bluethroat, "bluethroatValidation.csv")


# Common Cuckoo
setwd("C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/BirdNET/HEIM_PAM_2025_birdNET_Feb2026/Segments/Common Cuckoo")

# List all bluethroat segments
fileList <- list.files(pattern = "*.wav", recursive = TRUE)

cuckoo <- data.frame(Species = "Common Cuckoo",
                         Site = substr(fileList, 19,24),
                         fileName = fileList,
                         confScore = substr(fileList, 1, 5),
                         validation = NA,
                         notes = NA
)

write.csv(cuckoo, "commonCuckooValidation.csv")


# Meadow Pipit
setwd("C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/BirdNET/HEIM_PAM_2025_birdNET_Feb2026/Segments/Meadow Pipit")

# List all bluethroat segments
fileList <- list.files(pattern = "*.wav", recursive = TRUE)

pipit <- data.frame(Species = "Meadow Pipit",
                     Site = substr(fileList, 19,24),
                     fileName = fileList,
                     confScore = substr(fileList, 1, 5),
                     validation = NA,
                     notes = NA
)

write.csv(pipit, "meadowPipitValidation.csv")
