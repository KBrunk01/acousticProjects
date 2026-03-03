
# Code to randomly select 10% of Heimdalen 2025
# acoustic files while stratifying across Sites and Months

# set wd to external hard drive with files of interest
setwd("E:/HEIM_PAM/FLAC/HEIM_FLAC/")

fileList <- list.files(pattern = "*.flac", recursive = TRUE)

head(fileList)
Audiofiles <- substr(fileList, 44, 85)
head(Audiofiles)

# create dataframe with site, date, and time for each recording
audioDF <- data.frame(filePath = fileList,
                      fileName = substr(fileList, 44, 85),
                      Site = substr(Audiofiles, 11,16),
                      Year = substr(Audiofiles, 23,26),
                      Month = substr(Audiofiles,27,28),
                      Day = substr(Audiofiles,29,30),
                      Hour = substr(Audiofiles,32,33) )
audioDF$Date <- as.Date(with(audioDF,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

# select stratified random sample of files
# stratify across site and month
library(tidyverse)

set.seed(9)

sampled_data_n <- audioDF %>%
  group_by(Site, Month, Hour) %>%
  slice_sample(prop = 0.05)

# View the sampled data
sampled_data_n %>% count(Site)
sampled_data_n %>% count(Hour)

ggplot(data = sampled_data_n, aes(x = as.numeric(Hour))) +
  geom_histogram(binwidth = 1, color="white", fill = "turquoise", alpha=0.9) +
  labs(x = "Hour of the Day", y = "Count")

ggplot(data = sampled_data_n, aes(x = as.numeric(Month))) +
  geom_histogram(binwidth = 1, color="white", fill = "forestgreen", alpha=0.9) +
  labs(x = "Month", y = "Count")




# shuffle data
HEIM_forReview <- sampled_data_n[sample(nrow(sampled_data_n)), ]
HEIM_forReview$fileOrder <- row.names(HEIM_forReview)
HEIM_forReview$fileOrder <- as.numeric(HEIM_forReview$fileOrder)

current_folder <- "E:/HEIM_PAM/FLAC/HEIM_FLAC/"
new_folder <- "F:/audioFiles/"

# testFile <- sampled_data_n[1:5,]$filePath
filesToCopy <- sampled_data_n$filePath

file.copy(file.path(paste0(current_folder,filesToCopy)), new_folder)

write.csv(HEIM_forReview[,c(1,9,2:8)], paste0(new_folder,"HEIM2025_fileReviewProgress_Rubi.csv"))
