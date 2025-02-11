
# Code to randomly select 30% of Heimdalen 2024 
# acoustic files while stratifying across Sites and Months

# set wd to external hard drive with files of interest
setwd("F:/ARU Data/Heimdalen_ARUDeployments/2024/HEIM_PAM/FLACfiles/")

fileList <- list.files(pattern = "*.flac", recursive = TRUE)

head(fileList)
Audiofiles <- substr(fileList, 62, 106)
head(Audiofiles)

# create dataframe with site, date, and time for each recording
audioDF <- data.frame(filePath = fileList,
                      fileName = substr(fileList, 62, 106),
                      Site = substr(Audiofiles, 13,18),
                      Year = substr(Audiofiles, 25,28),
                      Month = substr(Audiofiles,29,30),
                      Day = substr(Audiofiles,31,32),
                      Hour = substr(Audiofiles,34,35) )
audioDF$Date <- as.Date(with(audioDF,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

# select stratified random sample of files
# stratify across site and month
library(tidyverse)

set.seed(11)

sampled_data_n <- audioDF %>%
  group_by(Site,Month) %>%
  slice_sample(prop = 0.31)

# View the sampled data
sampled_data_n %>% count(Site)

# shuffle data
HEIM_forReview <- sampled_data_n[sample(nrow(sampled_data_n)),]
HEIM_forReview$fileOrder <- row.names(HEIM_forReview)

current_folder <- "F:/ARU Data/Heimdalen_ARUDeployments/2024/HEIM_PAM/FLACfiles/"
new_folder <- "D:/audioFiles/"

# testFile <- sampled_data_n[1:5,]$filePath
filesToCopy <- sampled_data_n$filePath

file.copy(file.path(paste0(current_folder,filesToCopy)), new_folder)

write.csv(HEIM_forReview[,c(1,9,2:8)], paste0(new_folder,"HEIM_fileReviewProgress.csv"))
