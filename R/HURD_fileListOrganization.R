
# Code to randomly select 30% of Heimdalen 2024 
# acoustic files while stratifying across Sites and Months

# set wd to external hard drive with files of interest
setwd("E:/Hurdal_ARUDeployments/2024/HURD_PAM/FLACfiles/")

fileList <- list.files(pattern = "*.flac", recursive = TRUE)

head(fileList)
Audiofiles <- substr(fileList, 66, 111)
head(Audiofiles)

# create dataframe with site, date, and time for each recording
audioDF <- data.frame(filePath = fileList,
                      fileName = substr(fileList, 66, 111),
                      Site = substr(Audiofiles, 13,16),
                      Year = substr(Audiofiles, 27,30),
                      Month = substr(Audiofiles,31,32),
                      Day = substr(Audiofiles,33,34),
                      Hour = substr(Audiofiles,36,37) )
audioDF$Date <- as.Date(with(audioDF,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

# select stratified random sample of files
# stratify across site and month
library(tidyverse)

set.seed(11)

sampled_data_n <- audioDF %>%
  group_by(Site,Month) %>%
  slice_sample(prop = 0.15)

# View the sampled data
sampled_data_n %>% count(Site)

# shuffle data
HURD_forReview <- sampled_data_n[sample(nrow(sampled_data_n)),]
HURD_forReview$fileOrder <- row.names(HURD_forReview)

current_folder <- "E:/Hurdal_ARUDeployments/2024/HURD_PAM/FLACfiles/"
new_folder <- "D:/audioFiles/"

# testFile <- sampled_data_n[1:5,]$filePath
filesToCopy <- sampled_data_n$filePath

file.copy(file.path(paste0(current_folder,filesToCopy)), new_folder)

write.csv(HURD_forReview[,c(1,9,2:8)], paste0(new_folder,"HURD_fileReviewProgress.csv"))
