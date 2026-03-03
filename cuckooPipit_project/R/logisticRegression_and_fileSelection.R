
# Code for running the logistic regression to threshold cuckoo and pipit 
# detections from birdNET

### import validation data 
setwd("cuckooPipit_project/") # set the working directory to wherever you've saved the data

# Cuckoo (a little silly because they are all true positives)
cuckoo <- read.csv("Data/cuckoo_validation_Feb2026.csv")
# it kept a bunch of empty rows, filter those out
cuckoo <- cuckoo[complete.cases(cuckoo$confScore),]

# Same for pipit
pipit <- read.csv("Data/mpipit_validation_Feb2026.csv")
# it kept a bunch of empty rows, filter those out
pipit <- pipit[complete.cases(pipit$confScore),]


### logistic regressions
cuckoo.glm <- glm(validation ~ confScore, family = "binomial", data = cuckoo)
pipit.glm <- glm(validation ~ confScore, family = "binomial", data = pipit)

summary(cuckoo.glm) # looks funky bc all are TP
summary(pipit.glm)

# values we will predict across
prediction_values <- seq(0, 1, 0.01)

# predictions from the logistic regression for both species
cuckoo.pred <- predict(cuckoo.glm, list(confScore = prediction_values), type = "r")
pipit.pred <-  predict(pipit.glm, list(confScore = prediction_values), type = "r")
                
# Plot observations (as points) and the predicted regression relationship (a curve)
p90 <- 0.90
p95 <- 0.95
p99 <- 0.99

pipit99 <- (log(p99/(1-p99)) - pipit.glm$coefficients[1]) / pipit.glm$coefficients[2]
pipit95 <- (log(p95/(1-p95)) - pipit.glm$coefficients[1]) / pipit.glm$coefficients[2]
pipit90 <- (log(p90/(1-p90)) - pipit.glm$coefficients[1]) / pipit.glm$coefficients[2]

cuckoo99 <- (log(p99/(1-p99)) - cuckoo.glm$coefficients[1]) / cuckoo.glm$coefficients[2]
cuckoo95 <- (log(p95/(1-p95)) - cuckoo.glm$coefficients[1]) / cuckoo.glm$coefficients[2]
cuckoo90 <- (log(p90/(1-p90)) - cuckoo.glm$coefficients[1]) / cuckoo.glm$coefficients[2]

par(mfrow = c(1, 2))

plot(validation ~ confScore, data=pipit, xlim=c(0,1), ylim=c(0,1), pch=16, col=rgb(0,0,0,.3),
     xlab = "birdNET confidence score", ylab = "Pr(true positive", main = "Meadow pipit",
     cex.axis=1.1, cex.lab=1.25, cex.main=1.25) # observations
lines(pipit.pred ~ prediction_values, lwd=4, col=rgb(1,0,1,.5)) # plotting the logistic regression
abline(v = pipit99, lty = "dashed", lwd = 2, col = rgb(0,0.8,1,.75)) # 99% threshold

plot(validation ~ confScore, data=cuckoo, xlim=c(0,1), ylim=c(0,1), pch=16, col=rgb(0,0,0,.3),
     xlab = "birdNET confidence score", ylab = "Pr(true positive", main = "Common cuckoo",
     cex.axis=1.1, cex.lab=1.25, cex.main=1.25) # observations
lines(cuckoo.pred ~ prediction_values, lwd=4, col=rgb(1,0,1,.5)) # plotting the logistic regression
# abline(v = cuckoo99, lty = "dashed", lwd = 2, col = rgb(0,1,1,.5)) # 99% threshold

# we can just randomly select from all cuckoo detections and 
# pipit detections above 95% confidence, which is birdNET 
# confidence score >= 0.375.



# --------------------
# Check out the distribution of confidence scores for both species
cc.det <- read.csv("C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/BirdNET/HEIM_PAM_2025_birdNET_Feb2026/speciesSelections/Common Cuckoo.csv")
mp.det <- read.csv("C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/BirdNET/HEIM_PAM_2025_birdNET_Feb2026/speciesSelections/Meadow Pipit.csv")


# ----------------
# Often, this thresholding procedure limits the detection radius of the 
# recorder, so let's try to limit the radius similarly for both species

# We know all/most of the cuckoo detections are probably correct, but instead of just
# using them all, it might be nice to cut off the same percentile of detections
# as we did for the meadow pipit. 

# This tells us what % of mp detections have a birdNET confidence score > 0.375
ecdf(mp.det$Confidence)(0.375)
# Then we can calculate a threshold for cuckoo that leaves the same 
# % of detections
cc.thresh <- quantile(cc.det$Confidence, 0.484)

# We can also take a peak at what these thresholds look like
par(mfrow = c(1, 2)) # this tells R we want figures plotted in one row and two columns
# i.e., side-by-side

hist(mp.det$Confidence, xlab = "birdNET confidence score", main = "Meadow Pipit")
abline(v = 0.375, lty = "dashed", lwd = 2, col = rgb(0,0.8,1,.75)) # 95% threshold

hist(cc.det$Confidence, xlab = "birdNET confidence score", main = "Common Cuckoo")
abline(v = cc.thresh, lty = "dashed", lwd = 2, col = rgb(0,0.8,1,.75))


## Subset both species using these thresholds
mp.det.sub <- subset(mp.det, Confidence >= 0.375)
cc.det.sub <- subset(cc.det, Confidence >= cc.thresh)

# Bind the two data frames together row-wise
dets <- rbind(mp.det.sub, cc.det.sub)

# Add a file name column
dets$fileName <- substr(dets$Begin.Path, 87, 128)

# ----------
library(tidyverse)

# Re-organize a bit for better data plotting
combDat <- data.frame(Species = dets$Common.Name,
           Site = substr(dets$Begin.Path, 49,54),   # creates a column called 'Site' composed of characters 49-54 of the filepath (the chunk that includes the site ID)
           Year = substr(dets$Begin.Path, 78,81),   # same for a 'Year' column
           Month = substr(dets$Begin.Path, 82,83),
           Day = substr(dets$Begin.Path, 84,85),
           fileName = dets$fileName,
           confScore = dets$Confidence,
           startTime = dets$Begin.Time..s.,
           endTime = dets$End.Time..s.,
           fileOffset = dets$File.Offset..s.
)

# Format a date column as a date (R handles dates a bit weirdly)
combDat$Date <- as.Date(with(combDat,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")


# --------------------
# Summarizing data in a few ways for plotting

# First, let's look at the seasonal variation in # of detections 
# for each site and species
# summarize by species, date, and site
dayCount <- combDat %>%
  group_by(Species, Date, Site) %>%
  summarize(count = n())

# create the plot
ggplot(dayCount, aes(x = Date, y = count, group = Species))+
  geom_bar(aes(fill = Species), stat='identity', position = position_dodge())+
  ylab("Detection Count") +
  facet_wrap(~Site) + 
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(color = "black"),
        panel.grid.major.y = element_line(colour = "gray70", linewidth = 0.25),
        legend.position="none"
  )

# Now, let's look overall across all sites at the seasonal patterns
# Summarize by species and date
totalCount <- combDat %>%
  group_by(Species, Date) %>%
  summarize(count = n())

# create the plot
ggplot(totalCount, aes(x = Date, y = count, group = Species))+
  geom_bar(aes(fill = Species), stat='identity', position = position_dodge())+
  ylab("Detection Count") +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(color = "black"),
        panel.grid.major.y = element_line(colour = "gray70", linewidth = 0.25),
        legend.position="none"
  )

# Now, at the # of detections for each species at each site in total
# Summarize by species and site
siteCount <- combDat %>%
  group_by(Species, Site) %>%
  summarize(count = n())

ggplot(siteCount, aes(x = Site, y = count, group = Species))+
  geom_bar(aes(fill = Species), stat='identity', position = position_dodge())+
  ylab("Detection Count") +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(color = "black"),
        panel.grid.major.y = element_line(colour = "gray70", linewidth = 0.25),
        legend.position="top"
  )

# Now, let's peak at the counts of each species in each sound file
fileCount <- combDat %>%
  group_by(Species, fileName) %>%
  summarize(count = n())

# Many sound files have multiple detections, which is not so surprising
# We need to narrow down which files to annotate using the timing of the first
# detection in each file

# ---------------
# Keep files where the first detection of either species is after 
# five minutes (300 seconds) and before 25 minutes (1500 seconds)

# First, we need to see how many unique sound files we have
length(unique(combDat$fileName)) # counts unique 'fileName' entries

# Let's look at the timing of the first detection in each file
firstDets <- combDat %>%
  group_by(fileName) %>%
  summarize(count = n(),                # counts the number of detections
            firstDet = min(fileOffset)) # tells us when the first detection is in the file by keeping the minimum value of the offset column

# Now, we can filter to files with the first detection in our desired time window
keeps <- subset(firstDets, firstDet > 300 & firstDet < 1500)
# That leaves us with 2418 unique files

# Now, we can subset our combined data for just those files present in our 'keeps' list
combDat.filt <- filter(combDat, fileName %in% keeps$fileName)

# split the species up again
mpip <- subset(combDat.filt, Species == "Meadow Pipit")
ccuc <- subset(combDat.filt, Species == "Common Cuckoo")

# get lists of unique files for each species
mpip.files <- data.frame(fileName = unique(mpip$fileName),
                         Species = "Meadow Pipit")

ccuc.files <- (data.frame(filename = unique(ccuc$fileName),
                          Species = "Common Cuckoo"))


# randomly select a large number of files for each species to actually annotate
set.seed(11) # so we get the same results

mp.annotate <- mpip.files[sample(nrow(mpip.files), 500), ]
cc.annotate <- ccuc.files[sample(nrow(ccuc.files), 500), ]

# Now, we can pull these files, create a spreadsheet, 
# create an annotation protocol, and get to annotating!

















