
# Code to calculate precision, recall, and F-score for species of interest
# Also includes some code to summarize detections and make figures! 

# You will likely need to install these the first time
library(Rraven)
library(warbleR)
library(ohun)
library(tidyverse)
library(viridis)

#####################################

### Importing manual selection tables
# NB! You will need to change this filepath to wherever you have stored your manual selection tables! 
manual.path <- "C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/sampleSoundFiles/manualSelectionTables"

# See which selection tables are in the folder
list.files(path = manual.path, pattern = "\\.txt$")

# Import Raven selection tables for the manual annotations
manual <- imp_raven(all.data = TRUE, path = manual.path,
                name.from.file = T, ext.case = 'lower')

# The column 'sound.files' is helpful so we know which sound file the selections came from
# Notice, though, that right now, the sound file names are not quite right
head(manual$sound.files)
# They end in '.table.wav' and we want them to end in '.flac' like our sound files
# This fixes the sound file extenstions in the column that name the sound files
# substr just tells R which characters we want (by number), and then we paste
# on the correct file extension ".flac"
manual$sound.files <- paste0(substr(manual$sound.files,1,39),".flac")
head(manual$sound.files) # That's better

# NB! We need to subset manual detections to just the species of interest
# otherwise, it will search for every Norwegian species
manual.WIWA <- subset(manual, Species == "WIWA")
manual.REWI <- subset(manual, Species == "Redwing")


# Importing BirdNET detections
# point R towards birdNET selection tables
# Again, you will change this filepath to wherever you've stored your selection tables
bNet.path <- "C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/sampleSoundFiles/birdNET_processing"

# Import selection tables
bNet.dets <- imp_raven(all.data = TRUE, path = bNet.path,
                       name.from.file = T, ext.case = 'lower')

# fix file extenstions again
bNet.dets$sound.files <- paste0(substr(bNet.dets$sound.files,1,39),".flac")

# NB! We need to subset BirdNET detections to just the species of interest
# otherwise, it will search for every Norwegian species
bNet.WIWA <- subset(bNet.dets, `Common Name` == "Willow Warbler")
bNet.REWI <- subset(bNet.dets, `Common Name` == "Redwing")
# bNet.REWI <- subset(bNet.dets, `Common Name` == "Redwing" | `Common Name` == "Crested Tit" | `Common Name` == "Three-toed Woodpecker")

################################### Repeat this for each species!
## ohun package requires columns called selec, start and end
# that contain the selection number and start and end times for each file
manual.WIWA <- manual.WIWA %>%
  mutate(selec = Selection,
         start = `Begin Time (s)`,
         end = `End Time (s)`)

bNet.WIWA <- bNet.WIWA %>%
  mutate(selec = Selection,
         start = `Begin Time (s)`,
         end = `End Time (s)`)

## This is the line that actually diagnoses the precision, recall, and F score
d10 <- diagnose_detection(reference = manual.WIWA, detection = bNet.WIWA,
                   by.sound.file = T, min.overlap = 0.01)

# This prints the summary
summarize_diagnostic(d10)

# This plots the sound files with the manual and birdNET detections labeled
plot_detection(manual.WIWA, bNet.WIWA) +
  theme_bw(base_size = 16)


# Remember, that we set the birdNET threshold pretty low (confidence score = 0.10)
# we might want to see how precision and recall change if we filter to higher
# confidence scores. Then we can plot the precision and recall across different 
# confidence scores

# We could do this more efficiently with a function, but we will do it by hand for now
# Let's create subsets of the birdNET detection tables with higher confidence scores
bNet.2 <- subset(bNet, Confidence > 0.20)
bNet.3 <- subset(bNet, Confidence > 0.30)
bNet.4 <- subset(bNet, Confidence > 0.40)
bNet.5 <- subset(bNet, Confidence > 0.50)
bNet.6 <- subset(bNet, Confidence > 0.60)
bNet.7 <- subset(bNet, Confidence > 0.70)
bNet.8 <- subset(bNet, Confidence > 0.80)
bNet.9 <- subset(bNet, Confidence > 0.90)


## Now, we diagnose the preciison and recall at each confidence score
d10 <- diagnose_detection(reference = manual, detection = bNet,
                          by.sound.file = F, min.overlap = 0.01)

d20 <- diagnose_detection(reference = manual, detection = bNet.2,
                          by.sound.file = F, min.overlap = 0.01)

d30 <- diagnose_detection(reference = manual, detection = bNet.3,
                          by.sound.file = F, min.overlap = 0.01)

d40 <- diagnose_detection(reference = manual, detection = bNet.4,
                          by.sound.file = F, min.overlap = 0.01)

d50 <- diagnose_detection(reference = manual, detection = bNet.5,
                          by.sound.file = F, min.overlap = 0.01)

d60 <- diagnose_detection(reference = manual, detection = bNet.6,
                          by.sound.file = F, min.overlap = 0.01)

d70 <- diagnose_detection(reference = manual, detection = bNet.7,
                          by.sound.file = F, min.overlap = 0.01)

d80 <- diagnose_detection(reference = manual, detection = bNet.8,
                          by.sound.file = F, min.overlap = 0.01)

d90 <- diagnose_detection(reference = manual, detection = bNet.9,
                          by.sound.file = F, min.overlap = 0.01)

# Now let's bind all these results together
PRcurve <- rbind(d10, d20, d30, d40, d50, d60, d70, d80, d90)

# And add a column indicating the birdNet threshold that was used
PRcurve$confidenceScore <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)


# Now, we can plot the precision and recall across the range of birdNET confidence scores
# This is called a precision-recall curve, and it shows us how we balance precision and
# recall when we adjust the threshold of our birdNet confidence scores
# The usual recommendation is to set the threshold where the precision and recall curves cross one another
ggplot(PRcurve, aes(x = confidenceScore)) +
  geom_line(data = PRcurve, aes(y = precision, color = "precision"), size = 1.2) + 
  geom_point(data = PRcurve, aes(y = precision, color = "precision"), size = 2) +
  geom_line(data = PRcurve, aes(y = recall, color = "recall"), size = 1.2) + 
  geom_point(data = PRcurve, aes(y = recall, color = "recall"), size = 2) +
  scale_color_manual(values = c("#0072B2", "#56B4E9")) + # you can change these colors to whatever you want!
  scale_x_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1)), labels= c(seq(0, 1, by = 0.1))) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.25)), labels= c(seq(0, 1, by = 0.25))) +
  theme_light(base_size = 16) +
  labs(x = "birdNET Threshold", y = "Precision/Recall", colour = "")



##########################################################

# Now, let's look at some visualizations of the annotations you made
# You can also adjust this code to summarize the birdNET selection tables
# instead of just your manual annotations, just change the filepaths to where the
# birdNET selection tables are stored

## Pull in selection tables, just as before
# Jørgen
# table.path <- "C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Mentorship/BIOS3060/Jørgen Gulbrandsen/SelectionTables_7May2025"
# Lily - you will alter this filepath to wherever you have your selection tables saved!
table.path <- "C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Mentorship/BIOS3060/Lily Sandstrom/selectionTables"


# See which selection tables are in the folder
list.files(path = table.path, pattern = "\\.txt$")

# Import Raven selection tables
anot.all <- imp_raven(all.data = TRUE, path = table.path,
                         name.from.file = T, ext.case = 'lower')


# First, we may need to clean up the data a bit if there are typos or annotations with no species
unique(anot.all$Species)
# Let's keep just the bluethroat bit in the Species in the first row...
anot.all[1, "Species"] <- "Bluethroat"

# Fixing one name inconsistency
# There was one annotations labeled European crested tit, and we want to change it
# to match the rest of the labels for that species
# Leaving this here in case you run into any similar issues and want to 
# recycle the code
# levels(anot$Species)[levels(anot$Species) == "European crested tit"] <- "Crested tit"

# Can subset to only redwing and bluethroat, or can summarize everything
anot <- subset(anot.all, Species == "Bluethroat" | Species == "Redwing")

# Setting 'Species' to be a factor type object - this can be helpful down the line
anot$Species <- as.factor(anot$Species)

# Pull out date and time for each detection
# This creates new columns named Date, Hour, and Site
anot <- anot %>%
  mutate(Date = as.Date(substr(sound.files, 25, 32), format = "%Y%m%d"),
         Hour = substr(sound.files, 34, 35),
         Site = substr(sound.files, 13, 18))


# You may have selection table files with no species of interest,
# but it is still useful information to know what you annotated, not just
# where you found species of interest!
all.files <- list.files(path = table.path, pattern = "\\.txt$")

all.files.df <- data.frame(Site = substr(all.files, 13, 18),
                           Date = as.Date(substr(all.files, 25, 32), format = "%Y%m%d"),
                           Hour = substr(all.files, 34, 35) )

# creates a vector of all the hours we're interested in and pads them with zeroes,
# so they match the format hours are in within the anot data frame
all.hours <- as.character(sprintf("%02d", seq(1, 24, by = 1)))

# These create summaries of how many files you annotated in each hour of the day and date
all.files.hourSummary <- all.files.df %>%
  group_by(Hour) %>%
  summarize(obs = n()) %>%
  complete(expand(., Hour = all.hours), fill = list(n = 0))
  

all.files.dateSummary <- all.files.df %>%
  group_by(Date) %>%
  summarize(obs = n())


# This will count sound files with detections of focal species
length(unique(anot$sound.files))


# Let's plot the number of detections of each species
anot.species <- anot %>% 
  group_by(Species) %>%
  summarize(noDet = n())

ggplot(anot.species, aes(x = fct_rev(fct_reorder(Species, noDet)), y = noDet, fill = Species))+
  geom_bar(stat="identity", color="black")+
  geom_text(aes(label = noDet), vjust = -0.5) +
  scale_y_continuous(limits = c(0, 25)) +
  theme_light(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none") +
  labs(x = "", y = "Number of Detections")

# We can also break this out by site for a good idea of whether we had a just a few active
# sites or if species were common across the study area
anot.species2 <- anot %>% 
  count(Site, Species) %>%
  complete(expand(., Site), fill = list(n = 0)) %>%
  filter(Species == "Bluethroat" | Species == "Redwing")

ggplot(anot.species2, aes(x = fct_rev(fct_reorder(Site, n)), y = n, fill = Site)) +
  geom_bar(stat="identity", color="black")+
  # geom_text(aes(label = n), vjust = -0.5) +
  scale_y_continuous(limits = c(0, 500)) +
  facet_wrap(~Species, scales = "free") +
  theme_light(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none") +
  labs(x = "Site", y = "Number of Detections")



# Let's summarize detection by the hour of the day to understand the daily patterns
# of vocal activity for different species in our dataset
anot.summary <- anot %>% 
  group_by(Hour, Species) %>%
  summarize(noDet = n())

# NB! A few hours are missing bc we didn't have any detections of species within 
# them, even though we examined at least a few files from each hour
anot$Hour <- as.numeric(anot$Hour)

anot.summary <- anot %>%
  count(Hour, Species) %>%
  # this line fills in those missing hours in our dataframe
  complete(expand(., Hour = seq(1, 24, by = 1), Species), fill = list(n = 0)) %>% # This code fills in 0s for site/year/surveyPd combos with no observations
  filter(Species == "Bluethroat" | Species == "Redwing")
  
# Might be good to standardize by the number of files examined for each hour of the day,
# since we know from our summary above that we annotated different numbers of files from 
# different parts of the day
all.files.hourSummary$Hour <- as.numeric(all.files.hourSummary$Hour)

detPerFile <- merge(anot.summary, all.files.hourSummary, by = "Hour", keep.all=T)

detPerFile <- mutate(detPerFile, detPerFile = n / obs)

ggplot(detPerFile, aes(Species, as.factor(Hour), fill = detPerFile)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_light(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(x = "", y = "Hour of Day", fill = "Songs per file")
# Has grey right now for missing data, so will look better once more data is annotated!


# Can save this plot with high resolution, also...
# edit this to the path of where you want to save the image file
output.path <- "C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Mentorship/BIOS3060/Jørgen Gulbrandsen/"

# Wrap the code for the plot you want to save in png() and dev.off()

# png() specifies where to save the file and the file name, the size of the figure,
# what units you've specified the size in, and the resolution in dpi (600 is good for a poster) 
png(paste0(output.path,"diurnalPatterns_Heimdalen.png"), width = 7, height = 10, units = "in", res = 600)

ggplot(detPerFile, aes(Species, as.factor(Hour), fill = detPerFile)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_light(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(x = "", y = "Hour of Day", fill = "Songs per file")

dev.off()
# This one is important too! 



###### We can make the same figure with BirdNET outputs, as well! 
# Just change the input selection tables to be the BirdNET selections
# and don't forget to filter them to the focal species



###### Can also examine seasonal patterns using similar code

# Perhaps use day of year instead of the exact date for simplicity
anot$doy <- strftime(anot$Date, format = "%j")

# List all dates from 27 May to end of July
all.dates <- as.character(seq(148, 213, by = 1))

# AGain, this fills in dates that we don't have observations for
# during the period in which we recorded
anot.season <- anot %>%
  mutate(doy = strftime(Date, format = "%j")) %>%
  group_by(doy) %>%
  summarize(nDet = n()) %>%
  complete(expand(., doy = all.dates), fill = list(n = 0)) %>%
  mutate(Species = "Bluethroat") # Fix this to appropriate species 
  

# Let's standardize by the number of annotated files again
all.files.dateSummary <- all.files.df %>%
  mutate(doy = strftime(Date, format = "%j")) %>%
  group_by(doy) %>%
  summarize(filesReviewed = n()) %>%
  complete(expand(., doy = all.dates), fill = list(n = 0))

# Merge the tables by day of year
detPerFile.season <- merge(anot.season, all.files.dateSummary, by = "doy", keep.all=T)

# Divide the number of detections by the number of files annotated
detPerFile.season <- mutate(detPerFile.season, detPerFile = nDet / filesReviewed)

# And make a plot! 
ggplot(detPerFile.season, aes(Species, as.factor(doy), fill = detPerFile)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_light(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(x = "", y = "Day of Year", fill = "Songs per file")


# If you want to export it, wrap it in the png() and dev.off() commands!


