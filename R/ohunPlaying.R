
# Playing with warbleR and ohun packages for assessment of ML outputs

library(Rraven)
library(warbleR)
library(ohun)

# Path to selection tables
ST.path <- "C:/Users/brunk/Raven Pro 1.6/Selections/HEIM/"

# See which selection tables are in the folder
list.files(path = ST.path, pattern = "\\.txt$")

# Import Raven selection tables
ST <- imp_raven(all.data = TRUE, path = ST.path,
                name.from.file = T, ext.case = 'lower')

# fix file extenstions
ST$sound.files <- paste0(substr(ST$sound.files,1,39),".flac")


#####################################
library(tidyverse)
library(viridis)

WIWA.path <- "C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/sampleSoundFiles/manualSelectionTables"

# See which selection tables are in the folder
list.files(path = WIWA.path, pattern = "\\.txt$")

# Import Raven selection tables
WIWA.manual <- imp_raven(all.data = TRUE, path = WIWA.path,
                name.from.file = T, ext.case = 'lower')

# fix file extenstions
WIWA.manual$sound.files <- paste0(substr(WIWA.manual$sound.files,1,39),".flac")

# Pull in BirdNET detections
# point R towards brdNET selection tables
bNet.path <- "C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/sampleSoundFiles/birdNET_processing"

# Import selection tables
bNet.dets <- imp_raven(all.data = TRUE, path = bNet.path,
                       name.from.file = T, ext.case = 'lower')

# fix file extenstions
bNet.dets$sound.files <- paste0(substr(bNet.dets$sound.files,1,39),".flac")

# Subset BirdNET detections to species of interest
WIWA.bnet <- subset(bNet.dets, `Common Name` == "Willow Warbler")


WIWA.manual <- WIWA.manual %>%
  mutate(selec = Selection,
         start = `Begin Time (s)`,
         end = `End Time (s)`)

WIWA.bnet <- WIWA.bnet %>%
  mutate(selec = Selection,
         start = `Begin Time (s)`,
         end = `End Time (s)`)


dd <- diagnose_detection(reference = WIWA.manual, detection = WIWA.bnet,
                   by.sound.file = T, min.overlap = 0.01)

# First sound file lists 60 false positives, but I don't see any false positives 
# when I examine the data...

# Second sound file looks good

summarize_diagnostic(dd)


plot_detection(WIWA.manual, WIWA.bnet) +
  # scale_x_continuous(limits=c(500,1000)) +
  theme_bw(base_size = 16)




########### Playing with some data visualizations
## Jørgen's selection tables
table.path <- "C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Mentorship/BIOS3060/Jørgen Gulbrandsen/SelectionTables_7May2025"

# See which selection tables are in the folder
list.files(path = table.path, pattern = "\\.txt$")

# Import Raven selection tables
anot <- imp_raven(all.data = TRUE, path = table.path,
                         name.from.file = T, ext.case = 'lower')

# Dropping levels with no species detections
anot <- subset(anot, Species != "" & Species != "-")
# Setting Species to be a factor
anot$Species <- as.factor(anot$Species)
# Fixing one name inconsistency
levels(anot$Species)[levels(anot$Species) == "European crested tit"] <- "Crested tit"

# Pull out date and time for each detection
anot <- anot %>%
  mutate(Date = as.Date(substr(sound.files, 27, 34), format = "%Y%m%d"),
         Hour = substr(sound.files, 36, 37),
         Site = substr(sound.files, 13, 16))

# 192 sound files annotated - lots of files had no species of interest
# but it is still useful information to know what we annotated, not just
# where we found species of interest
all.files <- list.files(path = table.path, pattern = "\\.txt$")

all.files.df <- data.frame(Site = substr(all.files, 13, 16),
                           Date = as.Date(substr(all.files, 27, 34), format = "%Y%m%d"),
                           Hour = substr(all.files, 36, 37) )

all.files.hourSummary <- all.files.df %>%
  group_by(Hour) %>%
  summarize(obs = n())

all.files.dateSummary <- all.files.df %>%
  group_by(Date) %>%
  summarize(obs = n())


# 36 sound files with detections of focal species
length(unique(anot$sound.files))


# Number of detections by species
anot.species <- anot %>% 
  group_by(Species) %>%
  summarize(noDet = n())

ggplot(anot.species, aes(x = fct_rev(fct_reorder(Species, noDet)), y = noDet, fill = Species))+
  geom_bar(stat="identity", color="black")+
  geom_text(aes(label = noDet), vjust = -0.5) +
  scale_y_continuous(limits = c(0, 1500)) +
  theme_light(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none") +
  labs(x = "", y = "Number of Detections")

# Break it out by site, also
anot.species2 <- anot %>% 
  count(Site, Species) %>%
  complete(expand(., Site, Species), fill = list(n = 0))

ggplot(anot.species2, aes(x = fct_rev(fct_reorder(Site, n)), y = n, fill = Site)) +
  geom_bar(stat="identity", color="black")+
  # geom_text(aes(label = n), vjust = -0.5) +
  scale_y_continuous(limits = c(0, 500)) +
  facet_wrap(~Species, scales = "free") +
  theme_light(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none") +
  labs(x = "Site", y = "Number of Detections")



# Might be nice to look at diurnal patterns so far...
anot.summary <- anot %>% 
  group_by(Hour, Species) %>%
  summarize(noDet = n())

# NB! A few hours are missing bc we didn't have any detections of species within 
# them, even though we examined at least a few files from each hour

anot$Hour <- as.numeric(anot$Hour)

anot.summary <- anot %>%
  count(Hour, Species) %>%
  # this line fills in those missing hours in our dataframe
  complete(expand(., Hour = seq(1, 24, by = 1), Species), fill = list(n = 0)) # This code fills in 0s for site/year/surveyPd combos with no observations

# Might be good to standardize by the number of files examined for each hour of the day
all.files.hourSummary[1, 1] = '24'
all.files.hourSummary$Hour <- as.numeric(all.files.hourSummary$Hour)

detPerFile <- merge(anot.summary, all.files.hourSummary, by = "Hour", keep.all=T)

detPerFile <- mutate(detPerFile, detPerFile = n / obs)

ggplot(detPerFile, aes(Species, as.factor(Hour), fill = detPerFile)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_light(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(x = "", y = "Hour of Day", fill = "Songs per file")

# Can save this plot with high resolution, also...
# edit this to where you want to save the image file
output.path <- "C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Mentorship/BIOS3060/Jørgen Gulbrandsen/"

png(paste0(output.path,"diurnalPatterns_Hurdal.png"), width = 7, height = 10, units = "in", res = 600)

ggplot(detPerFile, aes(Species, as.factor(Hour), fill = detPerFile)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_light(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(x = "", y = "Hour of Day", fill = "Songs per file")

dev.off()




###### We can make the same figure with BirdNET outputs, as well! 

###### Can also examine seasonal patterns using similar code
anot.season <- anot %>% 
  group_by(Date, Species) %>%
  summarize(noDet = n())

# Perhaps use DOY instead of the exact date...



