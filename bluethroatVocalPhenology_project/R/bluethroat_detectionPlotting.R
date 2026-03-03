
library(tidyverse)

bluethroat <- read.csv("C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/BirdNET/HEIM_PAM_2025_birdNET_Feb2026/speciesSelections/Bluethroat.csv")

## Plot some bluethroat detections
hist(bluethroat$Confidence)

bt.25 <- subset(bluethroat, Confidence >= 0.25)
bt.50 <- subset(bluethroat, Confidence >= 0.50)

bt <- data.frame(Species = "Bluethroat",
                 Site = substr(bt.50$Begin.Path, 49,54),
                 Year = substr(bt.50$Begin.Path, 78,81),
                 Month = substr(bt.50$Begin.Path, 82,83),
                 Day = substr(bt.50$Begin.Path, 84,85),
                 confScore = bt.50$Confidence
)

bt$Date <- as.Date(with(bt,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

dayCount <- bt %>%
  group_by(Date, Site) %>%
  summarize(count = n())

dayCount.drop9 <- subset(dayCount, Site != "OHV009")

ggplot(dayCount.drop9, aes(x=Date, y=count))+
  geom_bar(stat='identity', fill="blue")+
  ylab("Detection Count") +
  # scale_y_continuous(limits = c(0, 200)) +
  facet_wrap(~Site) + 
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(color = "black"),
        panel.grid.major.y = element_line(colour = "gray70", linewidth = 0.25),
        legend.position="none"
  )

totalCount <- bt %>%
  group_by(Date) %>%
  summarize(count = n())

ggplot(totalCount, aes(x=Date, y=count))+
  geom_bar(stat='identity', fill="blue")+
  ylab("Detection Count") +
  # scale_y_continuous(limits = c(0, 200)) +
  # facet_wrap(~Site) + 
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(color = "black"),
        panel.grid.major.y = element_line(colour = "gray70", linewidth = 0.25),
        legend.position="none"
  )
