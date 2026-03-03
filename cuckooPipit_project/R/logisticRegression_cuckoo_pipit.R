
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
# Now, let's subset our files to those we want to annotate! 

# Filter pipit detections to those >= 95%
# Keep files where the first detection is after five minutes (300 seconds)
# and before 25 minutes (1500 seconds)

# Check out the distribution of confidence scores for both species
cc.det <- read.csv("C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/BirdNET/HEIM_PAM_2025_birdNET_Feb2026/speciesSelections/Common Cuckoo.csv")
mp.det <- read.csv("C:/Users/brunk/OneDrive/Documents/1. Oslo Postdoc/Acoustic/BirdNET/HEIM_PAM_2025_birdNET_Feb2026/speciesSelections/Meadow Pipit.csv")

hist(mp.det$Confidence, xlab = "birdNET confidence score", main = "")
abline(v = 0.375, lty = "dashed", lwd = 2, col = rgb(0,0.8,1,.75)) # 95% threshold

# Often, this thresholding procedure limits the detection radius of the 
# recorder, so let's try to limit the radius similarlyfor both species
# We know all of the cuckoo detections were correct, but instead of just
# using them all, it might be nice to cut off the same percentile of detections
# as we did for the meadow pipit. 

# This tells us what % of mp detections has a birdNET confidence score > 0.375
ecdf(mp.det$Confidence)(0.375)
# Then we can calculate a threshold for cuckoo that leaves the same 
# % of detections
cc.thresh <- quantile(cc.det$Confidence, 0.484)

hist(cc.det$Confidence, xlab = "birdNET confidence score", main = "")
abline(v = cc.thresh, lty = "dashed", lwd = 2, col = rgb(0,0.8,1,.75))


## Subset both species
mp.det.sub <- subset(mp.det, Confidence >= 0.375)
cc.det.sub <- subset(cc.det, Confidence >= cc.thresh)











