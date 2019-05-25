# Testing weighted least squares regression in WVW
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-05-24

rm(list = ls())

################################################################################
library(lubridate)
library(tidyverse)

# Read in iNaturalist observations
# 52539 is West Virginia White
iNaturalist <- read.csv(file = "data/observations-52593.csv")
iNat.obs <- iNaturalist[, c("latitude", "longitude", "observed_on")]
iNat.obs$observed_on <- as.Date(iNat.obs$observed_on)

# Read in GBIF observations
gbif <- read.delim(file = "data/0015022-190415153152247.csv")
gbif.obs <- gbif[, c("decimalLatitude", "decimalLongitude", "eventDate")]
gbif.obs$eventDate <- as.Date(x = gbif.obs$eventDate)
gbif.obs <- na.omit(gbif.obs)

# Join data sets
colnames(iNat.obs) <- c("latitude", "longitude", "date")
colnames(gbif.obs) <- c("latitude", "longitude", "date")
wvw <- rbind(iNat.obs, gbif.obs)

# Remove some bad geo coordinates (mostly GBIF)
wvw <- wvw[wvw$latitude > 29, ]
wvw <- wvw[wvw$longitude > -100, ]

# Extract year and day of year
wvw$year <- year(wvw$date)
wvw$yday <- as.POSIXlt(wvw$date)$yday

# Dates of January 1 (yday == 0) are not realistic
wvw <- wvw[wvw$yday > 0, ]
# Drop any from current, incomplete year
wvw <- wvw[wvw$year != year(today()), ]

# Drop duplicates
wvw <- unique(wvw)

# Will need to restrict it to years with a minimum number of observations (for 
# calculating standard deviations)
minimum.required <- 5

################################################################################
# Analysis

wvw.mins <- wvw %>%
  group_by(year) %>%
  filter(n() > minimum.required) %>%
  summarise(early = min(x = yday),
            variance = sd(x = yday)^2,
            n.obs = n())
wvw.mins <- as.data.frame(wvw.mins)

# Ordinary least squares (what we *don't* want to do)
ols.model <- lm(early ~ year, data = wvw.mins)
summary(ols.model)

# Weighted least squares
# Weight is inverse of variance for that year
wls.model <- lm(early ~ year, data = wvw.mins, weights = 1/variance)
summary(wls.model)

# Weighted least squares
# Weight is number of observations for that year
# Rationale: years with more observations have "more information", and thus 
# should be weighted more in coefficient estimate; given that we are using 
# the minimum of a response variable, rather than all values for a year, this 
# seems more appropriate
wls.model.n <- lm(early ~ year, data = wvw.mins, weights = n.obs)
summary(wls.model.n)
# Further justification comes from the documentation for lm:
#   when the elements of weights are positive integers w_i, that each response 
#   y_i is the mean of w_i unit-weight observations (including the case that 
#   there are w_i observations equal to y_i and the data have been summarized).
# Where the difference is that y_i is the minimum, not the mean.

# Quick look at _latest_ flight times
wvw.max <- wvw %>%
  group_by(year) %>%
  filter(n() > minimum.required) %>%
  summarise(late = max(x = yday),
            variance = sd(x = yday)^2,
            n.obs = n())
wvw.max <- as.data.frame(wvw.max)

wls.model.max <- lm(late ~ year, data = wvw.max, weights = n.obs)
summary(wls.model.max)
# Non-significant
plot(wvw.max$year, wvw.max$late)
abline(wls.model.max)
