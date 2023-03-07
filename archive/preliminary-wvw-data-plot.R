# Investigate west virginia white phenology
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-05-02

rm(list = ls())

################################################################################
library(lubridate)
library(ggplot2)

# Read in iNaturalist observations
# West Virginia White
wvw <- read.csv(file = "data/observations-52593.csv")
# Mustard White
mw <- read.csv(file = "data/observations-52726.csv")
# Cabbage White
cw <- read.csv(file = "data/observations-52729.csv")
# Exclude any cabbage white observations beyond the western limit of WVW & MW
# observations
cw <- cw[cw$longitude >= min(wvw$longitude, mw$longitude), ]
cw$scientific_name <- gsub(pattern = "rapae rapae", 
                           replacement = "rapae",
                           x = cw$scientific_name)
# Put 'em together
obs <- rbind(wvw, mw, cw)

obs$observed_on <- as.Date(obs$observed_on)

# Extract year and day of year
obs$year <- year(obs$observed_on)
obs$yday <- as.POSIXlt(obs$observed_on)$yday

# Remove observations for 2019 and before 2005
obs.2005.2018 <- obs[obs$year >= 2005 & obs$year < 2019, ]

# Plot observations for each year separately
obs.plot <- ggplot(data = obs.2005.2018, mapping = aes(x = year, y = yday, color = latitude)) +
  geom_point() +
  geom_smooth() +
  xlab(label = "Year") +
  ylab(label = "Day") +
  ggtitle("Observations 2005-2018") +
  facet_wrap(~ scientific_name) +
  scale_color_continuous(name = "Latitude")
print(obs.plot)

# Do a quick linear regression, including latitude as crude co-variate
wvw.lm <- lm(yday ~ year + latitude, data = obs.2005.2018[obs.2005.2018$scientific_name == "Pieris virginiensis", ])
wvw.summary <- summary(wvw.lm)
wvw.summary

mw.lm <- lm(yday ~ year + latitude, data = obs.2005.2018[obs.2005.2018$scientific_name == "Pieris oleracea", ])
mw.summary <- summary(mw.lm)
mw.summary

cw.lm <- lm(yday ~ year + latitude, data = obs.2005.2018[obs.2005.2018$scientific_name == "Pieris rapae", ])
cw.summary <- summary(cw.lm)
cw.summary
