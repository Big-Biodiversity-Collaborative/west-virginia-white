# Plot gdd and delta gdd for observational data
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-04-06

require(terra)
require(ggplot2)
require(dplyr)

# Interested to explore potential differential responses to climate change 
# at different growing degree days. Have "warmer" sites become warmer or cooler
# relative to "cooler" sites.

# Read in the cleaned observations
all_obs <- read.csv(file = "data/filtered-obs.csv")

# Growing degree days from https://climatena.ca/spatialData
gdd_recent <- terra::rast(x = "data/DD5_1991-2020.tif")
gdd_historical <- terra::rast(x = "data/DD5_1961-1990.tif")
# plot(gdd)
gdd_diff <- gdd_recent - gdd_historical

# Add the recent (~contemporary) gdd and delta gdd to observations data frame
all_obs$gdd <- terra::extract(x = gdd_recent,
                              y = all_obs[, c("longitude", "latitude")])[, 2]
all_obs$gdd_diff <- terra::extract(x = gdd_diff,
                                   y = all_obs[, c("longitude", "latitude")])[, 2]

################################################################################
# First investigation, just plot delta vs. GDD

# Start by plotting just insect data
ggplot(data = all_obs %>% filter(organism == "insect"), 
       mapping = aes(x = gdd, y = gdd_diff)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_bw()
summary(lm(gdd_diff ~ gdd, data = all_obs %>% filter(organism == "insect")))
# Effect size of GDD is 0.023, so higher change in warmer spots

# Now try all observations
ggplot(data = all_obs, 
       mapping = aes(x = gdd, y = gdd_diff)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_bw()
summary(lm(gdd_diff ~ gdd, data = all_obs))
# Effect size of GDD is 0.0022, so higher change in warmer spots, but effect
# size is an order of magnitude smaller than just insect observations

################################################################################
# Second investigation, uneasy about using raw difference, so plot relative 
# magnitude of change instead

all_obs <- all_obs %>%
  mutate(gdd_rel_diff = gdd_diff/gdd)

# Start by plotting just insect data
ggplot(data = all_obs %>% filter(organism == "insect"), 
       mapping = aes(x = gdd, y = gdd_rel_diff)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_bw()
# TODO: Likely VERY heteroskedastic, so interpret OLS model with caution
summary(lm(gdd_rel_diff ~ gdd, data = all_obs %>% filter(organism == "insect")))

# Plot data for all observations
ggplot(data = all_obs, 
       mapping = aes(x = gdd, y = gdd_rel_diff)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_bw()
# TODO: Heteroskedastic, interpret OLS with caution
summary(lm(gdd_rel_diff ~ gdd, data = all_obs))

# Upshot: (ignoring heteroskedasticity concerns for now) Cooler sites have 
# seen greater relative increase in GDD than warmer sites. That is, the 
# *relative* increase in growing degree days has been larger in sites that are 
# currently lower gdd over those sites currently higher gdd.