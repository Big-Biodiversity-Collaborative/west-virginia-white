# Make maps of observation data
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-02

# DEPRECATED
# See figure-gdd-obs.R for plots of observations

require(ggplot2)
require(dplyr)
require(sf)

# Load in insect data
obs <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")

# Convert to a simple feature for easier plotting
wsg84 <- "EPSG:4326"
obs_sf <- sf::st_as_sf(obs[, c("longitude", "latitude")], 
                       crs = wsg84,
                       coords = c("longitude", "latitude"))

ggplot(data = obs_sf) +
  geom_sf()

# Density plots
ggplot(data = obs[, c("longitude", "latitude")], 
       mapping = aes(x = longitude, y = latitude)) +
  geom_density_2d_filled(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw()
