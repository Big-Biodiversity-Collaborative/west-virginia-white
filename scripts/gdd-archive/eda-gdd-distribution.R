# Look at geographic distribution of GDD bins
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-06-08

require(dplyr)
require(ggplot2)
# require(sf)        # Filtering points in polygon
require(lubridate) # Julian days
require(terra)     # Extracting growing degree days data

# Read in filtered data
all_obs <- read.csv(file = "data/filtered-obs.csv")

# Growing degree days from https://climatena.ca/spatialData
gdd <- terra::rast(x = "data/DD5_1991-2020.tif")

# Get extent of map we want to make; extent of WVW +0.25 degrees
min_lon <- min(all_obs$longitude) - 0.25
max_lon <- max(all_obs$longitude) + 0.25
min_lat <- min(all_obs$latitude) - 0.25
max_lat <- max(all_obs$latitude) + 0.25
map_extent <- terra::ext(c(min_lon, max_lon, min_lat, max_lat))

# Crop GDD data to a little beyond observational data (quarter degree)
gdd <- terra::crop(x = gdd, y = map_extent)

# Convert to a data frame so we can use ggplot
gdd_df <- terra::as.data.frame(x = gdd, xy = TRUE)

# Extract relevant data for observations
all_obs$gdd <- terra::extract(x = gdd,
                              y = all_obs[, c("longitude", "latitude")])[, 2]

################################################################################
# Data preparation for three maps
# Three bins for three different ridge plots
gdd_bins <- stats::quantile(x = all_obs$gdd[all_obs$organism == "insect"],
                            probs = c(1/3, 2/3))

# Set bin assignment for each row
all_obs <- all_obs %>%
  mutate(gdd_bin = case_when(gdd < gdd_bins[1] ~ "low",
                             gdd < gdd_bins[2] ~ "medium",
                             gdd >= gdd_bins[2] ~ "high",
                             TRUE ~ NA_character_)) %>%
  mutate(gdd_bin = factor(gdd_bin, levels = c("low", "medium", "high")))

# One plot, different colors for the bins
one_plot <- ggplot(data = gdd_df, mapping = aes(x = x, y = y)) +
  geom_raster(fill = "#E5E5E5") + # to just show gray map
  geom_point(data = all_obs, 
             mapping = aes(x = longitude, 
                           y = latitude,
                           color = gdd_bin),
             size = 0.4) +
  scale_color_manual(values = c("#404040", "#fdae61", "#ca0020")) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()
one_plot
ggsave(plot = one_plot,
       filename = "output/gdd-dist-one.png")

# Three plots, faceted by bins
facet_plot <- ggplot(data = gdd_df, mapping = aes(x = x, y = y)) +
  geom_raster(fill = "#E5E5E5") + # to just show gray map
  geom_point(data = all_obs, 
             mapping = aes(x = longitude, 
                           y = latitude),
             size = 0.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  facet_wrap(~ gdd_bin, ncol = 2) +
  theme_bw()

facet_plot
ggsave(plot = facet_plot,
       filename = "output/gdd-dist-facet.png")
