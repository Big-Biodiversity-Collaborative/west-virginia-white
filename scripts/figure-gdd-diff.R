# Make a figure showing change in growing degree days between 61-90 and 90-2020
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-06

require(terra)
require(ggplot2)
require(dplyr)

# Load in observation data to dictate map bounds
insect <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")

# Only include >= 2000 for plot
insect <- insect %>%
  filter(year >= 2000)

# Get extent of map we want to make; extent of WVW +0.25 degrees
min_lon <- min(insect$longitude) - 0.25
max_lon <- max(insect$longitude) + 0.25
min_lat <- min(insect$latitude) - 0.25
max_lat <- max(insect$latitude) + 0.25
map_extent <- terra::ext(c(min_lon, max_lon, min_lat, max_lat))

# Growing degree days from https://climatena.ca/spatialData
gdd_recent <- terra::rast(x = "data/DD5_1991-2020.tif")
gdd_historical <- terra::rast(x = "data/DD5_1961-1990.tif")
# plot(gdd)
gdd_diff <- gdd_recent - gdd_historical

# Crop GDD data to a little beyond WVW data (quarter degree)
gdd <- terra::crop(x = gdd_diff, y = map_extent)
# plot(gdd)

# Convert to a data frame so we can use ggplot
gdd_df <- terra::as.data.frame(x = gdd, xy = TRUE)
# Rename column for easier reading
gdd_df <- gdd_df %>%
  rename(GDD = `DD5_1991-2020`)

# Plot GDD data, and add WVW as points
gdd_palette <- "BrBG" # "Spectral" # "YlGnBu"
gdd_plot <- ggplot(data = gdd_df, mapping = aes(x = x, y = y)) +
  geom_raster(mapping = aes(fill = GDD)) +
  scale_fill_distiller(palette = gdd_palette, 
                       direction = -1,
                       name = "\u0394 GDD") +
  geom_point(data = insect, 
             mapping = aes(x = longitude, y = latitude),
             shape = 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()
gdd_plot
ggsave(filename = "output/figure-1b.png",
       plot = gdd_plot)
