# Make a figure with growing degree days and WVW observations overlaid
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-06

require(terra)
require(ggplot2)
require(dplyr)

# Load in observation data (TODO: clean or filtered?)
insect <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")

# Get extent of map we want to make; extent of WVW +0.25 degrees
min_lon <- min(insect$longitude) - 0.25
max_lon <- max(insect$longitude) + 0.25
min_lat <- min(insect$latitude) - 0.25
max_lat <- max(insect$latitude) + 0.25
map_extent <- terra::ext(c(min_lon, max_lon, min_lat, max_lat))

# Growing degree days from https://climatena.ca/spatialData
gdd <- terra::rast(x = "data/DD5_1991-2020.tif")
# plot(gdd)

# Crop GDD data to a little beyond WVW data (quarter degree)
gdd <- terra::crop(x = gdd, y = map_extent)
# plot(gdd)

# Convert to a data frame so we can use ggplot
gdd_df <- terra::as.data.frame(x = gdd, xy = TRUE)
# Rename column for easier reading
gdd_df <- gdd_df %>%
  rename(GDD = `DD5_1991-2020`)

# Plot GDD data, and add WVW as points
gdd_palette <- "Spectral" # "YlGnBu"
gdd_plot <- ggplot(data = gdd_df, mapping = aes(x = x, y = y)) +
  geom_raster(mapping = aes(fill = GDD)) +
  scale_fill_distiller(palette = gdd_palette, 
                       direction = -1,
                       name = "GDD") +
  geom_point(data = insect, 
             mapping = aes(x = longitude, y = latitude),
             shape = 1) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()
gdd_plot
ggsave(filename = "output/figure-1.png",
       plot = gdd_plot)
