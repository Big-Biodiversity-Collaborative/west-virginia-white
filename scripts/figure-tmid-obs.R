# Make a figure with observations of insect and two host plant species
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-06

require(terra)   # To use growing degree day raster for base map
require(ggplot2)
require(dplyr)
require(ggpubr)  # For making a multi-panel plot

# Load in observation data (TODO: clean or filtered?)
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
# We are only using this to draw gray background map of eastern U.S.
# TODO: Get a better/easier outline of eastern U.S.?
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

################################################################################
# Multi-panel plot, where we have two panels: one for each of the Cardamine 
# host plant species, with WVW obs, too.

# Start by reading in all observations for each of the host plant species
host_diphylla <- read.csv(file = "data/cardamine_diphylla-gbif-clean.csv")
host_concatenata <- read.csv(file = "data/cardamine_concatenata-gbif-clean.csv")

host_list <- list(diphylla = list(data = host_diphylla,
                                  color = "#008837"),
                  concatenata = list(data = host_concatenata,
                                     color = "#a6dba0"))

# POC on one host:
# plot_concatenata <- ggplot(data = gdd_df, mapping = aes(x = x, y = y)) +
#   geom_raster(fill = "#EFEFEF") + # to just show gray map
#   geom_point(data = host_concatenata,
#              mapping = aes(x = longitude, y = latitude),
#              shape = 1, size = 0.5, color = "#a6dba0") +
#   geom_point(data = insect,
#              mapping = aes(x = longitude, y = latitude),
#              shape = 19, size = 0.1) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   ylim(c(min_lat, max_lat)) +
#   xlim(c(min_lon, max_lon)) +
#   theme_bw()
# plot_concatenata

# Vectorized on the list
host_plots <- lapply(X = host_list, 
                     FUN = function(x)
                     {
                       ggplot(data = gdd_df, mapping = aes(x = x, y = y)) +
                         geom_raster(fill = "#EFEFEF") + # to just show gray map
                         geom_point(data = x$data,
                                    mapping = aes(x = longitude, y = latitude),
                                    shape = 19, size = 0.75, color = "#96db90") +
                         geom_point(data = insect, 
                                    mapping = aes(x = longitude, y = latitude),
                                    shape = 19, size = 0.5, color = "#000000") +
                         xlab("Longitude") +
                         ylab("Latitude") +
                         ylim(c(min_lat, max_lat)) +
                         xlim(c(min_lon, max_lon)) +
                         theme_bw()
                     })
# host_plots[[1]]
# host_plots[[2]]

# Now use ggarrange to make a single plot

# Add titles to plots (works better than ggarrange's approach)
host_plots[[1]] <- host_plots[[1]] + 
  ggtitle(expression(paste("(a) ", italic("C. diphylla"))))
host_plots[[2]] <- host_plots[[2]] + 
  ggtitle(expression(paste("(b) ", italic("C. concatenata"))))

two_panel <- ggpubr::ggarrange(plotlist = host_plots,
                                ncol = 2)
two_panel
ggsave(filename = "output/figure-1.png",
       plot = two_panel)
