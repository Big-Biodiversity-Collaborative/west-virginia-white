# Make a figure with growing degree days and WVW observations overlaid
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-06

require(terra)
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
gdd_palette <- "YlOrBr" # "YlGnBu" "Spectral"
gdd_plot <- ggplot(data = gdd_df, mapping = aes(x = x, y = y)) +
  geom_raster(mapping = aes(fill = GDD)) +
  scale_fill_distiller(palette = gdd_palette, 
                       direction = 1,
                       name = "GDD") +
  geom_point(data = insect, 
             mapping = aes(x = longitude, y = latitude),
             shape = 19, size = 0.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "none")
gdd_plot
ggsave(filename = "output/figure-1a.png",
       plot = gdd_plot)

################################################################################
# Multi-panel plot, where we have one panel for gdd + WVW obs, and three plots:
# one for each of the host plant species, with WVW obs, too.

# Start by reading in all observations for each of the host plant species
host_concatenata <- read.csv(file = "data/cardamine_concatenata-gbif-clean.csv")
host_diphylla <- read.csv(file = "data/cardamine_diphylla-gbif-clean.csv")
# GBIF has separate entries for one of the host plants (synonyms) that *do not* 
# come through on a query of the accepted name
host_laevigata <- read.csv(file = "data/borodinia_laevigata-gbif-clean.csv")
b_laevigata_sp <- host_laevigata$species[1]
host_A_laevigata <- read.csv(file = "data/arabis_laevigata-gbif-clean.csv")
host_laevigata <- host_laevigata %>%
  bind_rows(host_A_laevigata) %>%
  mutate(species = b_laevigata_sp)

host_list <- list(concatenata = list(data = host_concatenata,
                                     color = "#a6dba0"), 
                  diphylla = list(data = host_diphylla,
                                  color = "#008837"),
                  laevigata = list(data = host_laevigata,
                                   color = "#5aae61"))

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
# host_plots[[3]]

# Now use ggarrange to make a single plot
# Two step process. Weird.
plot_list <- list(gdd_plot)
plot_list <- c(plot_list, host_plots)

# Add titles to plots (works better than ggarrange's approach)
plot_list[[1]] <- plot_list[[1]] + ggtitle("(a) GDD")
plot_list[[2]] <- plot_list[[2]] + ggtitle(expression(paste("(b) ", italic("C. concatenata"))))
plot_list[[3]] <- plot_list[[3]] + ggtitle(expression(paste("(c) ", italic("C. diphylla"))))
plot_list[[4]] <- plot_list[[4]] + ggtitle(expression(paste("(d) ", italic("B. laevigata"))))

four_panel <- ggpubr::ggarrange(plotlist = plot_list,
                                ncol = 2,
                                nrow = 2)
four_panel
ggsave(filename = "output/figure-1.png",
       plot = four_panel)
