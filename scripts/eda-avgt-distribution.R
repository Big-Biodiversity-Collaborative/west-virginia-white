# Look at geographic distribution of two-month temperature bins
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-06-29

require(dplyr)
require(ggplot2)
require(lubridate) # Julian days
require(terra)     # mapping points

# Read in filtered data
all_obs <- read.csv(file = "data/filtered-obs.csv")

# Add temperature data from data-weather.R
avgt <- read.csv(file = "data/temperature-obs.csv")
all_obs <- all_obs %>%
  left_join(avgt, by = "gbifID")

# Drop observations that are missing temperature data
all_obs <- all_obs %>%
  filter(!is.na(avgt))

# Get extent of map we want to make; extent of WVW +0.25 degrees
min_lon <- min(all_obs$longitude) - 0.25
max_lon <- max(all_obs$longitude) + 0.25
min_lat <- min(all_obs$latitude) - 0.25
max_lat <- max(all_obs$latitude) + 0.25
map_extent <- terra::ext(c(min_lon, max_lon, min_lat, max_lat))

################################################################################
# Data preparation for three maps
# Three bins for three different ridge plots
avgt_bins <- stats::quantile(x = all_obs$avgt[all_obs$organism == "insect"],
                            probs = c(1/3, 2/3))

# Set bin assignment for each row
all_obs <- all_obs %>%
  mutate(avgt_bin = case_when(avgt < avgt_bins[1] ~ "low",
                              avgt < avgt_bins[2] ~ "medium",
                              avgt >= avgt_bins[2] ~ "high",
                              TRUE ~ NA_character_)) %>%
  mutate(avgt_bin = factor(avgt_bin, levels = c("low", "medium", "high")))

# One plot, different colors for the bins
one_plot <- ggplot(data = all_obs, mapping = aes(x = longitude, y = latitude)) +
  geom_point(data = all_obs, 
             mapping = aes(x = longitude, 
                           y = latitude,
                           color = avgt_bin),
             size = 0.4) +
  scale_color_manual(values = c("#404040", "#fdae61", "#ca0020")) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()
one_plot
ggsave(plot = one_plot,
       filename = "output/avgt-dist-one.png")

# Four plots, different colors for the bins and separate plots for each species
four_plots <- ggplot(data = all_obs, mapping = aes(x = longitude, y = latitude)) +
  geom_point(data = all_obs, 
             mapping = aes(x = longitude, 
                           y = latitude,
                           color = avgt_bin),
             size = 0.4) +
  scale_color_manual(values = c("#404040", "#fdae61", "#ca0020"),
                     name = "Temperature") +
  xlab("Longitude") +
  ylab("Latitude") +
  facet_wrap(~ species) +
  theme_bw()
four_plots

################################################################################
# Temporal (i.e. Year) distribution of temperature
scatterplot <- ggplot(data = all_obs, mapping = aes(x = year, y = avgt)) +
  geom_point(mapping = aes(color = avgt_bin)) +
  scale_color_manual(values = c("#404040", "#fdae61", "#ca0020")) +
  theme_bw()
scatterplot

barplot <- ggplot(data = all_obs, mapping = aes(x = year, fill = avgt_bin)) +
  geom_bar(position = position_dodge()) +
  scale_fill_manual(values = c("#404040", "#fdae61", "#ca0020")) +
  theme_bw()
barplot

# Look at proportion of each category of temperatures for each year
avgt_proportions <- all_obs %>%
  group_by(year, avgt_bin) %>%
  summarize(bin_total = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total_obs = sum(bin_total)) %>%
  ungroup() %>%
  mutate(prop_bin = bin_total/total_obs) %>%
  select(year, avgt_bin, prop_bin)

prop_plot <- ggplot(data = avgt_proportions, mapping = aes(x = year, 
                                                           y = prop_bin,
                                                           fill = avgt_bin)) +
  geom_col() +
  scale_fill_manual(values = c("#404040", "#fdae61", "#ca0020")) +
  theme_bw()
prop_plot

################################################################################
# For each species, see avgt values
