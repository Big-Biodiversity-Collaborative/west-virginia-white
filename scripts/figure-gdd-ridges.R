# Ridge plots of observations for growing degree day bins
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-03

require(dplyr)
require(ggplot2)
# require(sf)        # Filtering points in polygon
require(lubridate) # Julian days
require(terra)     # Extracting growing degree days data
require(ggridges)  # Ridge plots
require(tidyr)

# Read in filtered data
all_obs <- read.csv(file = "data/filtered-obs.csv")

# Need to set insect as reference level
all_obs <- all_obs %>%
  mutate(organism = factor(organism, levels = c("insect", "host"))) %>%
  mutate(species = factor(species, levels = c("Pieris virginiensis",
                                              "Cardamine concatenata",
                                              "Cardamine diphylla")))

# The number of observations in a year for a GDD bin (see ridge plots, below) 
# to be considered for inclusion in the ridge plots
min_obs <- 10

################################################################################
# Growing degree days from https://climatena.ca/spatialData
gdd <- terra::rast(x = "data/DD5_1991-2020.tif")

# Extract relevant data for observations
all_obs$gdd <- terra::extract(x = gdd,
                              y = all_obs[, c("longitude", "latitude")])[, 2]

################################################################################
# Data preparation for ridge plots
# Three bins for three different ridge plots
gdd_bins <- stats::quantile(x = all_obs$gdd[all_obs$organism == "insect"],
                                 probs = c(1/3, 2/3))

# Set bin assignment for each row
all_obs <- all_obs %>%
  mutate(gdd_bin = case_when(gdd < gdd_bins[1] ~ "low",
                             gdd < gdd_bins[2] ~ "medium",
                             gdd >= gdd_bins[2] ~ "high",
                             TRUE ~ NA_character_))

# Use min_obs to filter to only those years with enough observations for both 
# insect and host
# Low GDD
gdd_include <- all_obs %>%
  group_by(gdd_bin, year, organism) %>%
  summarize(count = n()) %>% # count observations for each year
  ungroup() %>%
  mutate(enough = count >= min_obs) %>% # year/organism rows with enough
  pivot_wider(id_cols = c(gdd_bin, year), 
              names_from = organism, 
              values_from = enough) %>%
  filter(!is.na(host)) %>% # drop any years that had 0 host records
  filter(!is.na(insect)) %>% # drop any years that had 0 insect records
  mutate(include = insect & host) %>% # both insect & host have >= min_obs
  filter(include)

# Create three datasets
low_gdd <- all_obs %>%
  filter(gdd_bin == "low") %>%
  filter(year %in% gdd_include$year)

medium_gdd <- all_obs %>%
  filter(gdd_bin == "medium") %>%
  filter(year %in% gdd_include$year)

high_gdd <- all_obs %>%
  filter(gdd_bin == "high") %>%
  filter(year %in% gdd_include$year)

################################################################################
# Ridge plots
# Make three different ridgeline plots
# Since we are limiting the x-axis a priori, may get the following warnings:
# Removed 24 rows containing non-finite values (stat_density_ridges)
low_gdd_plot <- low_gdd %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  xlim(c(50, NA)) +
  theme_bw() +
  ylab("Year") +
  xlab("Julian day") +
  ggtitle("Low GDD")
low_gdd_plot
ggsave(filename = "output/figure-2a.png",
       plot = low_gdd_plot)

medium_gdd_plot <- medium_gdd %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  xlim(c(50, NA)) +
  theme_bw() +
  ylab("Year") +
  xlab("Julian day") +
  ggtitle("Medium GDD")
medium_gdd_plot
ggsave(filename = "output/figure-2b.png",
       plot = medium_gdd_plot)

high_gdd_plot <- high_gdd %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  xlim(c(50, NA)) +
  theme_bw() +
  ylab("Year") +
  xlab("Julian day") +
  ggtitle("High GDD")
high_gdd_plot
ggsave(filename = "output/figure-2c.png",
       plot = high_gdd_plot)
