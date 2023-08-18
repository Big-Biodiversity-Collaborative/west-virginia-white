# Ridge plots of observations for midpoint temperature bins
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-07-18

library(dplyr)
library(ggplot2)
library(ggridges)  # Ridge plots
library(tidyr)
library(ggpubr)    # Single, multi-pane plot

# Read in filtered data
all_obs <- read.csv(file = "data/filtered-obs.csv")

# Only include two species of Cardamine as hosts
all_obs <- all_obs %>%
  filter(species %in% c("Pieris virginiensis",
                        "Cardamine concatenata",
                        "Cardamine diphylla"))

# Need to set insect as reference level
all_obs <- all_obs %>%
  mutate(organism = factor(organism, levels = c("insect", "host"))) %>%
  mutate(species = factor(species, levels = c("Pieris virginiensis",
                                              "Cardamine diphylla",
                                              "Cardamine concatenata")))

# The number of observations in a year for a tmid bin (see ridge plots, below) 
# to be considered for inclusion in the ridge plots
min_obs <- 10

################################################################################
# Temperature data
temperature <- read.csv(file = "data/temperature-obs.csv")
all_obs <- all_obs %>%
  left_join(temperature, by = "gbifID")

# Since models will include temperature, drop any observations that are missing 
# temperature data and calculate the midpoint temperature (mean of tmin and 
# tmax)
all_obs <- all_obs %>%
  filter(!is.na(tmin) & !is.na(tmax)) %>%
  mutate(tmid = (tmin + tmax)/2) # mean function doesn't work

# Late summer and fall observations are not relevant
all_obs <- all_obs %>%
  filter(month <= 7)

################################################################################
# Data preparation for ridge plots
# Three bins for three different ridge plots
tmid_bins <- stats::quantile(x = all_obs$tmid[all_obs$organism == "insect"],
                                 probs = c(1/3, 2/3))

# Set bin assignment for each row
all_obs <- all_obs %>%
  mutate(tmid_bin = case_when(tmid < tmid_bins[1] ~ "low",
                             tmid < tmid_bins[2] ~ "medium",
                             tmid >= tmid_bins[2] ~ "high",
                             TRUE ~ NA_character_))

# Use min_obs to filter to only those years with enough observations for both 
# insect and host
tmid_include <- all_obs %>%
  group_by(tmid_bin, year, organism) %>%
  summarize(count = n()) %>% # count observations for each year
  ungroup() %>%
  mutate(enough = count >= min_obs) %>% # year/organism rows with enough
  pivot_wider(id_cols = c(tmid_bin, year), 
              names_from = organism, 
              values_from = enough) %>%
  filter(!is.na(host)) %>% # drop any years that had 0 host records
  filter(!is.na(insect)) %>% # drop any years that had 0 insect records
  mutate(include = insect & host) %>% # both insect & host have >= min_obs
  filter(include)

# Create three datasets
low_tmid <- all_obs %>%
  filter(tmid_bin == "low") %>%
  filter(year %in% tmid_include$year)

medium_tmid <- all_obs %>%
  filter(tmid_bin == "medium") %>%
  filter(year %in% tmid_include$year)

high_tmid <- all_obs %>%
  filter(tmid_bin == "high") %>%
  filter(year %in% tmid_include$year)

################################################################################
# Ridge plots
# Make three different ridgeline plots
# Since we are limiting the x-axis a priori, may get the following warnings:
# Removed 24 rows containing non-finite values (stat_density_ridges)
low_tmid_plot <- low_tmid %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  xlim(c(50, NA)) +
  theme_bw()

# Add title only to version that is standalone  
low_tmid_single <- low_tmid_plot +
  labs(title = "Low Temperature", x = "Julian day", y = "Year")

low_tmid_single
ggsave(filename = "output/figure-2a.png",
       plot = low_tmid_single)

medium_tmid_plot <- medium_tmid %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  xlim(c(50, NA)) +
  theme_bw()

medium_tmid_single <- medium_tmid_plot +
  labs(title = "Medium Temperature", x = "Julian day", y = "Year")

medium_tmid_single
ggsave(filename = "output/figure-2b.png",
       plot = medium_tmid_single)

high_tmid_plot <- high_tmid %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  xlim(c(50, NA)) +
  theme_bw()

high_tmid_single <- high_tmid_plot +
  labs(title = "High Temperature", x = "Julian day", y = "Year")

high_tmid_single
ggsave(filename = "output/figure-2c.png",
       plot = high_tmid_single)

# Multi-panel plot of the three ridge plots
# To align with figure three, low is at top
tmid_labels <- c(low = "Low temperature", 
                 medium = "Medium temperature", 
                 high = "High temperature")

all_tmid_plot <- all_obs %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  mutate(tmid_bin = factor(tmid_bin, levels = c("low", "medium", "high"))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.75, size = 0.1) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  xlim(c(50, NA)) +
  labs(x = "Julian day", y = "Year") +
  theme_bw() +
  facet_wrap(~ tmid_bin, ncol = 3, scales = "free_x",
             labeller = labeller(tmid_bin = tmid_labels))

all_tmid_plot
ggsave(filename = "output/figure-2.png",
       plot = all_tmid_plot,
       height = 4,
       width = 6.5,
       units = "in")
