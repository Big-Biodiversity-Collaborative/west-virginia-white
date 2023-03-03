# Investigate amount of overlap through time controlling for growing degree days
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-01

# Want to see how much (prop/perc) of West Virginia White observations overlap 
# with native host plants.

require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)
require(terra)
require(RColorBrewer)

# Minimum number of insect observations per band necessary to be included; note
# exclusion works at the year/band level, so in some years, one band may be 
# excluded, while other bands are retained
min_per_band <- 5

# Growing degree days from https://climatena.ca/spatialData
gdd <- terra::rast(x = "data/DD5.tif")

# Now do multiplications to get "bins"
# mult_fac of 1 gets 1000 bins, mult_fac of 2 gets 500 degree bins
mult_fac <- 1 # 2

# First convert to smaller numbers for rounding down
gdd_binned <- gdd * (0.001 * mult_fac)
# Use floor to round down to nearest integer
gdd_binned <- terra::app(gdd_binned, floor)
# Multiply by 10 to get back to actual degrees C
gdd_binned <- gdd_binned  * (1000 / mult_fac)
# plot(gdd_binned, xlim = c(-100, -70), ylim = c(30, 50))
# plot(gdd, xlim = c(-100, -70), ylim = c(30, 50))

# Earliest year of data to include; 
#     + change to run on different set of dates
#     + set to NULL to run on all dates
min_year <- 2000 # NULL

# If true, will exclude all observations that occur after June 21
pre_summer <- TRUE

# Load data for insect
insect <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")
# Add label for organism (insect vs host)
insect$organism <- "insect"

# Filter by year if necessary
if (!is.null(min_year)) {
  insect <- insect %>%
    filter(year >= min_year)
}

# Restrict to pre-summer observations as appropriate
if (pre_summer) {
  insect <- insect %>%
    filter(month <= 6) %>% # Drop any after June
    filter(month < 6 | day < 22) # Drop any after June 21
}

# Add growing degree days information to insect data frame
insect_gdd <- terra::extract(x = gdd_binned, 
                               y = insect[, c("longitude", "latitude")])
insect$gdd_band <- insect_gdd[, 2]

# Add a more human-readable column
insect <- insect %>%
  mutate(gdd_band = factor(gdd_band))

# Calculate Julian day
insect <- insect %>%
  mutate(julian_day = yday(as.Date(paste(year, month, day, sep = "-"))))

# Calculate, for each year, each band 80% envelope (or 10, 90 percentiles)
insect_env <- insect %>%
  group_by(year, gdd_band) %>%
  summarize(lower = quantile(julian_day, probs = 0.1)[1],
            upper = quantile(julian_day, probs = 0.9)[1],
            num_obs = n()) %>%
  filter(num_obs >= min_per_band) %>%
  ungroup()

# band_colors <- brewer.pal(n = 6, "OrRd")[3:6]
# band_colors <- brewer.pal(n = 6, "PRGn")[c(1, 2, 5, 6)]
# band_colors <- brewer.pal(n = 6, "RdYlBu")[c(1, 2, 5, 6)]
band_colors <- brewer.pal(n = 6, "BuGn")[3:6]

# Some plotting to see if things are working...
ggplot(data = insect_env, mapping = aes(color = gdd_band)) +
  geom_segment(mapping = aes(x = year, xend = year,
                             y = lower, yend = upper),
               linewidth = 2) +
  ylab("Julian day") +
  xlab("Year") +
  # scale_color_brewer(palette = "BrBG") +
  scale_color_manual(values = band_colors) +
  theme_bw()

# Now do calculations for hosts (envelopes, etc)

# Load data for two host plants
host_concatenata <- read.csv(file = "data/cardamine_concatenata-gbif-clean.csv")
host_diphylla <- read.csv(file = "data/cardamine_diphylla-gbif-clean.csv")

# Combine hosts and add label for organism (insect vs host)
hosts <- host_concatenata %>%
  bind_rows(host_diphylla) %>%
  mutate(organism = "host")

if (!is.null(min_year)) {
  hosts <- hosts %>%
    filter(year >= min_year)
}

if (pre_summer) {
  hosts <- hosts %>%
    filter(month <= 6) %>% # Drop any after June
    filter(month > 6 | day < 22) # Drop any after June 21
}

# Calculate Julian day
hosts <- hosts %>%
  mutate(julian_day = yday(as.Date(paste(year, month, day, sep = "-"))))

# Filter out observations that are completely outside the latitudinal range of 
# the insect
# hosts <- hosts %>%
#   filter(latitude >= min(insect$latitude)) %>%
#   filter(latitude <= max(insect$latitude))

# Add temperature band to host
hosts_gdd <- terra::extract(x = gdd_binned, 
                              y = hosts[, c("longitude", "latitude")])
hosts$gdd_band <- hosts_gdd[, 2]

# Add a more human-readable column
hosts <- hosts %>%
  mutate(gdd_band = factor(gdd_band))

# Filter out observations that are outside the temperature bands of the insect
hosts <- hosts %>%
  filter(gdd_band %in% unique(insect$gdd_band))

# Calculate the 80% envelopes for the plants
hosts_env <- hosts %>%
  group_by(year, gdd_band) %>%
  summarize(lower = quantile(julian_day, probs = 0.1)[1],
            upper = quantile(julian_day, probs = 0.9)[1],
            num_obs = n()) %>%
  filter(num_obs >= min_per_band) %>%
  ungroup()

# Quick QA/QC
ggplot(data = hosts_env, mapping = aes(color = gdd_band)) +
  geom_segment(mapping = aes(x = year, xend = year,
                             y = lower, yend = upper),
               linewidth = 2) +
  ylab("Julian day") +
  xlab("Year") +
  scale_color_manual(values = band_colors) +
  theme_bw()

# Calculate for each year, each band, what proportion of the insect 80% falls 
# within the plants' 80%
# Start by joining in data for plant lower/upper values to insect data
insect_c <- insect %>%
  left_join(hosts_env %>% select(year, gdd_band, lower, upper)) %>%
  mutate(position = case_when(julian_day < lower ~ "before",
                              julian_day <= upper ~ "within",
                              TRUE ~ "after")) %>%
  group_by(year, gdd_band, position) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year, gdd_band) %>%
  mutate(total_obs = sum(count)) %>%
  ungroup() %>%
  mutate(prop = count/total_obs)
# tail(insect_c)

# Plot the proportion of insects 80% by year for each latitude band
ggplot(data = insect_c %>% filter(position == "within"), 
       mapping = aes(x = year, y = prop, color = gdd_band)) +
  geom_point() +
  geom_smooth() +
  ggtitle(label = "Overlapping envelopes")

# Plot the proportion of insects coming out after 80% plant
ggplot(data = insect_c %>% filter(position == "after"), 
       mapping = aes(x = year, y = prop, color = gdd_band)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(label = "Prop. butterflies after plants")

# Plot the proportion of insects coming out before 80% plant
ggplot(data = insect_c %>% filter(position == "before"), 
       mapping = aes(x = year, y = prop, color = gdd_band)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(label = "Prop. butterflies before plants")

# Do a boxplot
all_obs <- insect %>%
  bind_rows(hosts)
ggplot(data = all_obs, mapping = aes(x = year, 
                                   y = julian_day, 
                                   group = interaction(year, organism),
                                   fill = organism,
                                   color = organism)) + 
  geom_boxplot(outlier.shape = NA, coef = 0) + 
  facet_wrap(~ gdd_band, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("#a6dba0", "#c2a5cf")) +
  scale_color_manual(values = c("#008837", "#7b3294")) +
  theme_bw()
