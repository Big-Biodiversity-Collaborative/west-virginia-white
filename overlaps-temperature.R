# Investigate amount of overlap through time
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-02-23

# TODO: A placeholder. Currently just a copy of the latitude approach.

# Want to see how much (prop/perc) of West Virginia White observations overlap 
# with native host plants.

require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)
require(terra)

# Number of latitudinal "bands"
# TODO: still some hard-coding below based on this value; currently only 
# accommodates values of 2 or 3
num_bands <- 3

# TODO: Based on Davis & Cipollini, could make a case for doing at split at 
# 40 degrees latitude...

# Minimum number of insect observations per band necessary to be included; note
# exclusion works at the year/band level, so in some years, one band may be 
# excluded, while other bands are retained
min_per_band <- 5

# Instead of latitude, use temperature bins as proxy of isoclines. Mean annual 
# temperature for North America from https://climatena.ca/spatialData
# Note these data are 10 x C
temps <- terra::rast(x = "data/MAT.tif")
temps <- temps * 0.1

# Convert to Fahrenheit?
# temps <- (temps * 9/5) + 32

# Now do another multiplication to get 10 degree "bins"
# First convert to 0.1 x C
temps_binned <- temps * 0.1
# Use floor to round down to nearest integer
temps_binned <- terra::app(temps_binned, floor)
# Multiply by 10 to get back to actual degrees C
temps_binned <- temps_binned  * 10
# plot(temps_binned, xlim = c(-100, -70), ylim = c(30, 50))
# plot(temps, xlim = c(-100, -70), ylim = c(30, 50))

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
    filter(month > 6 | day < 22) # Drop any after June 21
}

# Add mean annual temperature information to insect data frame
extracted_temps <- terra::extract(x = temps_binned, 
                                  y = insect[, c("longitude", "latitude")])
insect$mat <- extracted_temps[, 2]

# Determine latitude bands (1/4, 1/2, 3/4, 4/4) or (1/3, 2/3, 3/3)
lat_quantiles <- (1:(num_bands - 1))/num_bands
lat_bands <- quantile(x = insect$latitude, probs = lat_quantiles)
# lat_bands <- 40

# Indicate which band each observation is in
if (num_bands == 3) {
  insect <- insect %>%
    mutate(lat_band = case_when(latitude <= lat_bands[1] ~ 1,
                                latitude <= lat_bands[2] ~ 2,
                                TRUE ~ 3))
} else if (num_bands == 2) {
  insect <- insect %>%
    mutate(lat_band = case_when(latitude <= lat_bands[1] ~ 1,
                                TRUE ~ 2))
} else {
  warning("Number of bands must be 2 or 3")
}

# Calculate Julian day
insect <- insect %>%
  mutate(julian_day = yday(as.Date(paste(year, month, day, sep = "-"))))

# Calculate, for each year, each band 80% envelope (or 10, 90 percentiles)
insect_env <- insect %>%
  group_by(year, lat_band) %>%
  summarize(lower = quantile(julian_day, probs = 0.1)[1],
            upper = quantile(julian_day, probs = 0.9)[1],
            num_obs = n()) %>%
  filter(num_obs >= min_per_band) %>%
  ungroup()

# Some plotting to see if things are working...
ggplot(data = insect_env, mapping = aes(color = lat_band)) +
  geom_segment(mapping = aes(x = year, xend = year,
                             y = lower, yend = upper)) +
  ylab("Julian day") +
  xlab("Year") +
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
hosts <- hosts %>%
  filter(latitude >= min(insect$latitude)) %>%
  filter(latitude <= max(insect$latitude))

# Figure out which insect latitudinal band each host observation in is
if (num_bands == 3) {
  hosts <- hosts %>%
    mutate(lat_band = case_when(latitude <= lat_bands[1] ~ 1,
                                latitude <= lat_bands[2] ~ 2,
                                TRUE ~ 3))
} else if (num_bands == 2) {
  hosts <- hosts %>%
    mutate(lat_band = case_when(latitude <= lat_bands[1] ~ 1,
                                TRUE ~ 2))
} else {
  warning("Number of bands must be 2 or 3")
}

# Calculate the 80% envelopes for the plants
hosts_env <- hosts %>%
  group_by(year, lat_band) %>%
  summarize(lower = quantile(julian_day, probs = 0.1)[1],
            upper = quantile(julian_day, probs = 0.9)[1],
            num_obs = n()) %>%
  filter(num_obs >= min_per_band) %>%
  ungroup()

# Quick QA/QC
ggplot(data = hosts_env, mapping = aes(color = lat_band)) +
  geom_segment(mapping = aes(x = year, xend = year,
                             y = lower, yend = upper)) +
  ylab("Julian day") +
  xlab("Year") +
  theme_bw()

# Calculate for each year, each band, what proportion of the insect 80% falls 
# within the plants' 80%
# Start by joining in data for plant lower/upper values to insect data
insect_c <- insect %>%
  left_join(hosts_env %>% select(year, lat_band, lower, upper)) %>%
  mutate(position = case_when(julian_day < lower ~ "before",
                              julian_day <= upper ~ "within",
                              TRUE ~ "after")) %>%
  group_by(year, lat_band, position) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year, lat_band) %>%
  mutate(total_obs = sum(count)) %>%
  ungroup() %>%
  mutate(prop = count/total_obs) %>%
  mutate(lat_band = factor(lat_band))
# tail(insect_c)

# Plot the proportion of insects 80% by year for each latitude band
ggplot(data = insect_c %>% filter(position == "within"), 
       mapping = aes(x = year, y = prop, color = lat_band)) +
  geom_point() +
  geom_smooth() +
  ggtitle(label = "Overlapping envelopes")

# Plot the proportion of insects coming out after 80% plant
ggplot(data = insect_c %>% filter(position == "after"), 
       mapping = aes(x = year, y = prop, color = lat_band)) +
  geom_point() +
  geom_smooth() +
  ggtitle(label = "Prop. butterflies after plants")

# Plot the proportion of insects coming out before 80% plant
ggplot(data = insect_c %>% filter(position == "before"), 
       mapping = aes(x = year, y = prop, color = lat_band)) +
  geom_point() +
  geom_smooth() +
  ggtitle(label = "Prop. butterflies before plants")
