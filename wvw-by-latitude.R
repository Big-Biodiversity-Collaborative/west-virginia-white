# Look at trends of WVW at different latitudes
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-02-22

library(dplyr)
library(lubridate)
library(ggplot2)

# Earliest year of data to include; 
#     + change to run on different set of dates
#     + set to NULL to run on all dates
min_year <- 2000 # NULL

# Read in data & filter as appropriate
insect <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")
if (!is.null(min_year)) {
  insect <- insect %>%
    filter(year >= min_year)
}

# Calculate Julian day
insect <- insect %>%
  mutate(julian_day = yday(as.Date(paste(year, month, day, sep = "-"))))

lat_percentiles <- quantile(x = insect$latitude)

# Categorize into each quantile
insect <- insect %>%
  mutate(lat_quantile = case_when(latitude <= lat_percentiles[2] ~ 1,
                                  latitude <= lat_percentiles[3] ~ 2,
                                  latitude <= lat_percentiles[4] ~ 3,
                                  latitude <= lat_percentiles[5] ~ 4)) %>%
  mutate(lat_quantile = factor(lat_quantile))

# Do a box plot, grouping by quantile
insect_latitude <- ggplot(data = insect,
                          mapping = aes(x = year,
                                        y = julian_day,
                                        group = year,
                                        color = lat_quantile)) +
  geom_boxplot() +
  scale_color_discrete(name = "Latitude quantile") +
  xlab(label = "Year") +
  ylab(label = "Julian day") +
  theme_bw() +
  facet_wrap(~ lat_quantile, ncol = 1, scales = "free_y")
insect_latitude
