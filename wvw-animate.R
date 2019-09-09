# Using gganimate on West Virginia White
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-08-23

rm(list = ls())

# Trying to visualize changes in yday and latitude over years
# See https://gganimate.com/ specifically "Yet Another Example"
source(file = "scripts/global_values.R")
globals <- global_values()
library(tidyverse)
library(lubridate)
library(gganimate) # via devtools
library(transformr) # required for animating density plots
# install.packages("gifski") # Required for animation

species <- data.frame(genus = c("Pieris", "Cardamine", "Cardamine"),
                      species = c("virginiensis", "concatenata", "diphylla"),
                      category = c("insect", "host", "host"),
                      stringsAsFactors = FALSE)

# Iterate over each species and add to all_obs data frame
all_obs <- NULL
for (i in 1:nrow(species)) {
  genus_name <- species$genus[i]
  species_name <- species$species[i]
  category <- species$category[i]
  
  data_file <- paste0("data/", genus_name, "-", species_name, "-spocc.csv")

  observations <- read_csv(file = data_file)
  
  # Restrict to the days and years of interest
  observations <- observations %>%
    filter(!is.na(date),
           yday(x = date) >= globals$min_julian,
           yday(x = date) <= globals$max_julian,
           year(x = date) >= globals$min_year,
           year(x = date) <= globals$max_year)
  
  year_counts <- observations %>%  
    group_by(year = year(date)) %>%
    summarize(obs_count = n()) %>%
    filter(obs_count >= 10) # globals$minimum_required
  
  # Filter obserations to only include those years with enough samples
  observations <- observations %>%
    filter(year(date) %in% year_counts$year)
  
  # Add genus and species columns to the data
  observations$genus_name <- genus_name
  observations$species_name <- species_name
  observations$category <- category
  observations$binomial <- paste0(genus_name, " ", species_name)
  
  if (is.null(all_obs)){
    all_obs <- observations
  } else {
    all_obs <- bind_rows(observations, 
                         all_obs)
  }
  rm(observations)
}

# Add columns for yday and year
all_obs <- all_obs %>%
  mutate(yday = as.integer(yday(date)),
         year = as.integer(year(date)))


# Plot data
obs_plot <- ggplot(data = all_obs, 
                   mapping = aes(x = latitude, y = yday)) +
  geom_point(mapping = aes(color = category, shape = category)) +
  # gganimate
  labs(title = "Year: {frame_time}", x = "Latitude", y = "Julian Day") +
  transition_time(year) +
  ease_aes("linear")
print(obs_plot)

# Add density plots
# Probably should avoid these, as the animation creates artifacts when switching
# from one year to the next (especially when there are years with missing data)
# Troublesome years
remove_years <- c(1968, 1972, 1983, 2003)
good_year_obs <- all_obs[!(all_obs$year %in% remove_years), ]

density_plot <- ggplot(data = good_year_obs[good_year_obs$category == "insect", ], 
                   mapping = aes(x = latitude, y = yday)) +
  geom_point(alpha = 0.5) +
  geom_density2d() +
  # gganimate
  labs(title = "Year: {frame_time}", x = "Latitude", y = "Julian Day") +
  transition_time(year) +
  ease_aes("linear")
print(density_plot)
