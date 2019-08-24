# Trying support vector machine to define envelopes for host and insect
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
library(e1071)

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
    filter(obs_count >= globals$minimum_required)
  
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

# Just look at 2018
obs_2018 <- all_obs[all_obs$year == 2018, ]
wvw_2018 <- obs_2018[obs_2018$category == "insect", ]

# Create background points by limits defined by latitude and yday of all 
# insect observations
yday_background <- runif(n = 10 * nrow(wvw_2018),
                         min = min(all_obs$yday),
                         max = max(all_obs$yday))
latitude_background <- runif(n = 10 * nrow(wvw_2018),
                             min = min(all_obs$latitude),
                             max = max(all_obs$latitude))
background <- data.frame(yday = yday_background,
                         latitude = latitude_background,
                         presence = 0)
wvw_data <- data.frame(yday = wvw_2018$yday,
                       latitude = wvw_2018$latitude,
                       presence = 1)
all_data <- rbind(wvw_data, background)
all_data$presence <- factor(all_data$presence)

# Reality check
ggplot(data = all_data, mapping = aes(x = latitude, y = yday, color = presence)) + 
  geom_point()

set.seed(20190823)
train <- sample(x = nrow(all_data), size = round(x = nrow(all_data)/2))
svmfit <- svm(presence ~ ., 
              data = all_data[train, ], 
              kernel = "radial",
              gamma = 1,
              cost = 1e3)
plot(svmfit, all_data[train, ])
summary(svmfit)

# Do some cross-validation to get best cost and gamma values
tune.out <- tune(svm, presence ~ ., data = all_data[train, ],
                 kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 1e1, 1e2, 1e3, 1e4, 1e5),
                               gamma = c(0.5, 1, 2, 3, 4, 5)))
summary(tune.out)

# Best parameters were cost = 1e5, gamma = 5
# Boundaries, though, so re-test (almost certainly over-fit)
svmfit <- svm(presence ~ ., 
              data = all_data[train, ], 
              kernel = "radial",
              gamma = 5,
              cost = 1e5)
plot(svmfit, all_data[train, ])
summary(svmfit)
