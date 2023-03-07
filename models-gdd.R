# Run models on samples inside a 90-98% envelope, controlling for GDD
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-03

require(dplyr)
require(ggplot2)
require(sf)        # Filtering points in polygon
require(lubridate) # Julian days
require(terra)     # Extracting growing degree days data
require(ggridges)  # Ridge plots

# Read in filtered data
all_obs <- read.csv(file = "data/filtered-obs.csv")

# Need to set insect as reference level
all_obs <- all_obs %>%
  mutate(organism = factor(organism, levels = c("insect", "host"))) %>%
  mutate(species = factor(species, levels = c("Pieris virginiensis",
                                              "Cardamine concatenata",
                                              "Cardamine diphylla")))

################################################################################
# Growing degree days
# Load in growing degree days data
# Growing degree days from https://climatena.ca/spatialData
gdd <- terra::rast(x = "data/DD5.tif")

# Extract relevant data for observations
all_obs$gdd <- terra::extract(x = gdd,
                              y = all_obs[, c("longitude", "latitude")])[, 2]

################################################################################
# Analyses

# Run simple linear model
# julian_day ~ year
model_1 <- lm(julian_day ~ year,
              data = all_obs)
summary(model_1)

# Run polynomial model
# julian_day ~ year + year^2
model_2 <- lm(julian_day ~ poly(year, degree = 2),
              data = all_obs)
summary(model_2)

# At this point, may need to decide on simple vs. polynomial
# Also, consider weighted least squares...

# Run linear model with different effect for insect & hosts
# julian_day ~ year + species + year x species
model_3 <- lm(julian_day ~ year * species,
              data = all_obs)
summary(model_3)

# Run linear model with different effect for insects & hosts, 
# controlling for gdd
# julian_day ~ year + species + year x species + gdd
model_4 <- lm(julian_day ~ year * species + gdd,
              data = all_obs)
summary(model_4)

# Run linear model with different effect for insects & hosts, 
# and adding in gdd as interaction
# julian_day ~ year + species + gdd + year x species + gdd x species
model_5 <- lm(julian_day ~ year * species + gdd * species,
              data = all_obs)
summary(model_5)

# Run linear model with three-way interaction YUCK
# julian_day ~ year + species + gdd + year x species + gdd x species
model_6 <- lm(julian_day ~ year * gdd * species,
              data = all_obs)
summary(model_6)

################################################################################
# Model visualization

# Look at some predictions, for gdd at 25, 50, and 75
gdd_quantiles <- stats::quantile(x = all_obs$gdd[all_obs$organism == "insect"],
                                 probs = c(0.25, 0.5, 0.75))

# Create an empty data frame to hold values we want to make predictions for
empty_newdata <- data.frame(year = numeric(0),
                      species = character(0),
                      gdd = numeric(0))
# Make the data frame "complete"
newdata <- empty_newdata %>%
  tidyr::expand(year = unique(all_obs$year),
                species = unique(all_obs$species),
                gdd = gdd_quantiles)

# Use desired model to make predictions
newdata$julian_day <- predict(object = model_6, newdata = newdata)

# Plot predicted lines
ggplot(data = newdata, mapping = aes(x = year, y = julian_day,
                                     color = species)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c("#7b3294", "#a6dba0","#008837"),
                     name = "Species") +
  facet_wrap(~ gdd) +
  theme_bw() +
  labs(x = "Year", y = "Julian day")

################################################################################
# Ridge plots
# Three bins for three different ridge plots
gdd_bins <- stats::quantile(x = all_obs$gdd[all_obs$organism == "insect"],
                                 probs = c(1/3, 2/3))

# TODO: Need to do some filtering for those years where we have enough data to 
# plot densities for both hosts and insect

# Create three data subsets
low_gdd <- all_obs %>%
  filter(gdd < gdd_bins[1])

med_gdd <- all_obs %>%
  filter(gdd >= gdd_bins[1]) %>%
  filter(gdd < gdd_bins[2])

high_gdd <- all_obs %>%
  filter(gdd >= gdd_bins[2])

# Make three different ridgeline plots
low_gdd %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  xlim(c(50, NA)) +
  theme_bw() +
  ylab("Year") +
  xlab("Julian day") +
  ggtitle("Low GDD")

med_gdd %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  xlim(c(50, NA)) +
  theme_bw() +
  ylab("Year") +
  xlab("Julian day") +
  ggtitle("Medium GDD")

high_gdd %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  xlim(c(50, NA)) +
  theme_bw() +
  ylab("Year") +
  xlab("Julian day") +
  ggtitle("High GDD")
