# Run models on samples inside a 90-98% envelope, controlling for GDD
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-03

require(dplyr)
require(ggplot2)
require(sf)        # Filtering points in polygon
require(lubridate) # Julian days
require(ks)        # Estimating density envelope
require(terra)     # Extracting growing degree days data

# Set up filter values
min_year <- 2000
max_year <- 2022
# If true, will exclude observations after June 21 of each year
pre_summer <- TRUE
# Cutoff for inclusion in density envelope
density_cutoff <- 0.95

# Load in insect data
insect <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")
insect$organism = "insect"

# Load data for two host plants
host_concatenata <- read.csv(file = "data/cardamine_concatenata-gbif-clean.csv")
host_diphylla <- read.csv(file = "data/cardamine_diphylla-gbif-clean.csv")

# Combine hosts and add label for organism (insect vs host)
hosts <- host_concatenata %>%
  bind_rows(host_diphylla) %>%
  mutate(organism = "host")

# Combine insect and hosts
all_obs <- insect %>%
  bind_rows(hosts) %>%
  mutate(julian_day = yday(as.Date(paste(year, month, day, sep = "-")))) %>%
  mutate(organism = factor(organism, levels = c("insect", "host")))

# Run filtering for years and seasons as appropriate
all_obs <- all_obs %>%
  filter(year <= max_year) %>%
  filter(year >= min_year)

if (pre_summer) {
  all_obs <- all_obs %>%
    filter(month <= 6) %>% # Drop any after June
    filter(month < 6 | day < 22) # Drop any after June 21
}

# Set Pieris virginiensis and insect as reference levels
all_obs <- all_obs %>%
  mutate(organism = factor(organism, levels = c("insect", "host"))) %>%
  mutate(species = factor(species, levels = c("Pieris virginiensis",
                                              "Cardamine concatenata",
                                              "Cardamine diphylla")))

################################################################################
# Density envelope
# Determine density envelope for insect observations
insect_matrix <- all_obs %>%
  filter(organism == "insect") %>%
  dplyr::select(longitude, latitude) %>%
  as.matrix()

# Hns & Hbcv pretty similar to one another, Hns much faster
# bandwidth <- ks::Hns(insect_matrix)
# bandwidth <- ks::Hbcv(insect_matrix)
# Hscv and Hpi nearly identical
# bandwidth <- ks::Hscv(insect_matrix)
bandwidth <- ks::Hpi(insect_matrix)
kd_estimate <- ks::kde(x = insect_matrix, H = bandwidth)

# We have the density envelope, and now predict it for ALL observation points, 
# insect and hosts
all_matrix <- all_obs %>%
  dplyr::select(longitude, latitude) %>%
  as.matrix()
kd_predicted <- predict(object = kd_estimate, x = all_matrix)

# Vector indicating if points are included or excluded, based on our envelope
# cutoff. Values are stored as text with a percentage sign, hence gymnastics
min_density <- (1 - density_cutoff) * 100
within_envelope <- kd_predicted >= kd_estimate$cont[paste0(min_density, "%")]

# Create new data object with only those observations (insect and host) that 
# fall within the desired density envelope of the insect
envelope_obs <- all_obs[within_envelope, ]

# Reality check
# # Plot all observations
# plot(latitude ~ longitude, data = all_obs,
#      col = "gray80", pch = 19, cex = 0.5, las = 1, xlab = "", ylab = "")
# # Add just the ones in the envelope as darker
# points(latitude ~ longitude, data = envelope_obs,
#        col = "gray60", pch = 19, cex = 0.5)
# # Add in insect points as even darker
# points(latitude ~ longitude, data = all_obs %>% filter(organism == "insect"),
#        col = "gray20", pch = 19,
#        cex = 0.5, las = 1, xlab = "", ylab = "")
# # Draw density envelope
# plot(kd_estimate, cont = (density_cutoff * 100),
#      drawlabels = FALSE, col = "blue", add = TRUE)

################################################################################
# Growing degree days
# Load in growing degree days data
# Growing degree days from https://climatena.ca/spatialData
gdd <- terra::rast(x = "data/DD5.tif")

# Extract relevant data for observations
envelope_obs$gdd <- terra::extract(x = gdd,
                                   y = envelope_obs[, c("longitude", "latitude")])[, 2]

################################################################################
# Analyses

# Run simple linear model
# julian_day ~ year
model_1 <- lm(julian_day ~ year,
              data = envelope_obs)
summary(model_1)

# Run polynomial model
# julian_day ~ year + year^2
model_2 <- lm(julian_day ~ poly(year, degree = 2),
              data = envelope_obs)
summary(model_2)

# At this point, may need to decide on simple vs. polynomial
# Also, consider weighted least squares...

# Run linear model with different effect for insect & hosts
# julian_day ~ year + species + year x species
model_3 <- lm(julian_day ~ year * species,
              data = envelope_obs)
summary(model_3)

# Run linear model with different effect for insects & hosts, 
# controlling for gdd
# julian_day ~ year + species + year x species + gdd
model_4 <- lm(julian_day ~ year * species + gdd,
              data = envelope_obs)
summary(model_4)

# Run linear model with different effect for insects & hosts, 
# and adding in gdd as interaction
# julian_day ~ year + species + gdd + year x species + gdd x species
model_5 <- lm(julian_day ~ year * species + gdd * species,
              data = envelope_obs)
summary(model_5)

# Run linear model with three-way interaction YUCK
# julian_day ~ year + species + gdd + year x species + gdd x species
model_6 <- lm(julian_day ~ year * gdd * species,
              data = envelope_obs)
summary(model_6)

# Look at some predictions, for gdd at 25, 50, and 75
gdd_quantiles <- stats::quantile(x = envelope_obs$gdd[envelope_obs$organism == "insect"],
                                 probs = c(0.25, 0.5, 0.75))

# Create an empty data frame to hold values we want to make predictions for
empty_newdata <- data.frame(year = numeric(0),
                      species = character(0),
                      gdd = numeric(0))
# Make the data frame "complete"
newdata <- empty_newdata %>%
  tidyr::expand(year = unique(envelope_obs$year),
                species = unique(envelope_obs$species),
                gdd = gdd_quantiles)

# Use desired model to make predictions
newdata$julian_day <- predict(object = model_6, newdata = newdata)

# Plot predicted lines
ggplot(data = newdata, mapping = aes(x = year, y = julian_day,
                                     color = species)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c("#7b3294", "#a6dba0","#008837")) +
  facet_wrap(~ gdd) +
  theme_bw() +
  labs(x = "Year", y = "Julian day")
