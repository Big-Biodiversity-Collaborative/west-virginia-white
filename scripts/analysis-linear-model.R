# Linear regression analysis for yearly change in julian day
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-16

library(dplyr)     # data wrangling
library(lubridate) # Julian day calculations
library(ggplot2)   # plotting
library(terra)     # adding growing degree days data
library(tidyr)     # expand() for predicted data.frame
library(lmtest)    # test for heteroskedasticity
library(ggpubr)    # ggarrange() for multi-panel figure

################################################################################
# Data preparation
# Read in the cleaned observations
all_obs <- read.csv(file = "data/filtered-obs.csv")

# Calculate Julian day
all_obs <- all_obs %>%
  mutate(julian_day = yday(as.Date(paste(year, month, day, sep = "-"))))

# Set leveling so the insect is always reference group
all_obs$species <- factor(x = all_obs$species,
                          levels = c("Pieris virginiensis",
                                     "Cardamine concatenata",
                                     "Cardamine diphylla",
                                     "Borodinia laevigata"))
all_obs$organism <- factor(x = all_obs$organism,
                           levels = c("insect", "host"))

# Add growing degree days data from https://climatena.ca/spatialData
gdd <- terra::rast(x = "data/DD5_1991-2020.tif")

# Extract relevant data for observations
all_obs$gdd <- terra::extract(x = gdd,
                              y = all_obs[, c("longitude", "latitude")])[, 2]


################################################################################
# Analyses
# Run linear regression, no interaction
simple_lm <- lm(julian_day ~ gdd + year + species, 
                data = all_obs)

# Add the interaction between year and species
yearXspecies_lm <- lm(julian_day ~ gdd + year * species,
                      data = all_obs)

# Check to see if this is a better model
anova(simple_lm, yearXspecies_lm)

ygddXspecies <- lm(julian_day ~ gdd + year + species + 
                     gdd * species + year * species +
                     gdd * year * species,
                   data = all_obs)
# summary(ygddXspecies)
anova(yearXspecies_lm, ygddXspecies)

# Now make it three-way interaction among gdd, year, and species
# Only really adding the gdd * year term
three_X_lm <- lm(julian_day ~ gdd * year * species,
                 data = all_obs)

# Check to see if this is a better model than the gdd X year X species model
anova(ygddXspecies, three_X_lm)

# Three-way interaction model sans gdd * year is best; test for 
# heteroskedasticity
lmtest::bptest(ygddXspecies)
# studentized Breusch-Pagan test
# data:  ygddXspecies
# BP = 320.93, df = 15, p-value < 2.2e-16

# Update the model with residuals-based weights (WLS) (observations with lower 
# deviation from predicted values in OLS are given more weight)
resid_lm <- lm(abs(ygddXspecies$residuals) ~ ygddXspecies$fitted.values)
var_wts <- 1 / (resid_lm$fitted.values^2)
ygddXspecies_wls <- lm(julian_day ~ gdd + year + species + 
                    gdd * species + year * species +
                    gdd * year * species,
                  data = all_obs,
                  weights = var_wts)
# summary(ygddXspecies)
# summary(ygddXspecies_wls)

################################################################################
# Effect summary
# TODO: Need to extract numbers & do the math

################################################################################
# Plot responses
# Want three sub-plots, one for low GDD, medium GDD, high GDD
# Prior figures use 33% and 66% as cutoffs. Here we will use 33%/2, 50%, and 
# (66% + 100%)/2 as the points to use for our predicted values
gdd_points <- stats::quantile(x = all_obs$gdd[all_obs$organism == "insect"],
                       probs = c(1/6, 1/2, 5/6))

# Create an empty data frame to hold values we want to make predictions for
empty_newdata <- data.frame(year = numeric(0),
                            species = character(0),
                            gdd = numeric(0))

# Make the data frame "complete"
newdata <- empty_newdata %>%
  tidyr::expand(year = unique(all_obs$year),
                species = unique(all_obs$species),
                gdd = gdd_points)

# Use desired model to make predictions
newdata$julian_day <- predict(object = ygddXspecies_wls, 
                              newdata = newdata)
# Plot predicted lines
# Low GDD
low_gdd_prediction <- ggplot(data = newdata %>% filter(gdd == gdd_points[1]), 
                             mapping = aes(x = year, 
                                           y = julian_day,
                                           color = species)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c("#7b3294", "#a6dba0","#008837", "#5aae61"),
                     name = "Species") +
  theme_bw() +
  labs(title = "Low GDD", x = "Year", y = "Julian day")
low_gdd_prediction
ggsave(filename = "output/figure-3a.png",
       plot = low_gdd_prediction)

# Medium GDD
medium_gdd_prediction <- ggplot(data = newdata %>% filter(gdd == gdd_points[2]), 
                             mapping = aes(x = year, 
                                           y = julian_day,
                                           color = species)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c("#7b3294", "#a6dba0","#008837", "#5aae61"),
                     name = "Species") +
  theme_bw() +
  labs(title = "Medium GDD", x = "Year", y = "Julian day")
medium_gdd_prediction
ggsave(filename = "output/figure-3b.png",
       plot = medium_gdd_prediction)

# High GDD
high_gdd_prediction <- ggplot(data = newdata %>% filter(gdd == gdd_points[3]), 
                                mapping = aes(x = year, 
                                              y = julian_day,
                                              color = species)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c("#7b3294", "#a6dba0","#008837", "#5aae61"),
                     name = "Species") +
  theme_bw() +
  labs(title = "High GDD", x = "Year", y = "Julian day")
high_gdd_prediction
ggsave(filename = "output/figure-3c.png",
       plot = high_gdd_prediction)

# Single, multi-panel figure
multi_panel <- ggpubr::ggarrange(low_gdd_prediction, 
                                 medium_gdd_prediction, 
                                 high_gdd_prediction,
                                 ncol = 1)
multi_panel
