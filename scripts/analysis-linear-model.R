# Linear regression analysis for yearly change in julian day
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-16

library(dplyr)     # data wrangling
library(lubridate) # Julian day calculations
library(terra)     # adding growing degree days data
library(tidyr)     # expand() for predicted data.frame
library(lmtest)    # test for heteroskedasticity
library(broom)     # cleaning up stats output
library(ggplot2)   # plotting
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
simple_lm <- lm(julian_day ~ year + gdd + species, 
                data = all_obs)

# Add the interaction between year and species
yearXspecies_lm <- lm(julian_day ~ gdd + year * species,
                      data = all_obs)

# Check to see if this is a better model
anova(simple_lm, yearXspecies_lm)

# Add the interaction between gdd and species (species-specific responses to 
# local temperatures)
year_gddXspecies_lm <- lm(julian_day ~ year * species + gdd * species,
                          data = all_obs)
# summary(year_gddXspecies_lm)
anova(yearXspecies_lm, year_gddXspecies_lm)

# Now add the last two-way interaction between year and gdd, allowing 
# gdd-specific responses to year
all_2way_lm <- lm(julian_day ~ (year + gdd + species)^2,
                  data = all_obs)
# summary(all_2way_lm)
anova(year_gddXspecies_lm, all_2way_lm)

# We can test the full model, although somewhat difficult to justify the three-
# way interaction term...
full_model <- lm(julian_day ~ year * gdd * species,
                 data = all_obs)
# summary(full_model)
anova(all_2way_lm, full_model)

# And yet, it fits very well...test for heteroskedasticity
best_model <- full_model

lmtest::bptest(best_model)
# studentized Breusch-Pagan test
# data:  best_model
# BP = 316.14, df = 15, p-value < 2.2e-16

# Update the model with residuals-based weights (WLS) (observations with lower 
# deviation from predicted values in OLS are given more weight)
resid_lm <- lm(abs(best_model$residuals) ~ best_model$fitted.values)
var_wts <- 1 / (resid_lm$fitted.values^2)
# Update lm call with whichever model was identified as best
best_model_wls <- update(best_model,
                         data = all_obs,
                         weights = var_wts)
# summary(best_model_wls)
lmtest::bptest(best_model_wls)
# studentized Breusch-Pagan test
# 
# data:  ygddXspecies_wls
# The test statistic is chi-squared
# BP = 12.591, df = 15, p-value = 0.6338

################################################################################
# Effect summary
model_results <- broom::tidy(best_model_wls)

# Clean up the table for human eyes
model_table <- model_results %>%
  mutate(term = gsub(x = term, pattern = "gdd", replacement = "GDD")) %>%
  mutate(term = gsub(x = term, pattern = "year", replacement = "Year")) %>%
  mutate(term = gsub(x = term, pattern = "species", replacement = "")) %>%
  mutate(term = gsub(x = term, pattern = "Year:", replacement = "Year x ")) %>%
  mutate(term = gsub(x = term, pattern = "GDD:", replacement = "GDD x ")) %>%
  mutate(term = gsub(x = term, pattern = "(Intercept)", replacement = "Intercept"))

# Do not need such precision!
model_table <- model_table %>%
  mutate(across(.cols = estimate:p.value, .fns = signif, digits = 3))

# Write to a file
write.csv(file = "output/model-table.csv",
          x = model_table,
          row.names = FALSE)

# TODO: Math here
# How many days/year earlier for:
#    + each species
#    + each GDD (will be an "average" for each of three categories)

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
newdata$julian_day <- predict(object = best_model_wls, 
                              newdata = newdata)
# Plot predicted lines
# Want to have same Julian day scale on each plot
jd_limits <- c(min(newdata$julian_day), max(newdata$julian_day))

# Low GDD
# B. laevigata is not present at low GDD sites (there are only three records
# at low GDD locations), so do not include that species in this plot
low_gdd_prediction <- ggplot(data = newdata %>% 
                               filter(gdd == gdd_points[1]) %>%
                               filter(species != "Borodinia laevigata"), 
                             mapping = aes(x = year, 
                                           y = julian_day,
                                           color = species)) +
  geom_line(lwd = 1) +
  ylim(jd_limits) +
  scale_color_manual(values = c("#7b3294", "#a6dba0","#008837"),
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
  ylim(jd_limits) +
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
  ylim(jd_limits) +
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
