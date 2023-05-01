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
model_1 <- lm(julian_day ~ year + gdd + species, 
                data = all_obs)

# Add the interaction between year and species
model_2 <- lm(julian_day ~ gdd + year * species,
                      data = all_obs)

# Check to see if this is a better model
anova(model_1, model_2)

# Add the two-way interaction between year and gdd, allowing gdd-specific 
# responses to year, across all species
model_3 <- lm(julian_day ~ year * species + year * gdd,
                  data = all_obs)
# summary(model_3)
anova(model_2, model_3)

# Now add the last two-way interaction between gdd and species (species-
# specific responses to local temperatures)
model_4 <- lm(julian_day ~ (year + gdd + species)^2,
              data = all_obs)
# summary(model_4)
anova(model_3, model_4)

# Full model includes
model_5 <- lm(julian_day ~ year * gdd * species,
                 data = all_obs)
# summary(model_5)
anova(model_4, model_5)

# Collate model comparison results
# Table should have:
# Predictors | F | df | P
# Put all models in a list for easier computation
all_models <- list(model_1, model_2, model_3, model_4, model_5)
# Extract terms (predictors) for each model and put in a character vector
model_terms <- unlist(lapply(X = all_models,
                             FUN = function(x) {
                               paste(attributes(x$terms)$term.labels, collapse = ", ")
                             }))
# Some text cleanup
model_terms <- gsub(x = model_terms,
                    pattern = ":",
                    replacement = " x ")
model_terms <- tools::toTitleCase(model_terms)
model_terms <- gsub(x = model_terms,
                    pattern = "Gdd",
                    replacement = "GDD")

# Setup data frame to hold model comparison results 
model_compare <- data.frame(Predictors = model_terms,
                            F = NA_real_,
                            df = NA_integer_,
                            p = NA_real_)
for (i in 2:length(all_models)) {
  compare_fit <- anova(all_models[[i - 1]], all_models[[i]])
  # Vectors of model comparison stats all have NA in first element
  model_compare$F[i] <- round(compare_fit$F[2], digits = 3)
  model_compare$df[i] <- compare_fit$Df[2]
  model_compare$p[i] <- compare_fit$`Pr(>F)`[2]
}

write.csv(x = model_compare,
          file = "output/model-compare.csv",
          row.names = FALSE)

# Full model is best fit...test for heteroskedasticity
best_model <- model_5

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

# How many days/year earlier for:
#    + each species
#    + each GDD (will be an "average" for each of three categories)
# Full (best) model is
# Julian Day = B0 + B1 x year + B2 x gdd + B3 x species +
#                   B4 x year x gdd + B5 x year x species + B6 x gdd x species +
#                   B7 x year x gdd x species
# For insect (assumes insect is reference level for species), change in the 
# number of days / year is
# B1 + B4 x GDD
# For hosts, change in number of days / year is
# B1 + B5 + GDD x (B4 + B7)
# where B5 and B7 have specific values for each host

# Need to have values to substitute in for gdd
# Prior figures use 33% and 66% as cutoffs. Here we will use 33%/2, 50%, and 
# (66% + 100%)/2 as the points to use for our predicted values
gdd_points <- stats::quantile(x = all_obs$gdd[all_obs$organism == "insect"],
                              probs = c(1/6, 1/2, 5/6))

# Insect delta days/year:
insect_delta <- model_results$estimate[model_results$term == "year"] +
  (model_results$estimate[model_results$term == "year:gdd"] * gdd_points)

# Hosts delta days/year:
# Start by getting names to pull out appropriate terms (skipping first level, 
# which is the insect)
host_names <- levels(all_obs$species)[-1]
host_deltas <- sapply(X = host_names,
                      FUN = function(x) {
                        B1 <- model_results$estimate[model_results$term == "year"]
                        B4 <- model_results$estimate[model_results$term == "year:gdd"]
                        B5 <- model_results$estimate[model_results$term == paste0("year:species", x)]
                        B7 <- model_results$estimate[model_results$term == paste0("year:gdd:species", x)]
                        delta <- B1 + B5 + (gdd_points * (B4 + B7))
                        return(delta)
                      })
host_deltas <- t(host_deltas)
all_deltas <- rbind(insect_delta, host_deltas)
rownames(all_deltas)[1] <- levels(all_obs$species)[1]
colnames(all_deltas) <- c("Low GDD", "Medium GDD", "High GDD")
all_deltas <- data.frame(species = rownames(all_deltas),
                         all_deltas)
rownames(all_deltas) <- NULL

write.csv(x = all_deltas,
          file = "output/changes-table.csv",
          row.names = FALSE)

# We are especially interested in how the plants are responding relative to 
# the insect, so compare all host_deltas to the insect_delta
compared_to_insect <- t(apply(X = host_deltas, 
                              MARGIN = 1, 
                              FUN = function(x) { 
                                round(x - insect_delta, digits = 2)
                              }))
colnames(compared_to_insect) <- c("Low_GDD", "Medium_GDD", "High_GDD")
# Negative values: Number of days earlier hosts are shifting each year
# Positive values: Number of days later hosts are shifting each year
compared_to_insect <- data.frame(species = rownames(compared_to_insect),
                                 compared_to_insect)
rownames(compared_to_insect) <- NULL
# Change Low GDD B. laevigata to missing since it does not occur in low GDD 
# sites
compared_to_insect <- compared_to_insect %>%
  mutate(Low_GDD = if_else(species == "Borodinia laevigata",
                           true = NA_real_,
                           false = Low_GDD))
write.csv(x = compared_to_insect,
          file = "output/changes-rel-insect.csv",
          row.names = FALSE)

################################################################################
# Plot responses
# Want three sub-plots, one for low GDD, medium GDD, high GDD

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

line_colors <- c("Pieris virginiensis" = "#7b3294",
                 "Cardamine concatenata" = "#a6dba0",
                 "Cardamine diphylla" = "#008837",
                 "Borodinia laevigata" = "#5aae61")

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
  scale_color_manual(values = line_colors,
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
  scale_color_manual(values = line_colors,
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
  scale_color_manual(values = line_colors,
                     name = "Species") +
  theme_bw() +
  labs(title = "High GDD", x = "Year", y = "Julian day")
high_gdd_prediction
ggsave(filename = "output/figure-3c.png",
       plot = high_gdd_prediction)

# Single, multi-panel figure
# multi_panel <- ggpubr::ggarrange(low_gdd_prediction, 
#                                  medium_gdd_prediction, 
#                                  high_gdd_prediction,
#                                  ncol = 1)
# multi_panel
