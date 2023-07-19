# Linear regression analysis for yearly change in julian day incl. midpoint temp
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-07-14

# Only includes the two Cardamine hosts
# TODO: With best model excluding the three-way interaction, the three plots 
# differ only in intercept - is it necessary to have the three plots, still?

library(dplyr)     # data wrangling
library(lubridate) # Julian day calculations
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

# Only consider two host plants, C. diphylla & C. concatenata
all_obs <- all_obs %>%
  filter(species %in% c("Pieris virginiensis",
                        "Cardamine diphylla",
                        "Cardamine concatenata"))

# Set leveling so the insect is always reference group
all_obs$species <- factor(x = all_obs$species,
                          levels = c("Pieris virginiensis",
                                     "Cardamine diphylla",
                                     "Cardamine concatenata"))
all_obs$organism <- factor(x = all_obs$organism,
                           levels = c("insect", "host"))

# Add temperature data from data-weather.R
temperature <- read.csv(file = "data/temperature-obs.csv")
all_obs <- all_obs %>%
  left_join(temperature, by = "gbifID")

# Since models will include temperature, drop any observations that are missing 
# temperature data and calculate the midpoint temperature (mean of tmin and 
# tmax)
all_obs <- all_obs %>%
  filter(!is.na(tmin) & !is.na(tmax)) %>%
  mutate(tmid = (tmin + tmax)/2) # mean didn't work?!?

################################################################################
# Analyses
# Run linear regression, no interaction
model_1 <- lm(julian_day ~ year + tmid + species, 
                data = all_obs)
summary(model_1)

# Add the interaction between year and species
model_2 <- lm(julian_day ~ tmid + year * species,
                      data = all_obs)
summary(model_2)

# Check to see if this is a better model
anova(model_1, model_2)

# Add the two-way interaction between year and tmid, allowing tmid-specific 
# responses to year, across all species
model_3 <- lm(julian_day ~ year * species + year * tmid,
                  data = all_obs)
summary(model_3)
anova(model_2, model_3)

# Now add two-way interaction between year and species (species-specific 
# responses to local temperatures)
model_4 <- lm(julian_day ~ (year + tmid + species)^2,
              data = all_obs)
summary(model_4)
anova(model_3, model_4)

# Full model includes
model_5 <- lm(julian_day ~ year * tmid * species,
                 data = all_obs)
summary(model_5)
anova(model_4, model_5) # NS

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
                    pattern = "Tmid",
                    replacement = "Temperature")

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
          file = "output/model-compare-tmid.csv",
          row.names = FALSE)

# Model 4 is best fit; test for heteroskedasticity
best_model <- model_4

lmtest::bptest(best_model)
# studentized Breusch-Pagan test
# data:  best_model
# BP = 323.42, df = 9, p-value < 2.2e-16

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
# The test statistic, BP, is chi-squared
# studentized Breusch-Pagan test
# 
# data:  best_model_wls
# BP = 11.709, df = 9, p-value = 0.2302

################################################################################
# Effect summary
model_results <- broom::tidy(best_model_wls)

# Clean up the table for human eyes
model_table <- model_results %>%
  mutate(term = gsub(x = term, pattern = "tmid", replacement = "Temperature")) %>%
  mutate(term = gsub(x = term, pattern = "year", replacement = "Year")) %>%
  mutate(term = gsub(x = term, pattern = "species", replacement = "")) %>%
  mutate(term = gsub(x = term, pattern = "Year:", replacement = "Year x ")) %>%
  mutate(term = gsub(x = term, pattern = "Temperature:", replacement = "Temperature x ")) %>%
  mutate(term = gsub(x = term, pattern = "(Intercept)", replacement = "Intercept"))

# Do not need such precision!
# Funky new behavior of across
model_table <- model_table %>%
  # mutate(across(.cols = estimate:p.value, .fns = signif, digits = 3))
  mutate(across(.cols = estimate:p.value, \(x) signif(x, digits = 3)))

# Write to a file
write.csv(file = "output/model-table-tmid.csv",
          x = model_table,
          row.names = FALSE)

# How many days/year earlier for:
#    + each species
#    + each Temperature (will be an "average" for each of three categories)
# Best model is
# Julian Day = B0 + B1 x year + B2 x tmid + B3 x species +
#                   B4 x year x tmid + B5 x year x species + B6 x tmid x species
# For insect (assumes insect is reference level for species), change in the 
# number of days / year is
# B1 + tmid x B4
# For hosts, change in number of days / year is
# B1 + tmid x B4 + B5
# where B5 has specific values for each host

# Need to have values to substitute in for tmid
# Prior figures use 33% and 66% as cutoffs. Here we will use 33%/2, 50%, and 
# (66% + 100%)/2 as the points to use for our predicted values
tmid_points <- stats::quantile(x = all_obs$tmid[all_obs$organism == "insect"],
                              probs = c(1/6, 1/2, 5/6))

# Insect delta days/year:
insect_delta <- model_results$estimate[model_results$term == "year"] +
  (model_results$estimate[model_results$term == "year:tmid"] * tmid_points)

# Hosts delta days/year:
# Start by getting names to pull out appropriate terms (skipping first level, 
# which is the insect)
host_names <- levels(all_obs$species)[-1]
host_deltas <- sapply(X = host_names,
                      FUN = function(x) {
                        B1 <- model_results$estimate[model_results$term == "year"]
                        B4 <- model_results$estimate[model_results$term == "year:tmid"]
                        B5 <- model_results$estimate[model_results$term == paste0("year:species", x)]
                        delta <- B1 + (tmid_points * B4) + B5
                        return(delta)
                      })
host_deltas <- t(host_deltas)
all_deltas <- rbind(insect_delta, host_deltas)
rownames(all_deltas)[1] <- levels(all_obs$species)[1]
colnames(all_deltas) <- c("Low_Temp", "Medium_Temp", "High_Temp")
all_deltas <- data.frame(species = rownames(all_deltas),
                         all_deltas)
rownames(all_deltas) <- NULL

# We only want appropriate temperature combinations, so look at distribution of 
# plant tmids relative to each of the three bins based on insect tmid. That is, 
# if there are no observations of a plant species in low temperature bin, we 
# don't want those effects in the tables or lines in the corresponding plot.
tmid_bins <- stats::quantile(x= all_obs$tmid[all_obs$organism == "insect"],
                             probs = c(1/3, 2/3))
all_obs <- all_obs %>%
  mutate(tmid_bin = case_when(tmid <= tmid_bins[1] ~ "Low_Temp",
                              tmid > tmid_bins[1] & tmid <= tmid_bins[2] ~ "Medium_Temp",
                              tmid > tmid_bins[2] ~ "High_Temp",
                              .default = NA_character_)) %>%
  mutate(tmid_bin = factor(x = tmid_bin, 
                           levels = c("Low_Temp", "Medium_Temp", "High_Temp")))

table(all_obs$species, all_obs$tmid_bin)
#                       Low_Temp Medium_Temp High_Temp
# Pieris virginiensis        298         297       297
# Cardamine diphylla        1086        1747       908
# Cardamine concatenata      247        1538      2435

# Might still be useful to visualize?
ggplot(data = all_obs, mapping = aes(x = longitude, 
                                     y = latitude, 
                                     color = factor(tmid_bin,
                                                    levels = c("High_Temp",
                                                               "Medium_Temp",
                                                               "Low_Temp")))) +
  geom_point() +
  scale_color_manual(name = "Temperature", values = c("#CC5522", "#CCCC11", "#5533CC")) +
  facet_wrap(~ species) +
  theme_bw()

write.csv(x = all_deltas,
          file = "output/changes-table-tmid.csv",
          row.names = FALSE)

# We are especially interested in how the plants are responding relative to 
# the insect, so compare all host_deltas to the insect_delta
# Code is a lot more complicated than it needs to be with model_4 as best model
# Because there is no year x tmid x species interaction, the only thing
# the difference between insect and hosts response are the two values of B5 
# (one value for each species of host); resultant compared_to_insect matrix 
# will thus have identical values across a row (no differences among different 
# temperature zones)
compared_to_insect <- t(apply(X = host_deltas, 
                              MARGIN = 1, 
                              FUN = function(x) { 
                                round(x - insect_delta, digits = 2)
                              }))
colnames(compared_to_insect) <- c("Low_Temp", "Medium_Temp", "High_Temp")
# Negative values: Number of days earlier hosts are shifting each year
# Positive values: Number of days later hosts are shifting each year
compared_to_insect <- data.frame(species = rownames(compared_to_insect),
                                 compared_to_insect)
rownames(compared_to_insect) <- NULL

write.csv(x = compared_to_insect,
          file = "output/changes-rel-insect-tmid.csv",
          row.names = FALSE)

################################################################################
# Plot responses
# Want three sub-plots, one for low temperature, medium temperature, high 
# temperature

# Create an empty data frame to hold values we want to make predictions for
empty_newdata <- data.frame(year = numeric(0),
                            species = character(0),
                            tmid = numeric(0))

# Make the data frame "complete"
newdata <- empty_newdata %>%
  tidyr::expand(year = unique(all_obs$year),
                species = unique(all_obs$species),
                tmid = tmid_points)

# Use desired model to make predictions
jd_predict <- predict(object = best_model_wls, 
                              newdata = newdata,
                              se.fit = TRUE)
newdata$julian_day <- jd_predict$fit
newdata$jd_se <- jd_predict$se.fit

# Plot predicted lines
# Want to have same Julian day scale on each plot
jd_limits <- c(min(newdata$julian_day - newdata$jd_se), 
               max(newdata$julian_day + newdata$jd_se))

# line_colors <- c("Pieris virginiensis" = "#7b3294",
#                  "Cardamine concatenata" = "#a6dba0",
#                  "Cardamine diphylla" = "#008837",
#                  "Borodinia laevigata" = "#5aae61")

# line_colors <- c("Pieris virginiensis" = "#BEBADA",
#                  "Cardamine concatenata" = "#8DD3C7",
#                  "Cardamine diphylla" = "#8DD37C",
#                  "Borodinia laevigata" = "#FB8072")

line_colors <- c("Pieris virginiensis" = "#BEAED4",
                 "Cardamine concatenata" = "#FDC086",
                 "Cardamine diphylla" = "#7FC97F")

# Low temperature
low_tmid_prediction <- ggplot(data = newdata %>% 
                                filter(tmid == tmid_points[1]), 
                              mapping = aes(x = year, 
                                            color = species)) +
  geom_line(lwd = 1, mapping = aes(y = julian_day)) + # predicted lines
  geom_line(linetype = 2, mapping = aes(y = julian_day - jd_se)) + # Lower SE
  geom_line(linetype = 2, mapping = aes(y = julian_day + jd_se)) + # Upper SE
  # geom_jitter(data = all_obs, mapping = aes(y = julian_day), size = 0.5, alpha = 0.5) +
  ylim(jd_limits) +
  scale_color_manual(values = line_colors,
                     name = "Species") +
  theme_bw() +
  labs(title = "Low Temp", x = "Year", y = "Julian day")
low_tmid_prediction
# ggsave(filename = "output/figure-3a.png",
#        plot = low_tmid_prediction)

# Medium temperature
medium_tmid_prediction <- ggplot(data = newdata %>% 
                                   filter(tmid == tmid_points[2]), 
                                 mapping = aes(x = year, 
                                               color = species)) +
  geom_line(lwd = 1, mapping = aes(y = julian_day)) + # predicted lines
  geom_line(linetype = 2, mapping = aes(y = julian_day - jd_se)) + # Lower SE
  geom_line(linetype = 2, mapping = aes(y = julian_day + jd_se)) + # Upper SE
  # geom_jitter(data = all_obs, size = 0.5, alpha = 0.5) +
  ylim(jd_limits) +
  scale_color_manual(values = line_colors,
                     name = "Species") +
  theme_bw() +
  labs(title = "Medium Temp", x = "Year", y = "Julian day")
medium_tmid_prediction
# ggsave(filename = "output/figure-3b.png",
#        plot = medium_tmid_prediction)

# High temperature
high_tmid_prediction <- ggplot(data = newdata %>% 
                                 filter(tmid == tmid_points[3]), 
                               mapping = aes(x = year, 
                                             color = species)) +
  geom_line(lwd = 1, mapping = aes(y = julian_day)) + # predicted lines
  geom_line(linetype = 2, mapping = aes(y = julian_day - jd_se)) + # Lower SE
  geom_line(linetype = 2, mapping = aes(y = julian_day + jd_se)) + # Upper SE
  # geom_jitter(data = all_obs, size = 0.5, alpha = 0.5) +
  ylim(jd_limits) +
  scale_color_manual(values = line_colors,
                     name = "Species") +
  theme_bw() +
  labs(title = "High Temp", x = "Year", y = "Julian day")
high_tmid_prediction
# ggsave(filename = "output/figure-3c.png",
#        plot = high_tmid_prediction)

# Single, multi-panel figure
multi_panel <- ggpubr::ggarrange(low_tmid_prediction,
                                 medium_tmid_prediction,
                                 high_tmid_prediction,
                                 ncol = 1)
multi_panel

################################################################################
# Boxplots of observations
obs_box <- ggplot(data = all_obs, mapping = aes(x = as.factor(year), 
                                                y = julian_day, 
                                                fill = species,
                                                color = species)) +
  geom_violin(scale = "count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
obs_box

# Medians
obs_summary <- all_obs %>%
  group_by(species, year) %>%
  summarize(jd_median = median(julian_day, na.rm = TRUE),
            jd_mean = mean(julian_day, na.rm = TRUE),
            jd_se = sd(julian_day, na.rm = TRUE)/sqrt(n()))
med_plot <- ggplot(data = obs_summary, mapping = aes(x = year, 
                                                     color = species)) +
  geom_point(mapping = aes(y = jd_median), shape = 1) +
  geom_point(mapping = aes(y = jd_mean), shape = 3) +
  theme_bw()
med_plot
all_plot <- ggplot(data = all_obs, mapping = aes(x = year, 
                                                 y = julian_day,
                                                 color = species)) +
  geom_point() +
  theme_bw()
all_plot

# Just the bug, color by temperature
insect_plot <- ggplot(data = all_obs %>% filter(organism == "insect"), 
                      mapping = aes(x = year, 
                                    y = julian_day,
                                    color = tmid)) +
  geom_point(size = 3) +
  scale_color_continuous(type = "viridis") +
  theme_bw()
insect_plot

# One host, colored by temperature
diphylla_plot <- ggplot(data = all_obs %>% filter(species == "Cardamine diphylla"), 
                      mapping = aes(x = year, 
                                    y = julian_day,
                                    color = tmid)) +
  geom_point(size = 3) +
  scale_color_continuous(type = "viridis") +
  theme_bw()
diphylla_plot
