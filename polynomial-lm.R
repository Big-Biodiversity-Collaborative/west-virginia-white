# West Virginia White and host plant polynomial model
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-11-28

library(dplyr)     # data wrangling
library(lubridate) # Julian day calculations
library(ggplot2)   # plotting
library(tidyr)     # pivoting

# Test for differential responses to time between insect and host
# Latitude is included as co-variate
# Julian day ~ latitude + year + organism + year x organism
# (but year is modeled as second degree polynomial)

# TODO: Consider some QA/QC, especially for hosts. There appear a fair number
# (~50) of host observations that are very late in the year (November & 
# December). At least some of these are likely mis-identifications, e.g.
# GBIF ID 3070528423, https://www.inaturalist.org/observations/8854921
# But would be good to know if local botany experts are seeing these plants 
# emerge this "early"

# Load data for insect
insect <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")
# Add label for organism (insect vs host)
insect$organism <- "insect"

# Load data for two host plants
host_concatenata <- read.csv(file = "data/cardamine_concatenata-gbif-clean.csv")
host_diphylla <- read.csv(file = "data/cardamine_diphylla-gbif-clean.csv")
# Combine hosts and add label for organism (insect vs host)
hosts <- host_concatenata %>%
  bind_rows(host_diphylla) %>%
  mutate(organism = "host")

# Combine data into single data frame
all_obs <- insect %>%
  bind_rows(hosts)

# Calculate Julian day
all_obs <- all_obs %>%
  mutate(julian_day = yday(as.Date(paste(year, month, day, sep = "-"))))
# head(all_obs)

# Set leveling so the insect is always reference group
all_obs$species <- factor(x = all_obs$species,
                          levels = c("Pieris virginiensis",
                                     "Cardamine concatenata",
                                     "Cardamine diphylla"))
all_obs$organism <- factor(x = all_obs$organism,
                           levels = c("insect", "host"))

# Run linear regression, no interaction
simple_poly <- lm(julian_day ~ latitude + poly(year, degree = 2) + organism, 
                  data = all_obs)
# summary(simple_poly)

# Run linear regression with interaction, 
interaction_poly <- lm(julian_day ~ latitude + poly(year, degree = 2)*organism, 
                  data = all_obs)
# summary(interaction_poly)

# Compare the two regression models
anova(simple_poly, interaction_poly)
# Model 1: julian_day ~ latitude + poly(year, degree = 2) + organism
# Model 2: julian_day ~ latitude + poly(year, degree = 2) * organism
#   Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
# 1  20242 8279741                                  
# 2  20240 8254606  2     25135 30.816 4.339e-14 ***

# Run linear regressions where all three species are separate
species_simple <- lm(julian_day ~ latitude + poly(year, degree = 2) + species, 
                     data = all_obs)
species_poly <- lm(julian_day ~ latitude + poly(year, degree = 2)*species, 
                   data = all_obs)
# summary(species_poly)
anova(species_simple, species_poly)
# Model 1: julian_day ~ latitude + poly(year, degree = 2) + species
# Model 2: julian_day ~ latitude + poly(year, degree = 2) * species
# Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
# 1  20241 7661324                                  
# 2  20237 7646149  4     15175 10.041 4.079e-08 ***
  
# And compare this species polynomial model with the one that did not 
# distinguish between hosts (interaction is only insect/host)
anova(interaction_poly, species_poly)
# Model 1: julian_day ~ latitude + poly(year, degree = 2) * organism
# Model 2: julian_day ~ latitude + poly(year, degree = 2) * species
#   Res.Df     RSS Df Sum of Sq     F    Pr(>F)    
# 1  20240 8254606                                 
# 2  20237 7646149  3    608457 536.8 < 2.2e-16 ***

################################################################################
# Plotting models where two host species are treated as the same thing
################################################################################

# Create data set of varying latitudes to show relationships, using minimum, 
# maximum, and 25-, 50-, 75-percentiles of insect latitudes; quantile defaults
# to these percentiles
lat_percentiles <- quantile(x = all_obs$latitude[all_obs$organism == "insect"])

# Make a plot of the models, start with wide version
plot_data_wide <- all_obs %>%
  select(julian_day, year, organism) %>% 
  mutate(lat_000 = lat_percentiles[1],  # there must be a better way
         lat_025 = lat_percentiles[2],
         lat_050 = lat_percentiles[3],
         lat_075 = lat_percentiles[4],
         lat_100 = lat_percentiles[5])
head(plot_data_wide)

# Transform to long for easier use with ggplot
plot_data_long <- plot_data_wide %>%
  pivot_longer(cols = -c(julian_day, year, organism), 
               names_to = "percentile",
               values_to = "latitude") %>%
  mutate(percentile = as.numeric(gsub(pattern = "lat_", 
                                      replacement = "",
                                      x = percentile))/100)
head(plot_data_long)

# Finally, create a prediction data set to use with geom_line (geom_smooth does 
# not appear amenable to polynomial with interactions)
plot_data <- plot_data_long %>%
  mutate(julian_predicted = predict(interaction_poly, 
                                    newdata = plot_data_long)) %>%
  distinct(year, organism, percentile, .keep_all = TRUE) %>%
  arrange(year)
head(plot_data)

# Make plot with predicted lines
#     Many late-season, and some early-season Cardamine observations that 
#         warrant investigation
year_plot <- ggplot(data = plot_data, mapping = aes(x = year)) +
  # geom_point(data = all_obs, # observations
  #            mapping = aes(y = julian_day,
  #                          shape = species), size = 2) +
  # scale_shape_discrete(solid = FALSE) +
  geom_line(mapping = aes(y = julian_predicted, # models
                          color = interaction(percentile, organism),
                          linetype = organism))
year_plot

# A different approach, where the the latitude quantiles are plotted separately
year_plot <- ggplot(data = plot_data, mapping = aes(x = year)) +
  # geom_point(data = all_obs, # observations
  #            mapping = aes(y = julian_day,
  #                          shape = species), size = 2) +
  # scale_shape_discrete(solid = FALSE) +
  geom_line(mapping = aes(y = julian_predicted, # models
                          color = organism)) +
  facet_wrap(~ latitude) +
  theme_bw()
year_plot

# Let's plot just the mean (or median?) latitude lines
median_lat_plot <- ggplot(mapping = aes(x = year)) +
  geom_point(data = all_obs,
             mapping = aes(y = julian_day,
                           color = organism),
             alpha = 0.25,
             size = 0.25) +
  geom_line(data = plot_data %>% filter(percentile == 0.5),
            mapping = aes(y = julian_predicted,
                          color = organism),
            size = 1) +
  scale_y_continuous(limits = c(min(all_obs$julian_day), 
                                max(all_obs$julian_day))) +
  theme_bw()
median_lat_plot

################################################################################
# Plotting models where two host species are treated as *DIFFERENT*
################################################################################

# Make a plot of the models, start with wide version
plot_data_wide_s <- all_obs %>%
  select(julian_day, year, organism, species) %>% 
  mutate(lat_000 = lat_percentiles[1],  # there must be a better way
         lat_025 = lat_percentiles[2],
         lat_050 = lat_percentiles[3],
         lat_075 = lat_percentiles[4],
         lat_100 = lat_percentiles[5])
head(plot_data_wide_s)

# Transform to long for easier use with ggplot
plot_data_long_s <- plot_data_wide_s %>%
  pivot_longer(cols = -c(julian_day, year, organism, species), 
               names_to = "percentile",
               values_to = "latitude") %>%
  mutate(percentile = as.numeric(gsub(pattern = "lat_", 
                                      replacement = "",
                                      x = percentile))/100)
head(plot_data_long_s)

# Finally, create a prediction data set to use with geom_line (geom_smooth does 
# not appear amenable to polynomial with interactions)
plot_data_s <- plot_data_long_s %>%
  mutate(julian_predicted = predict(species_poly, 
                                    newdata = plot_data_long_s)) %>%
  distinct(year, species, percentile, .keep_all = TRUE) %>%
  arrange(year)
head(plot_data_s)

# Plotting just the means for mean latitudes lines
median_lat_plot_s <- ggplot(mapping = aes(x = year)) +
  geom_point(data = all_obs,
             mapping = aes(y = julian_day,
                           color = species),
             alpha = 0.05,
             size = 2) +
  geom_line(data = plot_data_s %>% filter(percentile == 0.5),
            mapping = aes(y = julian_predicted,
                          color = species),
            size = 1) +
  scale_y_continuous(limits = c(min(all_obs$julian_day), 
                                max(all_obs$julian_day))) +
  theme_bw()
median_lat_plot_s

################################################################################
# Once more, without any summer or fall observations
# Summer starts June 21 = 172 (or 173 in leap year)
spring_obs <- all_obs %>%
  filter(month <= 6) %>% # Drop any after June
  filter(month < 6 | day < 22) # Drop any after June 21

# Simple linear regression, no interaction
simple_linear_spring <- lm(julian_day ~ latitude + year + organism, 
                           data = spring_obs)

# Run linear regression, no interaction
simple_poly_spring <- lm(julian_day ~ latitude + poly(year, degree = 2) + organism, 
                         data = spring_obs)

anova(simple_linear_spring, simple_poly_spring)
# Model 1: julian_day ~ latitude + year + organism
# Model 2: julian_day ~ latitude + poly(year, degree = 2) + organism
# Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
# 1  20086 4141838                                  
# 2  20085 4131588  1     10250 49.828 1.733e-12 ***
  
# Run linear regression with interaction, 
interaction_poly_spring <- lm(julian_day ~ latitude + poly(year, degree = 2)*organism, 
                       data = spring_obs)

# Compare the two regression models
anova(simple_poly_spring, interaction_poly_spring)
# Model 1: julian_day ~ latitude + poly(year, degree = 2) + organism
# Model 2: julian_day ~ latitude + poly(year, degree = 2) * organism
#   Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
# 1  20085 4131588                                  
# 2  20083 4105665  2     25923 63.402 < 2.2e-16 ***

# Run linear regressions where all three species are separate
species_simple_spring <- lm(julian_day ~ latitude + poly(year, degree = 2) + species, 
                     data = spring_obs)
species_poly_spring <- lm(julian_day ~ latitude + poly(year, degree = 2)*species, 
                   data = spring_obs)
anova(species_simple, species_poly)
# Model 1: julian_day ~ latitude + poly(year, degree = 2) + species
# Model 2: julian_day ~ latitude + poly(year, degree = 2) * species
#   Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
# 1  20241 7661324                                  
# 2  20237 7646149  4     15175 10.041 4.079e-08 ***
summary(species_poly_spring)

# Make a plot of the models, start with wide version
spring_data_wide <- spring_obs %>%
  select(julian_day, year, organism, species) %>% 
  mutate(lat_000 = lat_percentiles[1],  # there must be a better way
         lat_025 = lat_percentiles[2],
         lat_050 = lat_percentiles[3],
         lat_075 = lat_percentiles[4],
         lat_100 = lat_percentiles[5])
head(spring_data_wide)

# Transform to long for easier use with ggplot
spring_data_long <- spring_data_wide %>%
  pivot_longer(cols = -c(julian_day, year, organism, species), 
               names_to = "percentile",
               values_to = "latitude") %>%
  mutate(percentile = as.numeric(gsub(pattern = "lat_", 
                                      replacement = "",
                                      x = percentile))/100)
head(spring_data_long)

# Finally, create a prediction data set to use with geom_line (geom_smooth does 
# not appear amenable to polynomial with interactions)
spring_plot_data <- spring_data_long %>%
  mutate(julian_predicted = predict(species_poly_spring, 
                                    newdata = spring_data_long)) %>%
  distinct(year, species, percentile, .keep_all = TRUE) %>%
  arrange(year)
head(spring_plot_data)

# Plotting just the means for mean latitudes lines
spring_median_plot <- ggplot(mapping = aes(x = year)) +
  geom_point(data = spring_obs,
             mapping = aes(y = julian_day,
                           color = species),
             alpha = 0.1,
             size = 1) +
  geom_line(data = spring_plot_data %>% filter(percentile == 0.5),
            mapping = aes(y = julian_predicted,
                          color = species),
            size = 1) +
  scale_y_continuous(limits = c(min(spring_obs$julian_day), 
                                max(spring_obs$julian_day))) +
  scale_color_discrete(name = "Species") +
  ylab(label = "Julian Day") +
  xlab(label = "Year") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.18))
spring_median_plot
ggsave(spring_median_plot, filename = "output/polynomial-plot.pdf")

# A model I do not want to try to interpret includes a year X latitude 
# interaction, too
complex_poly_spring <- lm(julian_day ~ latitude*year + poly(year, degree = 2)*species, 
                          data = spring_obs)
summary(complex_poly_spring)

# Do the predictions then and plot a few latitude lines
spring_complex_data <- spring_data_long %>%
  mutate(julian_predicted = predict(complex_poly_spring, 
                                    newdata = spring_data_long)) %>%
  distinct(year, species, percentile, .keep_all = TRUE) %>%
  arrange(year)
head(spring_complex_data)
# Warning message:
#   Problem while computing `julian_predicted = predict(complex_poly_spring, newdata = spring_data_long)`.
#   prediction from a rank-deficient fit may be misleading 

# A beast
spring_complex_plot <- ggplot(mapping = aes(x = year)) +
  # geom_point(data = spring_obs,
  #            mapping = aes(y = julian_day,
  #                          color = species),
  #            alpha = 0.1,
  #            size = 1) +
  geom_line(data = spring_complex_data,
            mapping = aes(y = julian_predicted,
                          # color = interaction(species,percentile),
                          color = as.factor(percentile),
                          lty = species),
            size = 1) +
  # scale_y_continuous(limits = c(min(spring_obs$julian_day), 
  #                               max(spring_obs$julian_day))) +
  scale_color_discrete(name = "Species") +
  ylab(label = "Julian Day") +
  xlab(label = "Year") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.18))
spring_complex_plot
