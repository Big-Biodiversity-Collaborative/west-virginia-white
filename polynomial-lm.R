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
# distinguish between hosts
anova(interaction_poly, species_poly)
# Model 1: julian_day ~ latitude + poly(year, degree = 2) * organism
# Model 2: julian_day ~ latitude + poly(year, degree = 2) * species
#   Res.Df     RSS Df Sum of Sq     F    Pr(>F)    
# 1  20240 8254606                                 
# 2  20237 7646149  3    608457 536.8 < 2.2e-16 ***

# TODO: Update plots where each species is treated separately


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
# TODO: Still room for improvement
#     Need to manually color/type lines so it is easier to see which are host, 
#         which are insect
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
