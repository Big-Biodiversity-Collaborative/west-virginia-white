# Focus on subset of data
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-02

require(ggplot2)
require(dplyr)
require(lubridate)

# Will likely have four "clusters":
#   Eastern Tennessee
#   OH, PA, WV
#   New England
#   Great Lakes-ish

min_year <- 2000 # NULL
max_year <- 2022

# If true, will exclude all observations that occur after June 21
pre_summer <- TRUE

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
  filter(year >= min_year) %>%
  filter(year <= max_year) %>%
  mutate(julian_day = yday(as.Date(paste(year, month, day, sep = "-"))))

# Restrict to pre-summer observations as appropriate
if (pre_summer) {
  all_obs <- all_obs %>%
    filter(month <= 6) %>% # Drop any after June
    filter(month > 6 | day < 22) # Drop any after June 21
}

# May need to use a MCP approach to select only those plant records relevant 
# to where the bugs are

# Use terra functions to define the minimum convex polygon (MCP), e.g.
# ch <- terra::convHull(x = terra::vect(x = presence,
#                                       geom = c("x", "y"),
#                                       crs = "EPSG:4326"))

# Perhaps then use st functions to only extract plant samples that are in the 
# polygon of interest

# Start by a very crude subset of two areas:
#   Tennessee
#   New England

tenn_data <- all_obs %>%
  filter(latitude < 37.5) %>%
  filter(latitude > 33) %>%
  filter(longitude < -81.5) %>%
  filter(longitude > -87)

ne_data <- all_obs %>%
  filter(latitude > 41) %>%
  filter(latitude < 46) %>%
  filter(longitude > -75) %>%
  filter(longitude < -72.5)

gl_data <- all_obs %>%
  filter(latitude > 43) %>%
  filter(latitude < 47.5) %>%
  filter(longitude < -82.5) %>%
  filter(longitude > -87)

ggplot(data = tenn_data, mapping = aes(x = longitude, 
                                     y = latitude,
                                     color = organism)) +
  geom_point(alpha = 0.5)

ggplot(data = ne_data, mapping = aes(x = longitude, 
                                     y = latitude,
                                     color = organism)) +
  geom_point(alpha = 0.5)

ggplot(data = gl_data, mapping = aes(x = longitude, 
                                     y = latitude,
                                     color = organism)) +
  geom_point(alpha = 0.5)

ggplot(data = tenn_data %>% filter(year >= 2010), mapping = aes(x = year, 
                                       y = julian_day,
                                       color = organism,
                                       group = interaction(organism, year))) +
  # geom_point(alpha = 0.5)
  geom_violin()

ggplot(data = ne_data, mapping = aes(x = year, 
                                     y = julian_day,
                                     color = organism,
                                     group = interaction(organism, year))) +
  # geom_point(alpha = 0.5)
  geom_violin()

ggplot(data = gl_data, mapping = aes(x = year, 
                                     y = julian_day,
                                     color = organism,
                                     group = interaction(organism, year))) +
  # geom_point(alpha = 0.5)
  geom_violin()

# Just plotting density for a single year, could do some calculations based on 
# the amount of overlap these densities have through time...
ggplot(data = ne_data %>% filter(year == 2020), 
       mapping = aes(x = julian_day, group = organism, fill = organism)) +
  geom_density(alpha = 0.5)

ggplot(data = gl_data %>% filter(year == 2020), 
       mapping = aes(x = julian_day, group = organism, fill = organism)) +
  geom_density(alpha = 0.5)

ggplot(data = tenn_data %>% filter(year == 2020), 
       mapping = aes(x = julian_day, group = organism, fill = organism)) +
  geom_density(alpha = 0.5)
