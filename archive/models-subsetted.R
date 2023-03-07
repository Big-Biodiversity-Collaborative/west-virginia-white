# Focus on subset of data
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-02

require(ggplot2)
require(dplyr)
require(lubridate)
require(ggridges)
require(ks)

# Will likely have four "clusters":
#   Eastern Tennessee
#   OH, PA, WV
#   New England
#   Great Lakes-ish

min_year <- 2010 # NULL
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
  mutate(julian_day = yday(as.Date(paste(year, month, day, sep = "-")))) %>%
  mutate(organism = factor(organism, levels = c("insect", "host")))

# Restrict to pre-summer observations as appropriate
if (pre_summer) {
  all_obs <- all_obs %>%
    filter(month <= 6) %>% # Drop any after June
    filter(month < 6 | day < 22) # Drop any after June 21
}

# May need to use a MCP approach to select only those plant records relevant 
# to where the bugs are
# Some density envelope fun
cutoff <- 0.75
insect <- insect %>%
  filter(year <= max_year) %>%
  filter(year >= min_year)
insect_mat <- as.matrix(insect[, c("longitude", "latitude")])
bandwidth <- ks::Hns(insect_mat)
kd_estimate <- ks::kde(x = insect_mat, H = bandwidth)
insect$kde <- predict(object = kd_estimate, x = insect_mat)
threshold <- (1 - cutoff) * 100

insect <- insect %>%
  mutate(envelope = if_else(kde >= kd_estimate$cont[paste0(threshold, "%")],
                            true = 1,
                            false = 0))

plot(latitude ~ longitude, data = insect, col = "gray30", pch = 19, cex = 0.5,
     las = 1, xlab = "", ylab = "")
plot(kd_estimate, cont = (cutoff * 100), drawlabels = FALSE, col = "blue", add = TRUE)
points(latitude ~ longitude, data = insect[insect$envelope == 1, ],
       col = "gray70", pch = 19, cex = 0.5)


# Use terra functions to define the minimum convex polygon (MCP), e.g.
# ch <- terra::convHull(x = terra::vect(x = presence,
#                                       geom = c("x", "y"),
#                                       crs = "EPSG:4326"))


# Start by a very crude subset of four areas:
#   Tennessee
#   New England
#   OH, PA, WV
#   Great Lakes

tenn_data <- all_obs %>%
  filter(latitude < 37.5) %>%
  filter(latitude > 33) %>%
  filter(longitude < -81.5) %>%
  filter(longitude > -87)

# Ditching plants that are outside the mcp for the bug
# Start by figuring out the MCP
tenn_mcp <- sf::st_combine(x = tenn_data %>% 
                                dplyr::filter(organism == "insect") %>%
                                dplyr::rename(x = longitude,
                                              y = latitude) %>%
                                dplyr::select(x, y) %>%
                                sf::st_as_sf(coords = c("x", "y"),
                                             crs = "EPSG:4326")) %>%
  sf::st_convex_hull()
# plot(tenn_mcp)
# Now do calculations for each point
tenn_in_mcp <- sf::st_within(x = sf::st_as_sf(tenn_data %>%
                                                dplyr::rename(x = longitude,
                                                              y = latitude) %>%
                                                dplyr::select(x, y) %>%
                                                sf::st_as_sf(coords = c("x", "y"),
                                                             crs = "EPSG:4326")),
                             y = tenn_mcp)
# Just weird stuff to get T/F from the sf object
points_in <- tenn_in_mcp %>% lengths > 0
# Select based on this logical
tenn_data <- tenn_data[points_in, ]


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

cent_data <- all_obs %>%
  filter(latitude > 39) %>%
  filter(latitude < 45) %>%
  filter(longitude < -77.5) %>%
  filter(longitude > -82.5)

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

# Ridgeline plots
# For Tennessee, example includes separate plots for insect and host, then a 
# plot showing both
tenn_data %>% 
  filter(organism == "insect") %>%
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year))) +
  geom_density_ridges() +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Year") +
  xlab("Julian day")

tenn_data %>% 
  filter(organism == "host") %>%
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year))) +
  geom_density_ridges() +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Year") +
  xlab("Julian day")

tenn_data %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  theme_bw() +
  ylab("Year") +
  xlab("Julian day") +
  ggtitle("Tennessee")

# OH, PA, WV
cent_data %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  theme_bw() +
  ylab("Year") +
  xlab("Julian day") +
  ggtitle("OH, PA, WV")

# New England
ne_data %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  theme_bw() +
  ylab("Year") +
  xlab("Julian day") +
  ggtitle("New England")

# Great Lakes
gl_data %>% 
  mutate(year = factor(year, levels = rev(sort(unique(year))))) %>%
  ggplot(mapping = aes(x = julian_day, y = factor(year), fill = organism)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_manual(values = c("#c2a5cf", "#a6dba0")) +
  theme_bw() +
  ylab("Year") +
  xlab("Julian day") +
  ggtitle("Great Lakes")

summary(lm(julian_day ~ year*organism, data = tenn_data))
summary(lm(julian_day ~ year*organism, data = gl_data))
summary(lm(julian_day ~ year*organism, data = ne_data))
summary(lm(julian_day ~ year*organism, data = cent_data))
