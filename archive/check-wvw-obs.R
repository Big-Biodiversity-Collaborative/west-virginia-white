# Check West Virginia White observations
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2019-05-17

library(dplyr)
library(lubridate)

# Some records of WVW are super late, want to flag for further investigation, 
# at least one we know is wrong https://www.gbif.org/occurrence/3427558396 
# (it is a moth, from Texas).

# Want to see all observations before week 10 and on or after week 30
# Translates to before yday 70 and after yday 203
early_day <- 70
late_day <- 203

wvw_obs <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")

# Get the year and julian day
wvw_obs$yday <- lubridate::yday(as.Date(paste0(wvw_obs$year, 
                                               "-", wvw_obs$month, 
                                               "-", wvw_obs$day)))

early <- wvw_obs %>%
  filter(yday <= 70)
# nrow(early)
# [1] 0

late <- wvw_obs %>%
  filter(yday >= 203)
# nrow(late)
# [7]
# Quick formatting of GBIF url to check on those observations
gbif_base <- "https://www.gbif.org/occurrence/"
gbif_urls <- paste0(gbif_base, late$gbifID)
# [1] "https://www.gbif.org/occurrence/3427558396"  # REMOVE
# [2] "https://www.gbif.org/occurrence/3397667698"  # Questionable; parking lot, no image
# [3] "https://www.gbif.org/occurrence/3397436165"  # No image
# [4] "https://www.gbif.org/occurrence/2804300425"  
# [5] "https://www.gbif.org/occurrence/43858728"  
# [6] "https://www.gbif.org/occurrence/2804280427"
# [7] "https://www.gbif.org/occurrence/3397466009"  # REMOVE, V. virginiensis

################################################################################
# Old below here

################################################################################
# Want to avoid Papilio rumiko / rumina disaster
# Definitely bad georefs in GBIF data; iNaturalist is OK
# See file read-in code (ca. line 28) for filtering option
# Note additional filtering on dates in GBIF data is warranted (01-Jan-1700)
library(ggplot2)
library(ggmap)
# Set timeout limit higher for slower connections
httr::set_config(httr::config(connecttimeout = 30))

# Plot either gbif data or iNaturalist data
# GBIF data:
# GBIF.org (18 May 2019) GBIF Occurrence 
# Download https://doi.org/10.15468/dl.yo1jwm 

plot.iNaturalist <- FALSE
if (plot.iNaturalist) {
  plot.data <- read.csv(file = "data/observations-52593.csv")
  latlongs <- unique(plot.data[, c("longitude", "latitude")])
  colnames(latlongs) <- c("Longitude", "Latitude")
} else {
  plot.data <- read.delim(file = "data/0015022-190415153152247.csv")
  plot.data <- plot.data[plot.data$decimalLatitude > 29, ]
  plot.data <- plot.data[plot.data$decimalLongitude > -100, ]
  latlongs <- unique(plot.data[, c("decimalLongitude", "decimalLatitude", "eventDate")])
  colnames(latlongs) <- c("Longitude", "Latitude", "Date")
}

# Drop duplicates and missing data
latlongs <- na.omit(latlongs)

map.bounds <- c(floor(min(c(latlongs$Longitude))),
                floor(min(c(latlongs$Latitude))),
                ceiling(max(c(latlongs$Longitude))),
                ceiling(max(c(latlongs$Latitude))))

# Get a map image
# Have to provide min/max for lat and long, otherwise will assume Google map,
# which requires API key
wvw.map <- get_map(location = map.bounds,
                   source = "stamen",
                   maptype = "terrain")

maptitle <- "P. virginiensis, "
if (plot.iNaturalist) {
  maptitle <- paste0(maptitle, "iNaturalist data")
} else {
  maptitle <- paste0(maptitle = "GBIF data")
}

wvw.obs.map <- ggmap(ggmap = wvw.map) +
  geom_point(data = latlongs,
             mapping = aes(x = Longitude, y = Latitude),
             size = 1.0) +
  theme_bw() +
  ggtitle(label = maptitle)
print(wvw.obs.map)
