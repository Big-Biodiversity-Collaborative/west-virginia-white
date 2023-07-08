# Download monthly temperature data for observations based on lat/long
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-06-12

# library(jsonlite)  # creating json objects & extracting json responses
# library(RCurl)     # posting query
library(lubridate) # getting starting & ending dates
library(dplyr)     # creating data for query
# library(tidyr)     # pivoting to wide

# TODO: Needs revision
#   + ACIS doesn't include Canada data :(
#   + MODISTools download of VIIRS temperature data can error out and is 
#     painfully slow
#   + Could try direct queries for Daymet data; see 
#     https://daymet.ornl.gov/web_services

################################################################################
# ACIS approach
################################################################################
# Using resources from https://www.rcc-acis.org/docs_webservices.html, 
# specifically will be using gridded data - documentation available at 
# https://www.rcc-acis.org/docs_webservices.html#title24
# Much comes courtesy of M. Crimmins @mcrimmins

# Create a JSON object with query parameters, e.g.
# {"loc":"-78.5,39.2",
#  "grid":"21",
#  "elems":"gdd40,pcpn",
#  "sdate":"2020-03-01",
#  "edate":"2020-04-30"}

# Notes on above:
# grid value of 21 is PRISM data

# Note all keys and values need to be wrapped in quotation marks *within* the 
# string

# Test 1: single location, one-row data frame
df <- data.frame(loc = "-78.5,39.2",
                 grid = "21",
                 elems = "avgt",
                 sdate = "2020-03-01",
                 edate = "2020-04-30")

# Test 1.1: single location, missing data
df <- data.frame(loc = "-79.56943,43.19078",
                 grid = "21",
                 elems = "avgt",
                 sdate = "2016-03-01",
                 edate = "2016-04-30")

# Turn the data frame into a json object each row becomes an element
df_asJSON <- jsonlite::toJSON(df)

# Web service doesn't like containing square braces, so remove those
df_asJSON <- substr(x = df_asJSON,
                    start = 2,
                    stop = nchar(df_asJSON) - 1)

# Post the JSON to the web service
df_result <- postForm(uri = "https://data.rcc-acis.org/GridData",
                      .opts = list(postfields = df_asJSON,
                                   httpheader = c("Content-Type" = "application/json",
                                                  Accept = "application/json")))
# Convert the JSON response into a data frame
df_result_out <- as.data.frame(jsonlite::fromJSON(df_result))

# Test 2: two locations, two-row data frame
# Doesn't seem to work (call to postForm just returns "Error: ")
# df_2 <- data.frame(loc = c("-78.5,39.2", "-82.1,41.2"),
#                    grid = "21",
#                    elems = "avgt",
#                    sdate = c("2020-03-01", "2020-04-01"),
#                    edate = c("2020-04-30", "2020-05-31"))
# 
# df_2_asJSON <- jsonlite::toJSON(df_2)
# df_2_asJSON <- substr(x = df_2_asJSON,
#                       start = 2,
#                       stop = nchar(df_2_asJSON) - 1)
# df_2_result <- postForm(uri = "https://data.rcc-acis.org/GridData",
#                       .opts = list(postfields = df_2_asJSON,
#                                    httpheader = c("Content-Type" = "application/json",
#                                                   Accept = "application/json")))

# Test 3: do loop on subset of actual observations
all_obs <- read.csv(file = "data/filtered-obs.csv")

# Preparing columns we need for query
query_obs <- all_obs %>%
  mutate(loc = paste0(longitude, ",", latitude)) %>%
  mutate(month_start = lubridate::ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(sdate = format(month_start %m-% months(1), "%Y-%m-%d"),
         edate = format(month_start %m+% months(1) - days(1), "%Y-%m-%d")) %>%
  select(loc, sdate, edate, gbifID) %>%
  mutate(grid = "21",
         elems = "avgt")

# For testing purposes, try it out on 200 rows
# query_obs <- query_obs[sample(x = 1:nrow(query_obs), size = 200), ]
print_freq <- 100
sleep_freq <- 10
# Data frame to hold results, only need identifier (gbifID) and mean value
means_df <- data.frame(gbifID = query_obs$gbifID,
                       avgt = NA_real_)
start_time <- Sys.time()
for (i in 1:nrow(query_obs)) {
  if (i %% print_freq == 0 || i == 1) {
    message("Querying ", i, " of ", nrow(query_obs))
  }
  if (i %% sleep_freq == 0) {
    Sys.sleep(1)
  }
  # Drop identifier column and pull out single row of observations
  x <- query_obs %>%
    select(-gbifID) %>%
    slice(i)

  # Turn the data frame into a json object; each row becomes an element
  obs_JSON <- jsonlite::toJSON(x)
  
  # Web service doesn't like containing square braces, so remove those
  obs_JSON <- substr(x = obs_JSON,
                     start = 2,
                     stop = nchar(obs_JSON) - 1)
  
  # Post the JSON to the web service
  query_result_JSON <- postForm(uri = "https://data.rcc-acis.org/GridData",
                                .opts = list(postfields = obs_JSON,
                                             httpheader = c("Content-Type" = "application/json",
                                                            Accept = "application/json")))
  # Convert the JSON response into a data frame
  query_result <- as.data.frame(jsonlite::fromJSON(query_result_JSON))
  # First column is date, second is temperature (everything returned as text)
  colnames(query_result) <- c("Date", "Temperature")

  # Missing values are returned as -999, convert to numeric then use NA
  query_result <- query_result %>%
    mutate(Temperature = as.numeric(Temperature)) %>%
    mutate(Temperature = if_else(Temperature < -273, 
                                 true = NA,
                                 false = Temperature))

  # Convert second column (temperature, F) to Celsius
  query_result <- query_result %>%
    mutate(Temperature = (Temperature - 32) * (5/9))

  # Calculate mean for this lat/long/date coord
  means_df$avgt[i] <- mean(query_result$Temperature, na.rm = TRUE)
}
Sys.time() - start_time

write.csv(file = "data/temperature-obs.csv", 
          x = means_df,
          row.names = FALSE)

################################################################################
# VIIRS temperature via MODISTools
################################################################################
library(MODISTools)
prods <- MODISTools::mt_products()
temperature_rows <- grep("temperature", 
                         ignore.case = TRUE, 
                         x = prods$description)
# The temperature products
# prods[temperature_rows, ]
# Look at bands for subset of products
# mt_bands(product = "MOD11A2")
# mt_bands(product = "MOD21A2")
# mt_bands(product = "VNP21A2") # <---- this one has temperature data
viirs_product <- "VNP21A2"
viirs_bands <- c("LST_Day_1KM", "LST_Night_1KM")

all_obs <- read.csv(file = "data/filtered-obs.csv")
all_obs <- all_obs %>%
  select(longitude, latitude, year, month, gbifID) %>%
  rename(lat = latitude,
         lon = longitude) %>%
  mutate(month_start = lubridate::ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(sdate = format(month_start %m-% months(1), "%Y-%m-%d"),
         edate = format(month_start %m+% months(1) - days(1), "%Y-%m-%d"))

# One query test
# viirs_query <- mt_subset(product = viirs_product,
#                         band = viirs_bands,
#                         lat = 49.03333333,
#                         lon = -122.65,
#                         start = "2019-01-01",
#                         end = "2020-12-30")

# Only 10 sites
# query_obs <- all_obs[1:10, ]
# 100 sites
query_obs <- all_obs[sample(x = 1:nrow(all_obs), size = 100), ]
# The full data set
# query_obs <- all_obs
temp_list <- vector(mode = "list", length = nrow(query_obs))
start <- Sys.time()
for (site_i in 1:nrow(query_obs)) {
  site_name <- query_obs$site_name[site_i]
  message("Running query for site ", site_name, 
          " (", site_i, " of ", nrow(query_obs), ")")
  # Run the query
  viirs_query <- mt_subset(product = viirs_product,
                           band = viirs_bands,
                           lat = query_obs$lat[site_i],
                           lon = query_obs$lon[site_i],
                           start = query_obs$sdate[site_i],
                           end = query_obs$edate[site_i],
                           progress = FALSE)

  # Returned query has Kelvin temperature that needs to be scaled, note valid 
  # values are 7500 - 65535, so replace anything outside that range with NA
  # Replace values with Celsius (multiply by scale and subtract 273)
  viirs_query <- viirs_query %>%
    select(band, latitude, longitude, value, scale) %>%
    mutate(value = if_else(between(value, 
                                   left = 7500,
                                   right = 65535),
                           true = value,
                           false = NA)) %>%
    mutate(value = (value * as.numeric(scale)) - 273.15)
  
  # Calculate two-month mean for each of two values
  viirs_summary <- viirs_query %>%
    group_by(band) %>%
    summarize(mean_temp = mean(value, na.rm = TRUE))
  
  # Wrangle a bit to get into two-column format
  viirs_result <- viirs_summary %>%
    pivot_wider(names_from = band, values_from = mean_temp) %>%
    rename(day_temp = LST_Day_1KM,
           night_temp = LST_Night_1KM)
  
  temp_list[[site_i]] <- viirs_result
}
end <- Sys.time()
# Bundle all the results back together
all_temps <- temp_list %>%
  bind_rows()
rownames(all_temps) <- NULL
end - start

# For a single site
site_i <- 5000
viirs_query <- mt_subset(product = viirs_product,
                         band = viirs_bands,
                         lat = query_obs$lat[site_i],
                         lon = query_obs$lon[site_i],
                         start = query_obs$sdate[site_i],
                         end = query_obs$edate[site_i])
# Returned query has Kelvin temperature that needs to be scaled, note valid 
# values are 7500 - 65535, so replace anything outside that range with NA
# Replace values with Celsius (multiply by scale and subtract 273)
viirs_query <- viirs_query %>%
  select(band, latitude, longitude, value, scale) %>%
  mutate(value = if_else(between(value, 
                                 left = 7500,
                                 right = 65535),
                         true = value,
                         false = NA)) %>%
  mutate(value = (value * as.numeric(scale)) - 273.15)

# Calculate two-month mean for each of two values
viirs_summary <- viirs_query %>%
  group_by(band) %>%
  summarize(mean_temp = mean(value, na.rm = TRUE))

  

################################################################################
# Daymet temperature via MODISTools
################################################################################
# Did not ever get this to work (always returned 400 error)
daymet_bands <- MODISTools::mt_bands(product = "Daymet")
# 1 dayl                  Day length (s/day)     s/day            1
# 2 prcp              Precipitation (mm/day)    mm/day            1
# 3 srad         Shortwave radiation (W/m^2)      W/m2            1
# 4  swe      Snow water equivalent (kg/m^2)     kg/m2            1
# 5 tmax Maximum air temperature (degrees C) degrees C            1
# 6 tmin Minimum air temperature (degrees C) degrees C            1
# 7   vp           Water vapor pressure (Pa)        Pa            1
daymet_product <- "Daymet"
daymet_bands <- c("tmax", "tmin")

site_i <- 5000
daymet_query <- mt_subset(product = daymet_product,
                          band = daymet_bands,
                          lat = query_obs$lat[site_i],
                          lon = query_obs$lon[site_i],
                          start = query_obs$sdate[site_i],
                          end = query_obs$edate[site_i])

# Works:
ndvi_query <- mt_subset(product = "MOD13Q1",
                          band = "250m_16_days_NDVI",
                          lat = 49.03333333,
                          lon = -122.65,
                          start = "2019-01-01",
                          end = "2020-12-30")

# Doesn't work (request fails)
daymet_query <- mt_subset(product = daymet_product,
                        band = daymet_bands[1],
                        lat = 49.03333333,
                        lon = -122.65,
                        start = "2019-01-01",
                        end = "2020-12-30")

################################################################################
# Daymet temperature via Daymet Web services
# https://daymet.ornl.gov/web_services
################################################################################
# https://daymet.ornl.gov/single-pixel/api/data?lat=43.1&lon=-85.3&vars=prcp&start=2012-01-01&end=2012-01-31

library(lubridate) # getting starting & ending dates
library(dplyr)     # creating data for query & wrangling of results

# Test 1: POC
daymet_base_url <- "https://daymet.ornl.gov/single-pixel/api/data?"
daymet_url <- paste0(daymet_base_url,
                     "lat=", 49.03333333,
                     "&lon=", -122.65,
                     "&vars=tmin,tmax", 
                     "&start=", "2019-01-01",
                     "&end=", "2019-02-28")
daymet_data <- read.csv(daymet_url,
                        skip = 7) # Do not want the metadata header
daymet_data <- daymet_data %>%
  rename(tmax = "tmax..deg.c.",
         tmin = "tmin..deg.c.")
tmin_mean <- mean(x = daymet_data$tmin, na.rm = TRUE)
tmax_mean <- mean(x = daymet_data$tmax, na.rm = TRUE)

# Since we are using a web service, we want to do a couple of things:
#   1. Do some writing to disk *before* everything is done. Timeouts / errors 
#      should not erase any (or much) progress we made
#   2. Find out how much duplication of efforts = we only need to download data 
#      for individual lat/lon/date coordinates once. Only a little savings here
#      (9010 unique combos, down from 9223 total combos). Skip for now

# Test 2: do loop on subset of actual observations
all_obs <- read.csv(file = "data/filtered-obs.csv")

# Preparing columns we need for query
all_obs <- all_obs %>%
  mutate(month_start = lubridate::ymd(paste0(year, "-", month, "-01"))) %>%
  mutate(sdate = format(month_start %m-% months(1), "%Y-%m-%d"),
         edate = format(month_start %m+% months(1) - days(1), "%Y-%m-%d")) %>%
  select(latitude, longitude, sdate, edate, gbifID)

# 10 row test
# query_obs <- all_obs[sample(x = 1:nrow(all_obs), size = 10),]
# Sample 100 rows
# query_obs <- all_obs[sample(x = 1:nrow(all_obs), size = 100),]
# 100 observations takes about 1.75 minutes to process
# Sample 1000 rows
# query_obs <- all_obs[sample(x = 1:nrow(all_obs), size = 1000),]
# About 16 minutes when there are no queried values on disk
# All observations
query_obs <- all_obs

# Iterations between progress message
print_freq <- 100
# Iterations between 1-2 second sleep
sleep_freq <- 10
# Iterations between saves to disk
save_freq <- 100
# Maximum number of attempts for each observation
max_attempts <- 5

# We are being paranoid about the process crashing halfway through not having 
# written anything to disk. So we start by loading in whatever file is on disk
# to start and updating that data frame. If nothing exists on disk, create the 
# data frame to hold results
temperature_file <- "data/temperature-obs.csv"
if (file.exists(temperature_file)) {
  means_df <- read.csv(file = temperature_file)
} else {
  # Data frame to hold results, only need identifier (gbifID) and mean value
  means_df <- data.frame(gbifID = query_obs$gbifID,
                         tmin = NA_real_,
                         tmax = NA_real_)
}

# Base URL for queries to Daymet web services
daymet_base_url <- "https://daymet.ornl.gov/single-pixel/api/data?"
start_time <- Sys.time()
for (i in 1:nrow(query_obs)) {
  if (i %% print_freq == 0 || i == 1) {
    message("Querying ", i, " of ", nrow(query_obs))
  }
  if (i %% sleep_freq == 0) {
    Sys.sleep(runif(n = 1, min = 1, max = 2))
  }
  # First make sure we do not already have data for this observation on file
  gbifID <- query_obs$gbifID[i]
  row_on_disk <- which(means_df$gbifID == gbifID)
  # It is possible this ID hasn't been written to the file, we need to check
  proceed <- FALSE
  # GBIF ID isn't on disk (there is no corresponding row in the file), so we 
  # want to run the query
  if (length(row_on_disk) == 0) {
    proceed <- TRUE
  }
  # Even if the GBIF ID is on disk it might not have values, so see if either 
  # is missing and update proceed value as appropriate
  if (!proceed) {
    proceed <- is.na(means_df$tmin[row_on_disk]) |
      is.na(means_df$tmax[row_on_disk])
  }
  if (proceed) {
    # Missing one or both pieces of data, so go ahead with query
    # Build the query 
    daymet_url <- paste0(daymet_base_url,
                         "lat=", query_obs$latitude[i],
                         "&lon=", query_obs$longitude[i],
                         "&vars=tmin,tmax", 
                         "&start=", query_obs$sdate[i],
                         "&end=", query_obs$edate[i])
    num_attempts <- 0
    success <- FALSE
    while (num_attempts < max_attempts & !success) {
      tryCatch(expr = {
        num_attempts <- num_attempts + 1

        # Testing how error catching works
        # if ((i == 3 & num_attempts < 3) | i == 7) {
        #   stop("an error")
        # }
        daymet_data <- read.csv(daymet_url,
                                skip = 7) # Do not want the metadata header
        daymet_data <- daymet_data %>%
          rename(tmax = "tmax..deg.c.",
                 tmin = "tmin..deg.c.")
        tmin_mean <- mean(x = daymet_data$tmin, na.rm = TRUE)
        tmax_mean <- mean(x = daymet_data$tmax, na.rm = TRUE)
        if (length(row_on_disk) == 0) {
          # If there isn't a row on the disk, need to add it
          means_df <- means_df %>%
            bind_rows(list(gbifID = gbifID,
                           tmin = tmin_mean,
                           tmax = tmax_mean))
        } else {
          # If there is a row, update it
          means_df$tmin[row_on_disk] <- tmin_mean
          means_df$tmax[row_on_disk] <- tmax_mean
        }
        success <- TRUE # End of successful query & update
      }, 
      error = function(e) {
        message("Unsuccessful download for GBIF ID ", query_obs$gbifID[i], 
                " on attempt ", num_attempts, " of ", max_attempts)
        e
      },
      finally = NULL) # end of tryCatch
    } # End of while
    # If query was never successful, and the GBIF ID wasn't on disk, go ahead
    # and add a row of missing data for this ID
    if (!success & length(row_on_disk) == 0) {
      means_df <- means_df %>%
        bind_rows(list(gbifID = gbifID,
                       tmin = NA_real_,
                       tmax = NA_real_))
    } 
    # If appropriate, write the data to file. Plenty of duplication of effort 
    # here, but not so memory intensive to worry about
    if (i %% save_freq == 0) {
      write.csv(file = temperature_file,
                x = means_df,
                row.names = FALSE)
    }
  } else { # already have data on disk, so skip query
    message("Data already on disk for GBIF ID ", gbifID)
  }
}
Sys.time() - start_time

# Finally, do one more write to catch those last few
write.csv(file = temperature_file, 
          x = means_df,
          row.names = FALSE)
