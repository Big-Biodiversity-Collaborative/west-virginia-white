# Download monthly temperature data for observations based on lat/long
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-06-12

library(lubridate) # getting starting & ending dates
library(dplyr)     # creating data for query

################################################################################
# Daymet temperature via Daymet Web services
# https://daymet.ornl.gov/web_services

# Test 1: POC
# daymet_base_url <- "https://daymet.ornl.gov/single-pixel/api/data?"
# daymet_url <- paste0(daymet_base_url,
#                      "lat=", 49.03333333,
#                      "&lon=", -122.65,
#                      "&vars=tmin,tmax", 
#                      "&start=", "2019-01-01",
#                      "&end=", "2019-02-28")
# daymet_data <- read.csv(daymet_url,
#                         skip = 7) # Do not want the metadata header
# daymet_data <- daymet_data %>%
#   rename(tmax = "tmax..deg.c.",
#          tmin = "tmin..deg.c.")
# tmin_mean <- mean(x = daymet_data$tmin, na.rm = TRUE)
# tmax_mean <- mean(x = daymet_data$tmax, na.rm = TRUE)

####################
# Real data
all_obs <- read.csv(file = "data/filtered-obs.csv")

# Preparing columns we need for query
all_obs <- all_obs %>%
  # Approach for two month span: observation month & prior month
  # mutate(month_start = lubridate::ymd(paste0(year, "-", month, "-01"))) %>%
  # mutate(sdate = format(month_start %m-% months(1), "%Y-%m-%d"),
  #        edate = format(month_start %m+% months(1) - days(1), "%Y-%m-%d")) %>%
  # Approach for 60 day span, where last day is date of observation
  # mutate(edate = lubridate::ymd(paste0(year, "-", month, "-", day))) %>%
  # mutate(sdate = format(edate %m-% days(60), "%Y-%m-%d")) %>%
  # Approach for "winter/spring" span of four months, February 1 - May 31
  mutate(sdate = lubridate::ymd(paste0(year, "-02-01")),
         edate = lubridate::ymd(paste0(year, "-05-31"))) %>%
  select(latitude, longitude, sdate, edate, gbifID)

# Tests: do loop on subset of actual observations

# 10 row test
# query_obs <- all_obs[sample(x = 1:nrow(all_obs), size = 10),]
# Sample 100 rows
# query_obs <- all_obs[sample(x = 1:nrow(all_obs), size = 100),]
# 100 observations takes about 1.75 minutes to process
# Sample 1000 rows
# query_obs <- all_obs[sample(x = 1:nrow(all_obs), size = 1000),]
# About 16 minutes when there are no queried values on disk

# Doing it for real
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
