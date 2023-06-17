# Download monthly temperature data for observations based on lat/long
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-06-12

library(jsonlite)  # creating json objects & extracting json responses
library(RCurl)     # posting query
library(lubridate) # getting starting & ending dates
library(dplyr)     # creating data for query

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
