# Download monthly temperature data for observations based on lat/long
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-06-12

library(jsonlite)  # creating json objects & extracting json responses
library(RCurl)     # posting query
library(lubridate) # getting starting & ending dates
library(dplyr)     # creating data for query

# Using resources from https://www.rcc-acis.org/docs_webservices.html, 
# specificially will be using gridded data - documentation available at 
# https://www.rcc-acis.org/docs_webservices.html#title24

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
  select(loc, sdate, edate) %>%
  mutate(grid = "21",
         elems = "avgt")

some_obs <- query_obs[1:200, ]
print_freq <- 100
sleep_freq <- 5
for (i in 1:nrow(some_obs)) {
  # if (i %% print_freq == 0 || i == 1) {
    message("Querying ", i, " of ", nrow(some_obs))
  # }
  if (i %% sleep_freq == 0) {
    Sys.sleep(1)
  }
  # Pull out single row of observations
  x <- some_obs[i, ]

  # Turn the data frame into a json object each row becomes an element
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
  # First column is date, second is temperature (everything returned is text)
  mean_value <- mean(as.numeric(query_result[, 2]), na.rm = TRUE)
}

#' Query web service and calculate mean temperature
calc_temp <- function(df) {
  
}

lon <- -78.5
lat <- 39.2
grid <- 21
base <- 40
sdate <- "2020-03-01"
edate <- "2020-04-30"

jsonQuery=paste0('{"loc":"',lon,',',lat,'","grid":"21","elems":"avgt","sdate":"',sdate,'","edate":"',edate,'"}')

paste_result <- postForm(uri = "http://data.rcc-acis.org/GridData",
                        .opts = list(postfields = jsonQuery,
                                     httpheader = c("Content-Type" = "application/json",
                                                    Accept = "application/json")))

df_result_out <- as.data.frame(jsonlite::fromJSON(df_result))
paste_result_out <- as.data.frame(jsonlite::fromJSON(paste_result))

jsonQuery=paste0('{"loc":"',lon,',',lat,'","grid":"21","elems":"gdd',base,',pcpn","sdate":"',sdate,'","edate":"',edate,'"}')
out<-postForm("http://data.rcc-acis.org/GridData",
              .opts = list(postfields = jsonQuery,
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)
temp<-as.data.frame(out$data)
