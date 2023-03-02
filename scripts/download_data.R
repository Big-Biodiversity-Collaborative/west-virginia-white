# Download data from gbif and do QA/QC as necessary
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-10-28

library(dplyr)
library(lubridate)
source(file = "functions/query_gbif.R")

# Replace raw data if already on disk?
replace <- TRUE

# The earliest year of observations to include
min_year <- 1960

# restrict insect observations to adults
adults_only <- TRUE

# Remove observations seen on same day and same lat/lon
deduplicate <- TRUE

# Problematic GBIF ids; some are know to be misidentifcations, list here
gbif_id_remove <- c(3427558396, 3397466009, 3397667698)

# Geographic limits
# TODO: Need to justify these limits - might need to adopt an envelope approach
limit_lon <- c(-99, -45)
limit_lat <- c(15, 70)

# Taxa to download with GBIF identifier
taxon_keys <- c("Pieris virginiensis" = 5137890,
                "Cardamine concatenata" = 3046217,
                "Cardamine diphylla" = 3045717)
                # "Alliaria petiolata" = 5376075)

# Downloads data from GBIF (unless already present or force re-download)
for (i in 1:length(taxon_keys)) {
  # Make a computer-friendly name
  nice_name <- gsub(x = tolower(names(taxon_keys)[i]),
                    pattern = " ",
                    replacement = "_")
  raw_data_file <- paste0("data/", nice_name, "-gbif-raw.csv")
  # Only proceed with download if file is missing or if we explicitly want to 
  # replace it
  if (!file.exists(raw_data_file) | replace) {
    # Do the download from GBIF
    gbif_obs <- query_gbif(taxon_keys = taxon_keys[i],
                           verbose = TRUE,
                           lon_limits = limit_lon,
                           lat_limits = limit_lat)
    # Write the raw data to file
    write.csv(file = raw_data_file,
              x = gbif_obs,
              row.names = FALSE)
  } else {
    message("Skipping download for ", names(taxon_keys)[i], " file on disk.")
  }
  # Read in from file
  gbif_obs <- read.csv(file = raw_data_file)
  
  # See how many we start with to see how many are filtered out
  num_raw <- nrow(gbif_obs)

  # Drop any that do not have year information
  gbif_obs <- gbif_obs[!is.na(gbif_obs$year), ]

  # Apply time restrictions (year, January 1 observations)
  gbif_obs <- gbif_obs %>%
    filter(year >= min_year) %>%
    filter(yday(as.Date(paste0(year, "-", month, "-", day))) > 1)
  
  # Apply geographic restrictions
  gbif_obs <- gbif_obs %>%
    filter(between(decimalLongitude, 
                   left = limit_lon[1], 
                   right = limit_lon[2])) %>%
    filter(between(decimalLatitude, 
                   left = limit_lat[1], 
                   right = limit_lat[2]))

  # Rename lat & lon columns
  gbif_obs <- gbif_obs %>%
    rename(longitude = decimalLongitude,
           latitude = decimalLatitude)

  # Remove egg and larva records for insect
  if (names(taxon_keys)[i] == "Pieris virginiensis" & adults_only) {
    gbif_obs <- gbif_obs %>%
      filter(!(lifeStage %in% c("Egg", "Larva")))
  }

  # Remove duplicates if appropriate
  if (deduplicate) {
    gbif_obs <- gbif_obs %>%
      distinct(latitude, longitude, year, month, day, .keep_all = TRUE)
  }
  
  # Remove records known to be problematic (mis-ID'd)
  gbif_obs <- gbif_obs %>%
    filter(!(gbifID %in% gbif_id_remove))
    
  # Write these cleaned data to file
  write.csv(x = gbif_obs,
            file = paste0("data/", nice_name, "-gbif-clean.csv"))
  
  num_clean <- nrow(gbif_obs)
  message("Wrote ", num_clean, " cleaned observations of ", 
          names(taxon_keys)[i], ". Filtering removed ", (num_raw - num_clean), 
          " observations.")
}

# After download & cleaning (as appropriate), create zip files
raw_files <- list.files(path = "data", 
                        pattern = "*-gbif-raw.csv", 
                        full.names = TRUE)
zip(zipfile = "data/gbif-raw.zip",
    files = raw_files)
clean_files <- list.files(path = "data", 
                          pattern = "*-gbif-clean.csv", 
                          full.names = TRUE)
zip(zipfile = "data/gbif-clean.zip",
    files = clean_files)

