#' Download records from GBIF for a set of taxa
#' 
#' @param taxon_keys numeric vector of taxon keys to search for
#' @param verbose logical indicating whether or not to print status messages
#' @param cols columns to retain; if \code{NULL}, returns all columns that are 
#' returned from a call to \code{rgbif::occ_search}
#' 
#' @details This function is modified from the function of the same name at 
#' https://github.com/Big-Biodiversity-Collaborative/BotanicGardenHotspot
#' 
#' @return data frame of observations returned from GBIF
query_gbif <- function(taxon_keys, verbose = FALSE,
                       cols = c("decimalLatitude", "decimalLongitude",
                                "individualCount", "family", "species", "year", 
                                "month", "day", "datasetName", "gbifID", 
                                "lifeStage")) {
  if (!require(rgbif)) {
    stop("GBIF queries require the rgbif library")
  }
  if (!require(dplyr)) {
    stop("GBIF queries require the dplyr library")
  }
  
  # Count number of observations in the rectangle, as pagination might be 
  # necessary; actually performs one search per taxonKey value
  gbif_count <- rgbif::occ_search(taxonKey = taxon_keys,
                                  limit = 1)

  # For each taxon, get count and paginate as necessary
  gbif_obs_list <- list()
  for (taxon_key in taxon_keys) {
    # Pull out the total count of records for this taxon
    # If there was only one taxon key, returned value is a result list with 
    # five elements, but if there is more than one taxon_key, the result is a 
    # list of lists
    if (length(taxon_keys) > 1) {
      taxon_count <- gbif_count[[as.character(taxon_key)]]$meta$count
    } else {
      taxon_count <- gbif_count$meta$count
    }
    if (taxon_count > 0) {
      page <- 1
      start <- 0
      while(start <= taxon_count) {
        if (verbose) {
          message(paste0("Downloading ", (start + 1), "-", 
                         min((start+300), taxon_count), " of ", 
                         taxon_count, " for taxon key ", taxon_key))
        }
        gbif_obs <- rgbif::occ_search(taxonKey = taxon_key,
                                      start = start,
                                      limit = 300)
        if (page == 1) {
          gbif_obs_list[[as.character(taxon_key)]] <- gbif_obs$data
        } else {
          gbif_obs_list[[as.character(taxon_key)]] <- dplyr::bind_rows(gbif_obs_list[[as.character(taxon_key)]],
                                                                       gbif_obs$data)
        }
        page <- page + 1
        start <- (page - 1) * 300
      }
    } else {
      gbif_obs_list[[as.character(taxon_key)]] <- NULL
    }
  }
  # We now have a list with observations for all taxa; combine to single data 
  # frame
  all_obs <- dplyr::bind_rows(gbif_obs_list)
  
  # Restrict columns as necessary
  if (nrow(all_obs) > 0 & length(cols) > 0) {
    # Some results returned from GBIF do not always include all columns 
    # (e.g. individualCount doesn't always come through); only select on 
    # columns that do exist
    all_obs <- all_obs %>%
      dplyr::select(dplyr::intersect(cols, colnames(all_obs)))
  }
  return(all_obs)
}