# Prepare data for analyses and plotting
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-06

require(dplyr)
require(lubridate) # Julian days
require(ks)        # Estimating density envelope

# Restricts observations to 
#   + those occurring in specific time envelope (see min_year & max_year)
#   + those occurring within a density envelope of P. virginiensis observations 
#     (see density_cutoff for percentage)
#   + those occurring before the first day of summer of the year (optional, see
#     pre_summer); generally not necessary? Points that would be filtered out 
#     by this criterion are already gone after year and density envelope 
#     filters are applied

# Set up filter values
min_year <- 2000
max_year <- 2023
# If true, will exclude observations after June 21 of each year
pre_summer <- FALSE
# Cutoff for inclusion in density envelope
density_cutoff <- 0.95
# Reality check plot (TRUE will run code to print plot)
reality_check <- FALSE

# Load in insect data
insect <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")
insect$organism = "insect"

# Load data for three host plants
host_concatenata <- read.csv(file = "data/cardamine_concatenata-gbif-clean.csv")
host_diphylla <- read.csv(file = "data/cardamine_diphylla-gbif-clean.csv")
# GBIF has separate entries for one of the host plants (synonyms) that *do not* 
# come through on a query of the accepted name
host_laevigata <- read.csv(file = "data/borodinia_laevigata-gbif-clean.csv")
b_laevigata_sp <- host_laevigata$species[1]
host_A_laevigata <- read.csv(file = "data/arabis_laevigata-gbif-clean.csv")
host_laevigata <- host_laevigata %>%
  bind_rows(host_A_laevigata) %>%
  mutate(species = b_laevigata_sp)

# Combine hosts and add label for organism (insect vs host)
hosts <- host_concatenata %>%
  bind_rows(host_diphylla) %>%
  bind_rows(host_laevigata) %>%
  mutate(organism = "host")

# Combine insect and hosts
all_obs <- insect %>%
  bind_rows(hosts) %>%
  mutate(julian_day = yday(as.Date(paste(year, month, day, sep = "-")))) %>%
  mutate(organism = factor(organism, levels = c("insect", "host")))

# Run filtering for years and seasons as appropriate
all_obs <- all_obs %>%
  filter(year <= max_year) %>%
  filter(year >= min_year)

# Write some basic information about number of observations to file (additional 
# info written at tend of file, too
counts_file <- "output/observation-counts.txt"
sink(file = counts_file)
cat("Total 2000-2022 observations :")
nrow(all_obs)
cat("Total 2000-2022 WVW observations :")
all_obs %>%
  filter(species == "Pieris virginiensis") %>%
  nrow()
sink()

if (pre_summer) {
  all_obs <- all_obs %>%
    filter(month <= 6) %>% # Drop any after June
    filter(month < 6 | day < 21) # Drop any after June 21
}

# Set Pieris virginiensis and insect as reference levels
all_obs <- all_obs %>%
  mutate(organism = factor(organism, levels = c("insect", "host"))) %>%
  mutate(species = factor(species, levels = c("Pieris virginiensis",
                                              "Cardamine concatenata",
                                              "Cardamine diphylla",
                                              "Borodinia laevigata")))

################################################################################
# Density envelope
# Determine density envelope for insect observations
insect_matrix <- all_obs %>%
  filter(organism == "insect") %>%
  dplyr::select(longitude, latitude) %>%
  as.matrix()

# Hns & Hbcv pretty similar to one another, Hns much faster
# bandwidth <- ks::Hns(insect_matrix)
# bandwidth <- ks::Hbcv(insect_matrix)
# Hscv and Hpi nearly identical
# bandwidth <- ks::Hscv(insect_matrix)
bandwidth <- ks::Hpi(insect_matrix)
kd_estimate <- ks::kde(x = insect_matrix, H = bandwidth)

# We have the density envelope, and now predict it for ALL observation points, 
# insect and hosts
all_matrix <- all_obs %>%
  dplyr::select(longitude, latitude) %>%
  as.matrix()
kd_predicted <- predict(object = kd_estimate, x = all_matrix)

# Vector indicating if points are included or excluded, based on our envelope
# cutoff. Values are stored as text with a percentage sign, hence gymnastics
min_density <- (1 - density_cutoff) * 100
within_envelope <- kd_predicted >= kd_estimate$cont[paste0(min_density, "%")]

# Create new data object with only those observations (insect and host) that 
# fall within the desired density envelope of the insect
envelope_obs <- all_obs[within_envelope, ]

# Reality check
if (reality_check) {
  # Plot all observations
  plot(latitude ~ longitude, data = all_obs,
       col = "gray80", pch = 19, cex = 0.5, las = 1, xlab = "", ylab = "")
  # Add just the ones in the envelope as darker
  points(latitude ~ longitude, data = envelope_obs,
         col = "gray60", pch = 19, cex = 0.5)
  # Add in insect points as even darker
  points(latitude ~ longitude, data = all_obs %>% filter(organism == "insect"),
         col = "gray20", pch = 19,
         cex = 0.5, las = 1, xlab = "", ylab = "")
  # Draw density envelope
  plot(kd_estimate, cont = (density_cutoff * 100),
       drawlabels = FALSE, col = "blue", add = TRUE)
}

write.csv(x = envelope_obs,
          file = "data/filtered-obs.csv",
          row.names = FALSE)

sink(file = counts_file, append = TRUE)
cat("Filtered 2000-2022 observations :")
nrow(envelope_obs)
cat("Filtered 2000-2022 WVW observations :")
envelope_obs %>%
  filter(species == "Pieris virginiensis") %>%
  nrow()
sink()
