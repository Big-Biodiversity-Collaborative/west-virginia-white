# Approximate range maps for insect and hosts
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-03-06

# TODO: Really early proof of concept here

require(dplyr)
require(tidyr)
require(lubridate) # Julian days
require(ks)        # Estimating density envelope
require(ggplot2)

# Set up filter values
min_year <- 2000
max_year <- 2022
# If true, will exclude observations after June 21 of each year
pre_summer <- TRUE
# Cutoff for inclusion in density envelope
density_cutoff <- 0.95

# Load in data
insect <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")
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

# Make a single dataset for easier filtering
all_obs <- insect %>%
  bind_rows(host_concatenata) %>%
  bind_rows(host_diphylla) %>%
  bind_rows(host_laevigata) %>%
  mutate(julian_day = yday(as.Date(paste(year, month, day, sep = "-"))))

# Run filtering for years and seasons as appropriate
all_obs <- all_obs %>%
  filter(year <= max_year) %>%
  filter(year >= min_year)

if (pre_summer) {
  all_obs <- all_obs %>%
    filter(month <= 6) %>% # Drop any after June
    filter(month < 6 | day < 22) # Drop any after June 21
}

# For each species, calculate the XX% density envelope
species_names <- unique(all_obs$species)

# Create map with all those envelopes in the figure

# Will keep the plot restricted to P. virginiensis range, so that one gets
# plotted first; this makes sure the plot gets started and not overwritten
plot_info <- data.frame(species = species_names,
                        colors = c("#7b3294", "#5c5c5c","#5c5c5c", "#5c5c5c"))
plot_drawn <- FALSE
for (i in 1:nrow(plot_info)) {
  species_name <- plot_info$species[i]
  species_matrix <- all_obs %>%
    filter(species == species_name) %>%
    dplyr::select(longitude, latitude) %>%
    as.matrix()  

  bandwidth <- ks::Hpi(species_matrix)
  kd_estimate <- ks::kde(x = species_matrix, H = bandwidth)

  species_color <- plot_info$colors[i]
  plot(kd_estimate, cont = (density_cutoff * 100),
       drawlabels = FALSE, display = "filled.contour",
       col = c("white", species_color), alpha = c(0, 0.5),
       add = plot_drawn)
  # Add solid lines for insect
  if (!plot_drawn) {
    plot(kd_estimate, cont = (density_cutoff * 100),
         drawlabels = FALSE, display = "slice",
         col = species_color, add = TRUE, lwd = 2)
  }
  plot_drawn <- TRUE
}
# One more plot of P. virginiensis lines
species_name <- plot_info$species[1]
species_matrix <- all_obs %>%
  filter(species == species_name) %>%
  dplyr::select(longitude, latitude) %>%
  as.matrix()  

bandwidth <- ks::Hpi(species_matrix)
kd_estimate <- ks::kde(x = species_matrix, H = bandwidth)
plot(kd_estimate, cont = (density_cutoff * 100),
     drawlabels = FALSE, display = "slice",
     col = plot_info$colors[1], add = TRUE, lwd = 2)

##### A different approach with three plots, one for each host plant
par(mfrow = c(2, 2))
insect_matrix <- all_obs %>%
  filter(species == plot_info$species[1]) %>%
  dplyr::select(longitude, latitude) %>%
  as.matrix()
insect_bandwidth <- ks::Hpi(insect_matrix)
insect_estimate <- ks::kde(x = insect_matrix, H = insect_bandwidth)
insect_color <- plot_info$colors[1]

for (i in 2:nrow(plot_info)) {
  host_name <- plot_info$species[i]
  
  # Add insect range as line, plot this first to get the correct map limits
  plot(insect_estimate, cont = (density_cutoff * 100),
       drawlabels = FALSE, display = "slice",
       col = insect_color, lwd = 2, main = host_name)

  host_matrix <- all_obs %>%
    filter(species == host_name) %>%
    dplyr::select(longitude, latitude) %>%
    as.matrix()  
  
  bandwidth <- ks::Hpi(host_matrix)
  kd_estimate <- ks::kde(x = host_matrix, H = bandwidth)
  
  host_color <- plot_info$colors[i]
  
  plot(kd_estimate, cont = (density_cutoff * 100),
       drawlabels = FALSE, display = "filled.contour",
       col = c("white", host_color), alpha = c(0, 0.5), add = TRUE)
  
  # Add insect range as line
  # TODO: Not sure this is necessary...
  plot(insect_estimate, cont = (density_cutoff * 100),
       drawlabels = FALSE, display = "slice",
       col = insect_color, lwd = 2, add = TRUE)
}
par(mfrow = c(1, 1))

# If interested in just one host species + insect, can use this code
# "Cardamine concatenata"
# "Cardamine diphylla"
# "Borodinia laevigata"
host_name <- "Borodinia laevigata"
species_names <- c("Pieris virginiensis", host_name)

plot_info <- data.frame(species = species_names,
                        colors = c("#7b3294", "#5cbb5c"))
plot_drawn <- FALSE
for (i in 1:nrow(plot_info)) {
  species_name <- plot_info$species[i]
  species_matrix <- all_obs %>%
    filter(species == species_name) %>%
    dplyr::select(longitude, latitude) %>%
    as.matrix()  
  
  bandwidth <- ks::Hpi(species_matrix)
  kd_estimate <- ks::kde(x = species_matrix, H = bandwidth)
  
  species_color <- plot_info$colors[i]
  plot(kd_estimate, cont = (density_cutoff * 100),
       drawlabels = FALSE, display = "filled.contour",
       col = c("white", species_color), alpha = c(0, 0.5),
       add = plot_drawn)
  # Add solid lines for insect
  if (!plot_drawn) {
    plot(kd_estimate, cont = (density_cutoff * 100),
         drawlabels = FALSE, display = "slice",
         col = species_color, add = TRUE, lwd = 2)
  }
  plot_drawn <- TRUE
}
