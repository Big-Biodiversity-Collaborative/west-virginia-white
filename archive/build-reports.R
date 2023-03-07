# Create several emergence reports
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2019-06-19

rm(list = ls())

################################################################################
# species_data <- data.frame(genus = c("Pieris", "Cardamine", "Cardamine"),
#                            species = c("virginiensis", "concatenata", "diphylla"))

# Small data frame, for testing
species_data <- data.frame(genus = c("Pieris"),
                           species = c("virginiensis"))

# Would be nice to do this via lapply, but the below doesn't work as anticipated
# reports <- lapply(X = species_data, 
#                   FUN = function(x){
#                     outfile <- paste0(x["genus"], "-", x["species"], "-emergence.pdf")
#                     rmarkdown::render(input = "templates/emergence-template.Rmd",
#                                       output_file = outfile,
#                                       params = list(genus_name = x$genus,
#                                                     species_name = x$species))
#                     return(NA)
#                   })

# So using old fashioned iteration
for (i in 1:nrow(species_data)) {
  genus <- species_data$genus[i]
  species <- species_data$species[i]
  basefile <- paste0("../output/", genus, "-", species, "-emergence")
  outfile <- paste0(basefile, ".pdf")
  message(paste0("Building report for ", genus, " ", species, "\n"))
  rmarkdown::render(input = "templates/emergence-template.Rmd",
                    output_file = outfile,
                    params = list(genus_name = genus,
                                  species_name = species))
  
  # Post-knit cleanup
  # need to remove tex files, since rmarkdown isn't doing it for us
  texfile <- paste0("output/", genus, "-", species, "-emergence.tex")
  if (file.exists(texfile)) {
    file.remove(texfile)
  }
  # And remove directories with files if they exist, too
  knit_folder <- paste0("output/", genus, "-", species, "-emergence_files")
  if (dir.exists(knit_folder)) {
    # Need to empty out folder first (if not already empty)
    knit_files <- list.files(path = knit_folder)
    if (length(knit_files) > 0) {
      for (one_file in knit_files) {
        file.remove(one_file)
      }
    }
    file.remove(knit_folder)
  }
}
