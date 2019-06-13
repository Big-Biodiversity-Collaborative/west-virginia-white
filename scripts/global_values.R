#' Global settings for analyses and visualization
#' 
#' @return list of named elements with values to use for analyses and data 
#' visualization
#' @details
#' \itemize{
#'   \item \code{minimum_required} minimum number of required samples in a year for analyses
#'   \item \code{bs_sample_size}   sample size to use for bootstrapping; should be less than or equal to \code{minimum_required}
#'   \item \code{num_bs_reps}      number of bootstrap replicates to run
#'   \item \code{min_year}         minimum year to include in observations (inclusive)
#'   \item \code{max_year}         maximum year to include in observations (inclusive)
#'   \item \code{min_julian}       earliest day of year to include in observations (inclusive)
#'   \item \code{max_julian}       latest day of year to include in observations (inclusive)
#' }
global_values <- function() {
  values <- list(minimum_required = 5,
                 bs_sample_size = 5,
                 num_bs_reps = 1000,
                 min_year = 1950,
                 max_year = as.integer(format(Sys.Date(), "%Y")) - 1, # previous year
                 min_julian = 1,
                 max_julian = 366)
  return(values)
}