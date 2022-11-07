# Test of spline and polynomial models
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-10-26

# Want to accommodate (potential) non-linear relationship between year and 
# flight/appearance day of year. It's possible that the effects of climate 
# change on one or more of the species has not been constant (i.e. think of the 
# hockey stick of CO2).

library(ggplot2)
library(lubridate)
library(splines)
library(dplyr)

set.seed(20221107)

# obs <- read.csv(file = "data/Pieris-virginiensis-spocc.csv")
obs <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")
# Some are missing dates. Drop those.
obs <- obs[!is.na(obs$year), ]
# obs$date <- as.Date(obs$date)

# Get the year and julian day
# obs$year <- lubridate::year(obs$date)
obs$yday <- lubridate::yday(as.Date(paste0(obs$year, 
                                           "-", 
                                           obs$month, 
                                           "-", 
                                           obs$day)))

# Some observations are *really* old; drop those, too
obs <- obs[obs$year >= 1960, ]

# Also have some observations for January 1, which is not realistic, so exclude
obs <- obs[obs$yday > 1, ]

################################################################################
# Start by running a series of models just for Pieris virginiensis:
#    1. Simple linear regression with year and latitude
#    2. 2- and 3- degree polynomials for year (latitude still linear)
#    3. Natural spline for year with 1 knot (df = 2) or 2 knots (df = 3)
#       Will need to do some cross-validation to figure out best knot 
#       placement, as default is to evenly distribute them (one knot by default 
#       creates a knot at 50%, two knots at 33.33% & 66.67%, etc)

########################################
# 1. Linear regression
model_1_lm <- lm(yday ~ year + latitude, data = obs)

# 2. Polynomial
model_2_1_poly2 <- lm(yday ~ poly(year, degree = 2) + latitude, data = obs)
model_2_2_poly3 <- lm(yday ~ poly(year, degree = 3) + latitude, data = obs)

########################################
# Model checks
summary(model_1_lm)
summary(model_2_1_poly2)
anova(model_1_lm, model_2_1_poly2) # 0.044
summary(model_2_2_poly3)
anova(model_2_1_poly2, model_2_2_poly3) # n.s.
anova(model_1_lm, model_2_2_poly3) # n.s.

# Do a quick bit of plotting to see the linear & polynomial results
# Need to create data frame with mean values of latitude
pred_data <- data.frame(year = obs$year,
                        yday = obs$yday,
                        latitude = mean(obs$latitude))
pred_data <- pred_data[order(pred_data$year), ]
pred_1 <- predict(model_1_lm, newdata = pred_data)
pred_2_1 <- predict(model_2_1_poly2, newdata = pred_data)
pred_2_2 <- predict(model_2_2_poly3, newdata = pred_data)
plot(x = obs$year, y = obs$yday)
lines(x = pred_data$year, y = pred_1)
lines(x = pred_data$year, y = pred_2_1, col = "blue")
lines(x = pred_data$year, y = pred_2_2, col = "red")

ggplot(data = pred_data, mapping = aes(x = year, y = yday)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, color = "green") +
  geom_smooth(method = lm, formula = y ~ poly(x, degree = 2)) +
  geom_smooth(method = lm, formula = y ~ poly(x, degree = 3), color = "#CC3377") +
  theme_bw()


################################################################################
# 3. Natural spline
# Quadratic polynomial, where we determine knot location with cross-validation
# on 10% of observations that are left out. Our data end up making pretty noisy
# MSE, so we will repeat the entire cross-validation approach 10 times.

# Need to try different knot locations on each 90% partition of the data, then 
# calculate the MSE for the remaining 10% of the data. For the knot locations 
# to test, we don't want knots right at the beginning or ending of our data, 
# let's say they need to be at least three years from beginning or end
knot_range <- c((min(obs$year) + 2):(max(obs$year) - 2))

num_reps <- 20
ns_mse_list <- list(num_reps)
for (rep_i in 1:num_reps) {
  message("Cross-validation replicate ", rep_i)
  
  # Assign each observation to a fold, randomizing just in case
  obs$fold <- sample(rep(x = 1:10, 
                         length.out = nrow(obs)),
                     replace = FALSE)
  
  # Matrix to hold MSE values for each knot/fold for natural spine
  ns_mse_mat <- matrix(data = NA, 
                       nrow = length(knot_range),
                       ncol = length(unique(obs$fold)))
  # Iterate over all possible knot locations
  for (knot_i in 1:length(knot_range)) {
    knot_location <- knot_range[knot_i]
    # message("Testing knot location ", knot_location)
    # Estimate model on training and calculate MSE on testing data, using each 
    # fold once as testing data
    for (test_fold_i in 1:length(unique(obs$fold))) {
      test_fold <- sort(unique(obs$fold))[test_fold_i]
      # message("...with fold ", test_fold, " as testing fold")
      train_data <- obs[obs$fold != test_fold, ]
      test_data <- obs[obs$fold == test_fold, ]
      # Estimate model with training data
      quad_model <- lm(yday ~ ns(year, df = 2, 
                                 knots = knot_location) + latitude,
                       data = train_data)
      # To calculate the MSE on testing data, need to make predictions and 
      # compare with actual; pred will be a vector of predicted yday values
      # Occasionally throws:
      # Warning message:
      #   In predict.lm(quad_model, newdata = test_data) :
      #   prediction from a rank-deficient fit may be misleading
      pred <- predict(quad_model, newdata = test_data)
      mse <- mean((test_data$yday - pred)^2)
      ns_mse_mat[knot_i, test_fold_i] <- mse
    }
  }
  # Summarize the MSE
  ns_mse_mean <- rowMeans(x = ns_mse_mat)
  ns_mse_list[[rep_i]] <- data.frame(year = knot_range,
                               mse = ns_mse_mean)
}

# Bind results from each replicate together
ns_mse_summary <- dplyr::bind_rows(ns_mse_list, .id = "replicate")
plot(x = ns_mse_summary$year, y = ns_mse_summary$mse)

# For each replicate, find the year with the minimum MSE
ns_mse_mins <- ns_mse_summary %>%
  group_by(replicate) %>%
  slice_min(order_by = mse)
  
# Take average year from the minimum MSEs
knot <- mean(ns_mse_mins$year)

# Run spline model with that year as knot
model_4_spline1 <- lm(yday ~ splines::ns(year, df = 2, 
                                         knots = knot) + latitude, 
                      data = obs)
summary(model_4_spline1)
ggplot(data = pred_data, mapping = aes(x = year, y = yday)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::ns(x, df = 2, knots = knot)) +
  theme_bw()

# Compare this spline model to linear & polynomial regression
anova(model_1_lm, model_4_spline1) # p = 0.028
anova(model_2_1_poly2, model_4_spline1) # Same number of parameters...

################################################################################
# Examples below here

# Example of regression spline on mtcars with quadratic & cubic polynomial
# with one knot
cars_cs_2 <- lm(mpg ~ bs(disp, df = 5, knots = 200, degree = 2), data = mtcars)
cars_cs_3 <- lm(mpg ~ bs(disp, df = 5, knots = 200, degree = 3), data = mtcars)
# attr(x = bs(mtcars$disp, df = 5, knots = 200), which = "knots")
# attributes(x = bs(mtcars$disp, df = 5, knots = 200))
summary(cars_cs_2)
summary(cars_cs_3)
anova(cars_cs_2, cars_cs_3) # n.s.
plot(x = mtcars$disp, y = mtcars$mpg)
pred_data <- data.frame(disp = mtcars$disp[order(mtcars$disp)],
                        mpg = mtcars$mpg[order(mtcars$disp)],
                        wt = mean(mtcars$wt)) # use mean for smooth line
preds_2 <- predict(cars_cs_2, newdata = pred_data)
preds_3 <- predict(cars_cs_3, newdata = pred_data)
lines(x = pred_data$disp, y = preds_2)
lines(x = pred_data$disp, y = preds_3, lty = 2)

# Example of natural splines on mtcars (two models: 1 knot and 2 knots)
cars_ns_2 <- lm(mpg ~ ns(disp, df = 2) + wt, data = mtcars)
summary(cars_ns_2)
cars_ns_3 <- lm(mpg ~ ns(disp, df = 3) + wt, data = mtcars)
summary(cars_ns_3)
plot(x = mtcars$disp, y = mtcars$mpg)
pred_data <- data.frame(disp = mtcars$disp[order(mtcars$disp)],
                        mpg = mtcars$mpg[order(mtcars$disp)],
                        wt = mean(mtcars$wt)) # use mean for smooth line
preds_2 <- predict(cars_ns_2, newdata = pred_data)
preds_3 <- predict(cars_ns_3, newdata = pred_data)
lines(x = pred_data$disp, y = preds_2)
lines(x = pred_data$disp, y = preds_3, lty = 2)

