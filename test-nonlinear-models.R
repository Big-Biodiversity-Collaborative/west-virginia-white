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

# obs <- read.csv(file = "data/Pieris-virginiensis-spocc.csv")
obs <- read.csv(file = "data/pieris_virginiensis-gbif-clean.csv")
# Some are missing dates. Drop those.
obs <- obs[!is.na(obs$year), ]
# obs$date <- as.Date(obs$date)

# Get the year and julian day
# obs$year <- lubridate::year(obs$date)
obs$yday <- lubridate::yday(as.Date(paste0(obs$year, "-", obs$month, "-", obs$day)))

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
anova(model_1_lm, model_2_1_poly2)
summary(model_2_2_poly3)
anova(model_2_1_poly2, model_2_2_poly3) # n.s.

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

# TODO: Really need to try better knot locations
# 3. Natural spline
model_3_spline1 <- lm(yday ~ splines::ns(year, df = 2) + latitude, data = obs)
model_3_spline2 <- lm(yday ~ splines::ns(year, df = 3) + latitude, data = obs)
anova(model_3_spline1, model_3_spline2) # P = 3e-06
anova(model_2_2_poly3, model_3_spline2) # n.s.
ggplot(data = pred_data, mapping = aes(x = year, y = yday)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::ns(x, df = 2)) +
  geom_smooth(method = lm, formula = y ~ splines::ns(x, df = 3), color = "#88EE55") +
  geom_smooth(method = lm, formula = y ~ poly(x, degree = 3), color = "#CC3377") +
  theme_bw()


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

