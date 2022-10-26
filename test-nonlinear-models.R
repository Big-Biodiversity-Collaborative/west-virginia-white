# Test of spline and polynomial models
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-10-26

# Want to accommodate (potential) non-linear relationship between year and 
# flight/appearance day of year. It's possible that the effects of climate 
# change on one or more of the species has not been constant (i.e. think of the 
# hockey stick of CO2).

# Start by running a series of models just for Pieris virginiensis:
#    1. Simple linear regression with year and latitude
#    2. 2- and 3- degree polynomials for year (latitude still linear)
#    3. Natural spline for year with 1 knot (df = 2) or 2 knots (df = 3)
#       Will need to do some cross-validation to figure out best knot 
#       placement, as default is to evenly distribute them (one knot by default 
#       creates a knot at 50%, two knots at 33.33% & 66.67%, etc)

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
