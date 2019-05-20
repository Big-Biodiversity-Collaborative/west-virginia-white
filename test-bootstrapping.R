# Testing bootstrap sampling to deal with unequal sampling across years
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-05-06

rm(list = ls())

################################################################################
library(tidyverse)
library(lubridate)
library(gridExtra)

# Read in iNaturalist observations
# West Virginia White
wvw <- read.csv(file = "data/observations-52593.csv")

wvw$observed_on <- as.Date(wvw$observed_on)

# Extract year and day of year
wvw$year <- year(wvw$observed_on)
wvw$yday <- as.POSIXlt(wvw$observed_on)$yday

# Remove observations for 2019 and before 2005
wvw.2005.2018 <- wvw[wvw$year >= 2005 & wvw$year < 2019, ]

# Plot observations for each year separately
obs.plot <- ggplot(data = wvw.2005.2018, mapping = aes(x = year, y = yday, color = latitude)) +
  geom_point() +
  geom_smooth() +
  xlab(label = "Year") +
  ylab(label = "Day") +
  ggtitle("Observations 2005-2018") +
  scale_color_continuous(name = "Latitude") +
  theme_bw()
suppressMessages(expr = print(obs.plot))

# Plot _only_ minimums
wvw.mins <- wvw.2005.2018 %>%
  group_by(year) %>%
  summarise(early = min(yday, na.rm = TRUE),
            n.obs = n())

min.plot <- ggplot(data = wvw.mins, mapping = aes(x = year, y = early)) +
  geom_point() +
  geom_smooth()  +
  xlab(label = "Year") +
  ylab(label = "Day") +
  ggtitle("Earliest observations 2005-2018") + 
  theme_bw()
suppressMessages(expr = print(min.plot))

################################################################################
# Look at bootstrapping for earliest date
# General idea:
# Interest in butterfly watching has increased with time, so if we observe 
# earlier emergence dates through time, it could be due to climate change, but 
# it could also just be a sampling artifact.
# Test to see if bootstrapping with avoid this artifact by bootstrapping from 
# the most recent year's data to re-create the sampling efforts for each 
# previous year
# Process: Create vector of observations from most recent year, `sample.from`, 
# this will be the distribution to sample from. For each year that is included, 
# draw some number of samples, `sample.size`, from `sample.from`. For each year,
# extract the minimum value and run linear regression one earliest ~ year to 
# test for an effect. Plot the coefficient estimate and p-value densities

minimum.required <- 5
sample.size <- 5
bootstrap.reps <- 1000
sample.from <- wvw.2005.2018$yday[wvw.2005.2018$year == 2018]
include.years <- wvw.mins$year[wvw.mins$n.obs >= minimum.required]
wvw.for.bs <- wvw.2005.2018[wvw.2005.2018$year %in% include.years, ]

# Replicate process of sampling and linear regression
bs.results <- data.frame(replicate = 1:bootstrap.reps,
                         estimate = NA,
                         p.value = NA)
for (r in 1:bootstrap.reps) {
  # Data fram will hold bootstrapped data
  bootstrapped.df <- data.frame(year = rep(include.years, times = sample.size),
                                yday = NA)
  # Fill in data frame with bootstrapped date for each year
  for (yr in include.years) {
    bootstrapped.df$yday[bootstrapped.df$year == yr] <- sample(x = sample.from,
                                                               size = sample.size,
                                                               replace = FALSE)
  }

  bs.mins <- bootstrapped.df %>%
    group_by(year) %>%
    summarise(early = min(yday))
  
  bs.lm <- lm(early ~ year, data = bs.mins)
  bs.results$estimate[r] <- summary(bs.lm)$coefficients[2, 1]
  bs.results$p.value[r] <- summary(bs.lm)$coefficients[2, 4]
}

# Trying a parallel approach to bootstrapping
library(parallel)

# Ultimately, returning a vector of two elements
# the coefficient estimate and the p

# Example
library(parallel)
# Calculate the number of cores
num.cores <- detectCores() - 1
comp.cluster <- makeCluster(num.cores)

# Establish sampling design parameters
minimum.required <- 5
sample.size <- minimum.required
bootstrap.reps <- 10

sample.from <- wvw.2005.2018$yday[wvw.2005.2018$year == 2018]
include.years <- wvw.mins$year[wvw.mins$n.obs >= minimum.required]

# Example
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
mydf <- data.frame(x = c(1:10),
                   y = c(1:10) + rnorm(n = 10))
num.reps <- c(1:10)
exponent <- parLapply(cl = cl, 
                      # simplify = FALSE,
                      X = num.reps, # This vector ends up being first argument of FUN
                      fun = function(i, df) {
                        data.x <- sample(x = df[, "x"], size = 5)
                        data.y <- sample(x = df[, "y"], size = 5)
                        df.lm <- lm(data.y ~ data.x)
                        coeff.est <- summary(df.lm)$coefficients[2, 1]
                        p.value <- summary(df.lm)$coefficients[2, 4]
                        return(data.frame(bs.rep = i,
                                    estimate = coeff.est,
                                    p.value = p.value))
                      },
                      df = mydf)
stopCluster(cl)
rm(cl)
exponent.df <- do.call(rbind, exponent)

# Plot results of bootstrapping
# Coefficient estimates
estimate.plot <- ggplot(data = bs.results, mapping = aes(x = estimate)) +
  geom_histogram() +
  geom_vline(xintercept = 0) +
  theme_bw()
suppressMessages(expr = print(estimate.plot))

# P-values
p.value.plot <- ggplot(data = bs.results, mapping = aes(x = p.value)) +
  geom_histogram() +
  geom_vline(xintercept = 0.05, color = "#FF0000") +
  theme_bw()
suppressMessages(expr = print(p.value.plot))

################################################################################
# Explicitly testing for artifact effect
# Measure the potential for this artifact by testing for a earlier emergence 
# time when bootstrapping from the most recent year's data to re-create the 
# sampling efforts for each previous year
# Process: Create vector of observations from most recent year, `sample.from`, 
# this will be the distribution to sample from. For each year that is included, 
# draw the a number of samples that corresponds to actual number of samples 
# per year, calculate the minimum, and test for an effect. _Should_ see lower 
# minimums in more recent years, due to larger sample sizes.

# Set up number of reps and distribution to sample from
bootstrap.reps <- 1000
sample.from <- wvw.2005.2018$yday[wvw.2005.2018$year == 2018]
include.years <- wvw.mins$year[wvw.mins$n.obs >= minimum.required]
wvw.for.bs <- wvw.2005.2018[wvw.2005.2018$year %in% include.years, ]

# Replicate process of sampling and linear regression
bs.results <- data.frame(replicate = 1:bootstrap.reps,
                         estimate = NA,
                         p.value = NA)

for (r in 1:bootstrap.reps) {
  # Erase values in yday; these will be replaced via bootstrap sampling
  wvw.for.bs$yday <- NA
  # Should be a tidyverse way of doing this...
  for (y in unique(wvw.for.bs$year)) {
    num.obs <- sum(wvw.for.bs$year == y)
    wvw.for.bs$yday[wvw.for.bs$year == y] <- sample(x = sample.from, size = num.obs)
  }
  bs.mins <- wvw.for.bs %>%
    group_by(year) %>%
    summarise(early = min(yday))
  
  suppressMessages(expr = bs.lm <- lm(early ~ year, data = bs.mins))
  bs.results$estimate[r] <- summary(bs.lm)$coefficients[2, 1]
  bs.results$p.value[r] <- summary(bs.lm)$coefficients[2, 4]
}

# Plot results of bootstrapping
# Coefficient estimates
estimate.plot <- ggplot(data = bs.results, mapping = aes(x = estimate)) +
  geom_histogram() +
  geom_vline(xintercept = 0, color = "#FF0000") +
  theme_bw()
suppressMessages(expr = print(estimate.plot))
# P-values
p.value.plot <- ggplot(data = bs.results, mapping = aes(x = p.value)) +
  geom_histogram() +
  geom_vline(xintercept = 0.05, color = "#FF0000") +
  theme_bw()
suppressMessages(expr = print(p.value.plot))

########################################
# Now need to do reciprocal test, where there **is** an effect of time on 
# earliest emergence date. Want to test a few effect sizes; i.e. a shift of 
# 0.1, 0.25, 0.5, 1 days earlier per year. Include a shift of 0 to make the 
# control abstract. Then use boxplots to summarize coefficient estimate 
# distributions at different effect sizes

# Effect size in change of # days per year
effect.sizes <- c(0, 0.1, 0.5, 1, 2, 2.5)
bootstrap.reps <- 100
sample.size <- 5
bs.effect.results <- data.frame(effect.size = rep(effect.sizes, 
                                                  times = bootstrap.reps),
                                replicate = NA,
                                estimate = NA)
for (effect.size in effect.sizes) {
  # Replicate process of sampling and linear regression
  bs.results <- data.frame(replicate = 1:bootstrap.reps,
                           estimate = NA)
  for (r in 1:bootstrap.reps) {
    # Data fram will hold bootstrapped data
    bootstrapped.df <- data.frame(year = rep(include.years, times = sample.size),
                                  yday = NA)
    # Fill in data frame with bootstrapped date for each year
    for (yr in include.years) {
      bootstrapped.df$yday[bootstrapped.df$year == yr] <- sample(x = sample.from,
                                                                 size = sample.size,
                                                                 replace = FALSE)
      # Now change yday by the effect size
      year.diff <- max(include.years) - yr
      bootstrapped.df$yday[bootstrapped.df$year == yr] <- 
        bootstrapped.df$yday[bootstrapped.df$year == yr] + effect.size * year.diff
    }
    
    bs.mins <- bootstrapped.df %>%
      group_by(year) %>%
      summarise(early = min(yday))
    
    bs.lm <- lm(early ~ year, data = bs.mins)
    bs.results$estimate[r] <- summary(bs.lm)$coefficients[2, 1]
  }
  bs.effect.results[bs.effect.results$effect.size == effect.size, c("replicate", "estimate")] <-
    bs.results
}

effect.size.plot <- ggplot(data = bs.effect.results, mapping = aes(x = as.factor(effect.size), y = estimate)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, color = "red") + 
  xlab(label = "Effect size (days/yr)") +
  ylab(label = "Estimated coefficent estimate") +
  theme_bw()
print(effect.size.plot)
