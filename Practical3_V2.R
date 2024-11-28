# Package loading
library(here)
library(fpp2)
library(dplyr)
library(ismev)
library(tseries)
library(extRemes)
library(lubridate)

# Import the data from our .csv
american_airlines_data <- read.csv(here::here("flights_sample_3m_filtered.csv"))

# Convert the date in a date format
american_airlines_data$DATE <- as.Date(american_airlines_data$FL_DATE, format = "%Y-%m-%d")

# Calculate max departure delay per day
daily_max_delay <- american_airlines_data %>%
  group_by(FL_DATE) %>%
  summarise(max_delay = max(DEP_DELAY, na.rm = TRUE))

# Transform the dataframe into a time series
daily_max_delay_ts <- ts(daily_max_delay$max_delay, frequency = 365)

# Visualization of the time series (Change the legend y axis and values x axis)
# Observation of trend lower between 2 and 3, higher after that with peak at 5
# Observation of patterns : not obvious
# Delays are more concentrated around the lower range (0–25 minutes) for most days
# Peak at 5 (see what it means in date to give an explanation)
autoplot(daily_max_delay_ts)

# Histogram with our max daily delay
ggplot(data.frame(daily_max_delay_ts), aes(x = daily_max_delay_ts)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of the max daily delay",
       x = "Daily max delay (min)",
       y = "Frequency") +
  theme_minimal()


# Fit the distribution to asses our value
fit_norm <- fitdist(daily_max_delay$max_delay, "norm")
fit_gamma <- fitdist(daily_max_delay$max_delay, "gamma")

# Plot the density of fitted distributions
plot.legend <- c("Normal", "Gamma")
denscomp(list(fit_norm, fit_gamma), legendtext = plot.legend)

# Summary of the fitted distributions
summary(fit_norm)
summary(fit_gamma)

# Compare models using AIC (or BIC)
AIC(fit_norm)
AIC(fit_gamma)

# gamma distribution fits the best our data on AIC and BIC it's very closed form the normal distribution


# Transform as date format
daily_max_delay$FL_DATE <- as.Date(daily_max_delay$FL_DATE)

# Get a new columns with the week number
daily_max_delay$week_number <- as.numeric(strftime(daily_max_delay$FL_DATE, format = "%U"))

# Fit a model for a weekly linear regression
weekly_model <- lm(max_delay ~ week_number, data = daily_max_delay)

# Predict the weekly delay based on our data
predictions <- predict(weekly_model)

ggplot(daily_max_delay, aes(x = week_number)) +
  geom_point(aes(y = max_delay), color = "blue", alpha = 0.6, size = 2) +  # Actual max_delay
  geom_line(aes(y = predictions), color = "red", size = 1) +              # Predicted values
  labs(
    title = "Weekly Max Delay vs Predictions",
    x = "Week Number",
    y = "Max Delay",
    color = "Legend"
  ) +
  theme_minimal()

# The linear model doesn't capture any variances among our delay values


# Use a Gev model
gev_const <- fevd(daily_max_delay$max_delay, type = "GEV")

gev_time_var <- fevd(daily_max_delay$max_delay, data = daily_max_delay, location.fun = ~ week_number, type = "GEV")

# Step 5: Extract AIC and BIC for both models
summary_const <- summary(gev_const)
summary_time_var <- summary(gev_time_var)


# AIC and BIC values
AIC_const <- summary_const$aic
AIC_time_var <- summary_time_var$aic

BIC_const <- summary_const$bic
BIC_time_var <- summary_time_var$bic

# # Print AIC values
# cat("AIC for GEV with constant parameters:", AIC_const, "\n")
# cat("AIC for GEV with time-varying location parameter:", AIC_time_var, "\n")
# 
# # Print BIC values
# cat("BIC for GEV with constant parameters:", BIC_const, "\n")
# cat("BIC for GEV with time-varying location parameter:", BIC_time_var, "\n")

# 
plot(gev_const)





# Extract the parameters from the fitted GEV model
location <- gev_const$results$par[1]  # Location parameter
scale <- gev_const$results$par[2]     # Scale parameter
shape <- gev_const$results$par[3]     # Shape parameter

# Step 2: Calculate the 10-year return level
T <- 10  # Return period in years
return_level_10_year <- location + scale * (-log(1 - (1/T)))^(shape)

# Step 3: Create a data frame for plotting
Week_number <- daily_max_delay$week_number
max_Delay <- daily_max_delay$max_delay

# Create a data frame for observed data
observed_data <- data.frame(Week = Week_number, Max_Delay = max_Delay)

return_levels <- data.frame(
  Week = max(Week_number) + 1:10,  # Extend weeks by 10 periods
  Return_Level = rep(return_level_10_year, 10)  # Repeat the 10-year return level
)


ggplot() +
  geom_point(data = observed_data, aes(x = Week, y = Max_Delay), color = "blue") +  # Plot observed data
  geom_line(data = return_levels, aes(x = Week, y = Return_Level), color = "red", linetype = "dashed", size = 1) +  # Plot return levels
  labs(
    title = "10-Year Return Level for Precipitation in Lausanne",
    x = "Week",
    y = "Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) 





















# Aggregate data by week

data$week <- format(daily_max_delay$FL_DATE, "%Y-%U")  # Year and Week number
weekly_data <- aggregate(max_delay ~ week, data = data, mean)  # Mean max delay per week

# Convert week to a numeric sequence for regression
weekly_data$time_numeric <- 1:nrow(weekly_data)

# Fit the linear model
weekly_model <- lm(max_delay ~ time_numeric, data = weekly_data)





#Fit a linear model to the yearly maximum precipitation values
linear_model <- lm(daily_max_delay ~ FL_Date, data = max_delay)



adf.test(daily_max_delay_ts)
acf(daily_max_delay_ts)

pacf(daily_max_delay_ts)

kpss.test(daily_max_delay_ts)
pp.test(daily_max_delay_ts)


# Check for seasonality
# clear seasonal pattern and trend 
# We use a yearly seasonality for each color (need to modify to 20XX)
# No regular patern so delay may be influenced by episodic and not regular events (need to see for each year)
# year 2 is lower (Covid) we have probably less flights due to Covid
# Year 5 peak at start of the year , need to find date
# try to link with Cause of delay in other columns 
ggseasonplot(daily_max_delay_ts)


## Stationary, trend and seasonality, t.windows = 5 (because we have 5 years of observation)
# Trend lowering around second /5 of the time but upwards rest of the time
# Clear periodic fluctuations , summer peaks then down after, seasonal pattern
# remainder (no clear pattern but some spikes, why ?) suggest one-off disruptions or anomalies 
#that are not explained by trend or seasonality
# ?grey bar on the left,  represents how much variation but ot clear on full data, need explanation
autoplot(stl(daily_max_delay_ts, t.window=18, s.window="periodic")) 
# to checked

# Combination of trended and seasonal effects
# The ACF values decrease gradually with increasing lag, the series has a long-term autocorrelation structure.
#There are significant positive correlations for almost all the lags within the 50-lag window, 
# suggesting the series has a persistent structure or trend.
# The slow decay of the ACF indicates a trend in the data,
# The absence of regular sharp spikes at specific lags suggests that any seasonality in the data is not strong or is overshadowed by the trend
# Statistically significant exceeds the blue dashed bars
ggAcf(daily_max_delay_ts, lag=50)
# Hige autocorelation into the data
# The time series is not stationnary


# The differenced series oscillates around zero
# Some spikes (e.g., around time point 5)
autoplot(diff(daily_max_delay_ts)) 
# decreases gradually over many lags, showing a long-term dependence in the series.
# We can still see that we have less variation during the year 2
# significant autocorrelation (bars outside the blue)
# This indicates that values in the time series are correlated with their past values 
# over extended periods, further confirming non-stationarity
# the differenced series appears stationary, good
ggAcf(diff(daily_max_delay_ts))
# Is diff enough ?
# From the plot, most autocorrelations are within the blue confidence bands, which suggests the differenced series is likely stationary.

# Creation of Model
# GEV model

# Store the stationnary time series
diff_stationary <- diff(daily_max_delay_ts)
# plot to check
plot(diff_stationary, type = "l", main = "Differenced Stationary Series")

# Store it into a dataframe
diff_df <- data.frame(
  FL_DATE = daily_max_delay$FL_DATE[-1],  # Adjust for one less observation after differencing
  diff_delay = diff_stationary
)

acf(diff_stationary)



library(ismev)
# use a GEV model with th fevd function

diff_df <- diff_df[!is.na(diff_df$diff_delay) & !is.infinite(diff_df$diff_delay), ]

mod_gev <- fevd(diff_stationary, type = "GEV", time.units = "days")
mod_gev <- fevd(daily_max_delay$max_delay, type = "GEV", time.units = "days")
summary(mod_gev)
mod_gev$results$par

# Compute return levels for specified return periods
return_levels <- return.level(mod_gev, return.period = c(10, 100, 1000),period="days")
print(return_levels)


# GP model (POT - peak over the threshold)
# Find the optimum threshold
mrlplot(daily_max_delay$max_delay, main="Mean Residual Life Plot")

th=1500
pot_mle <- fevd(as.vector(daily_max_delay$max_delay), method = "MLE", type="GP", threshold=th)

pot_mle
plot(pot_mle)

rl_mle <- return.level(pot_mle, conf = 0.05, return.period= c(2,5,10,20,50,100), do.ci=T)
rl_mle


