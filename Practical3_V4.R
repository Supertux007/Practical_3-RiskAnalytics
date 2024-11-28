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


# STL plot
autoplot(stl(daily_max_delay_ts, t.window = 18, s.window = "periodic"))
stl_result <- stl(daily_max_delay_ts, t.window = 18, s.window = "periodic")

# Extract Residuals (Remainder Component)
residuals_series <- stl_result$time.series[, "remainder"]

# Plot the Residuals
autoplot(residuals_series) + 
  ggtitle("Residuals (Trend and Seasonality Removed)") +
  xlab("Time") +
  ylab("Residual Maximum Delay")

# compare the residuals against the original series:
original_plot <- autoplot(daily_max_delay_ts) + 
  ggtitle("Original Series") +
  xlab("Time") +
  ylab("Maximum Delay")

residuals_plot <- autoplot(residuals_series) + 
  ggtitle("Residuals") +
  xlab("Time") +
  ylab("Residual Maximum Delay")

grid.arrange(original_plot, residuals_plot, ncol = 1)

# Slightly better
acf(residuals_series)


############## Linear Model ###########################

# Switch to a date format
daily_max_delay$FL_DATE <- as.Date(daily_max_delay$FL_DATE, format = "%Y-%m-%d")

# Create a new column with Year and Week Number
daily_max_delay$Year_monthNum <- paste(
  format(daily_max_delay$FL_DATE, "%Y"),  # Extract year
  format(daily_max_delay$FL_DATE, "%m"),  # Extract month number
  sep = "-"
)

# Aggregate data by Year and Month Number to calculate monthly maximum delays
monthly_max_delay <- daily_max_delay %>%
  group_by(Year_monthNum) %>%
  summarize(max_delay = max(max_delay))  # Get monthly max delay


monthly_max_delay <- monthly_max_delay %>%
  mutate(
    Year = as.numeric(sub("-.*", "", Year_monthNum)),  # Extract Year
    Month = as.numeric(sub(".*-", "", Year_monthNum))   # Extract Week
  )

monthly_max_delay$Year_Month_Continuous <- monthly_max_delay$Year + (monthly_max_delay$Month / 12)


# Fit a linear model for the weekly maximum delay
Year_Month_model <- lm(max_delay ~ Year_Month_Continuous, data = monthly_max_delay)


last_month_continuous <- max(monthly_max_delay$Year_Month_Continuous)


# Predict the delay for the next 10 weeks
# Create a new dataframe for future weeks
future_months <- data.frame(
  Year_Month_Continuous = seq(
    from = last_month_continuous, 
    by = 1 / 12,   # Incrément de 1 mois (1/12 d'une année)
    length.out = 4 * 12
  )
)

# Predict values for the next months of the next 4 years
predictions <- predict(Year_Month_model, newdata = future_months, interval = "confidence")

# Combine predictions with the future weeks into a dataframe
predicted_data <- data.frame(future_months, predictions)

# Plot the original weekly data, the linear model, and predictions
ggplot() +
  geom_point(data = monthly_max_delay, aes(x = Year_Month_Continuous, y = max_delay), color = "blue", size = 2) +
  geom_line(data = monthly_max_delay, aes(x = Year_Month_Continuous, y = max_delay), color = "blue", size = 1) +
  geom_line(data = predicted_data, aes(x = Year_Month_Continuous, y = fit), color = "red", linetype = "dashed") +
  geom_ribbon(data = predicted_data, aes(x = Year_Month_Continuous, ymin = lwr, ymax = upr), 
              fill = "grey80", alpha = 0.5) +
  labs(
    title = "Weekly Maximum Delay with Linear Model Predictions",
    x = "Year (Week Continuous)",
    y = "Maximum Delay (minutes)"
  ) +
  theme_minimal()

# The linear model doesn't capture any variances among our delay values



################ GEV Model #####################


# Fit the GEV model with constant parameters on our residual series
gev_const <- fevd(as.numeric(residuals_df$residual), type = "GEV")

# Show the result of the modelling
plot(gev_const)


# Extract summaries for both models
summary_const <- summary(gev_const)

# Extract AIC and BIC values
AIC_const <- summary_const$aic
BIC_const <- summary_const$bic

# Print AIC and BIC for comparison
cat("Constant GEV Model - AIC:", AIC_const, "BIC:", BIC_const, "\n")





### Return level

# Extract the GEV parameters
location <- gev_const$results$par[1]  # Location parameter
scale <- gev_const$results$par[2]     # Scale parameter
shape <- gev_const$results$par[3]     # Shape parameter

# Define the return period in days (e.g., 1000 days)
T <- 10  

# Calculate the return level using the GEV formula
return_level_1000_day <- location + 
  (scale / shape) * ((-log(1 - 1 / T))^shape - 1)

# Get the last date in the observed data
last_date <- max(daily_max_delay$FL_DATE)

# Create a sequence of dates for the next 1000 days
future_dates <- seq(from = last_date + 1, by = 1, length.out = 1000)

# Create a data frame for the future return levels
return_levels <- data.frame(
  Date = future_dates,                      # Future dates
  Return_Level = rep(return_level_1000_day, length(future_dates))  # Same return level for all
)

residuals_df$FL_DATE <- as.Date(residuals_df$FL_DATE)

# Combine observed data and return levels into one data frame if needed
observed_data <- data.frame(
  Date = residuals_df$FL_DATE,
  Max_Delay_res = residuals_df$residual
)


ggplot() +
  geom_point(data = observed_data, aes(x = Date, y = Max_Delay_res), color = "blue") +  # Plot observed data
  geom_line(data = return_levels, aes(x = Date, y = Return_Level), color = "red", linetype = "dashed", size = 1) +  # Plot return levels
  labs(
    title = "10-Year Return Level for Maximum Delay",
    x = "Date",
    y = "Delay (min)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))





# Parameters from the GEV model
mu <- gev_const$results$par["location"]  # Location parameter
sigma <- gev_const$results$par["scale"]   # Scale parameter
xi <- gev_const$results$par["shape"]      # Shape parameter

# Specify the delay time for which we want to calculate the return period

Delay_threshold<- 1000

# Calculate the CDF for the given amount using the GEV distribution
if (xi != 0) {
  # GEV with shape parameter not equal to 0
  F_x <- exp(-((1 + (xi * (Delay_threshold - mu)) / sigma) ^ (-1 / xi)))
} else {
  # GEV with shape parameter equal to 0 (Gumbel distribution)
  F_x <- exp(-exp(-(Delay_threshold - mu) / sigma))
}

# Calculate the probability of exceeding the specified precipitation amount
P_exceed <- 1 - F_x

# Calculate the return period
return_period <- 1 / P_exceed

# Output the result
return_period



#Using the fitted model, compute the probability that there will be a day in the next year when the precipitation exceeds 150 mm.
# Parameters from the fitted GEV model
mu <- gev_const$results$par["location"]  # Location parameter
sigma <- gev_const$results$par["scale"]   # Scale parameter
xi <- gev_const$results$par["shape"]      # Shape parameter

# Specify the precipitation amount for which we want to calculate the probability
Delay_threshold <- 1000

# Calculate the CDF for the given amount using the GEV distribution
if (xi != 0) {
  # GEV with shape parameter not equal to 0
  F_x <- exp(-((1 + (xi * (Delay_threshold - mu)) / sigma) ^ (-1 / xi)))
} else {
  # GEV with shape parameter equal to 0 (Gumbel distribution)
  F_x <- exp(-exp(-(Delay_threshold - mu) / sigma))
}

# Calculate the probability of exceeding the specified precipitation amount
D_exceed_1000 <- 1 - F_x

# Calculate the probability of not exceeding 1000 min on all 365 period
D_not_exceed_1000 <- F_x^52

# Calculate the probability of at least one exceedance in the next year
D_at_least_one_exceedance <- 1 - D_not_exceed_1000

# Output the result
D_at_least_one_exceedance











############ Peak over the threshold ######################


# Extract the daily precipitation values
delay_Res <- residuals_df$residual

# Generate the Mean Residual Life (MRL) plot
mrlplot(delay_Res, main = "Mean Residual Life Plot for may delay")

# Specify the threshold based on the MRL plot, here we choose 50
threshold <- 1500

# Create a new column in the data indicating whether the precipitation exceeds the threshold
residuals_df$Exceeds_Threshold <- ifelse(delay > threshold, TRUE, FALSE)

# Plot the time series with highlighted exceedances
ggplot(residuals_df, aes(x = FL_DATE, y = residual)) +
  geom_line(color = "blue") +  # Regular precipitation values
  geom_point(data = subset(residuals_df, Exceeds_Threshold == TRUE), aes(x = FL_DATE, y = residual), 
             color = "red", size = 1.5) +  # Highlight exceedances
  labs(title = "Time Series of Daily Precipitation with Threshold Exceedances",
       x = "Date", y = "Daily Precipitation (mm)") +
  theme_minimal()











