# Package loading
library(here)
library(fpp2)
library(dplyr)

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











