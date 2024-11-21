# Import the data from our .csv
library(here)
library(fpp2)
library(dplyr)

#flights_data <- read.csv(here::here("flights_sample_3m.csv"))

# Filter to keep only the data for the American Airline company
#american_airlines_datacan_airlines_data <- flights_data[flights_data$AIRLINE == "American Airlines Inc.", ]

#write.csv(american_airlines_datacan_airlines_data, here::here("flights_sample_3m_filtered.csv"), row.names = FALSE)

american_airlines_data <- read.csv(here::here("flights_sample_3m_filtered.csv"))

# Convert the date in a date format
american_airlines_data$DATE <- as.Date(american_airlines_data$FL_DATE, format = "%Y-%m-%d")

# Calculate mean delay per day
daily_mean_delay <- american_airlines_data %>%
  group_by(FL_DATE) %>%
  summarise(mean_delay = mean(DEP_DELAY, na.rm = TRUE))

# Transform the dataframe into a time series
daily_mean_delay_ts <- ts(daily_mean_delay$mean_delay, frequency = 365)

# Modify the type of date as a date object
#daily_mean_delay_ts$FL_DATE <- as.Date(daily_mean_delay$FL_DATE) 

# Visualization of the time series
autoplot(daily_mean_delay_ts)


# Check for seasonality
# clear seasonal pattern and trend 
ggseasonplot(daily_mean_delay_ts, year.labels = TRUE, year.labels.left = TRUE)
ggseasonplot(daily_mean_delay_ts)


# Combination of trended and seasonal effects
ggAcf(daily_mean_delay_ts, lag=50)

## Stationarity, trend and seasonality, t.windows = 5
autoplot(stl(daily_mean_delay_ts, t.window=5, s.window="periodic")) 








