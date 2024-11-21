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

# Calculate mean de parture delay per day
daily_mean_delay <- american_airlines_data %>%
  group_by(FL_DATE) %>%
  summarise(mean_delay = mean(DEP_DELAY, na.rm = TRUE))

# Transform the dataframe into a time series
daily_mean_delay_ts <- ts(daily_mean_delay$mean_delay, frequency = 365)

# Modify the type of date as a date object
#daily_mean_delay_ts$FL_DATE <- as.Date(daily_mean_delay$FL_DATE) 

# Visualization of the time series (Change the legend y axis and values x axis)
# Observation of seasonality lower between 2 and 3, higher after that with peak at 5
# Observation of paterns : not obvious
# Delays are more concentrated around the lower range (0–25 minutes) for most days
# Peak at 5 (see what it means in date to give an explanation)
autoplot(daily_mean_delay_ts)


# Check for seasonality
# clear seasonal pattern and trend 
# We use a yearly seasonality for each color (need to modify to 20XX)
# No regular patern so delay may be influenced by episodic and not regular events (need to see for each year)
# year 2 is lower (Covid)
# Year 5 peak at start of the year , need to find date
# try to link with Cause of delay in other columns 
ggseasonplot(daily_mean_delay_ts, year.labels = TRUE, year.labels.left = TRUE)
ggseasonplot(daily_mean_delay_ts)


# Combination of trended and seasonal effects
# The ACF values decrease gradually with increasing lag, the series has a long-term autocorrelation structure.
#There are significant positive correlations for almost all the lags within the 50-lag window, 
# suggesting the series has a persistent structure or trend.
# The slow decay of the ACF indicates a trend in the data,
# The absence of regular sharp spikes at specific lags suggests that any seasonality in the data is not strong or is overshadowed by the trend
# Statistically significant exceeds the blue dashed bars
ggAcf(daily_mean_delay_ts, lag=50)

## Stationarity, trend and seasonality, t.windows = 5
# Trend lowering around second /5 of the time but upwards rest of the time
# Clear periodic fluctuations , summer peaks then down after, seasonal pattern
# remainder (no clear pattern but some spikes, why ?) suggest one-off disruptions or anomalies 
#that are not explained by trend or seasonality
# ?grey bar on the left,  represents how much variation but ot clear on full data, need explanation
autoplot(stl(daily_mean_delay_ts, t.window=18, s.window="periodic")) 


#STL decomposition
# to obtain the components, use seasonal() or trendcycle(), e.g. 
# Need better timeline and legends
# extraction of seasonality only
# periodic patterns repeating over time
# predictability with seasonality
seasonal(stl(daily_mean_delay_ts, t.window=5, s.window="periodic"))
plot(seasonal(stl(daily_mean_delay_ts, t.window=5, s.window="periodic")))
# During summer we an increase of flights


## ARIMA models 

# first, differencing is possible using diff()
# clear trend pattern 
autoplot(daily_mean_delay_ts) 
# trend removal 
# The differenced series oscillates around zero
# Some spikes (e.g., around time point 5)
autoplot(diff(daily_mean_delay_ts)) 
# decreases gradually over many lags, showing a long-term dependence in the series.
# significant autocorrelation (bars outside the blue)
# This indicates that values in the time series are correlated with their past values 
#over extended periods, further confirming non-stationarity
ggAcf(daily_mean_delay_ts)
# this stabilises the mean of the series, whereas logarithms stabilise the variance. 
# Majority of lines are inside the interval
# Why is lag 1 so big ?
# the differenced series appears stationary, good
ggAcf(diff(daily_mean_delay_ts))
 


# for the log
# can delete already done
autoplot(daily_mean_delay_ts)
ggAcf(daily_mean_delay_ts)

#Autocorrelation variance controlled with log
# The ACF gradually decreases across lags rather than dropping off sharply. This is a clear sign of non-stationarity, 
# likely caused by a trend in the series
# negative around lag 100 - could suggest cyclic or seasonal behavior
# spikes near lag 200, recurring events like holidays seasonal effect 
# Negative values are due to yearly seasonality ?
ggAcf(log(daily_mean_delay_ts))

autoplot(log(daily_mean_delay_ts)) #variance is smaller, yet trend and seasonality still here
ggAcf(diff(daily_mean_delay_ts)) #differencing is not enough...

ggAcf(diff(log(daily_mean_delay_ts), 12)) #sometimes, still not enough... 

autoplot(diff(diff(log(daily_mean_delay_ts), 12), 1)) 
autoplot(log(daily_mean_delay_ts)) #but much better in terms of stationarity...


# check values log like -5859
auto.mod <-auto.arima(daily_mean_delay_ts)
auto.mod

# Plot the residual
autoplot(auto.mod$residuals)
# same peak at lag1
#The significant spikes at lag 1 and 2 in the PACF confirm the use of two AR terms ?
ggPacf(daily_mean_delay_ts)



# formally test, e.g. with Anderson-Darling
library(DescTools)
AndersonDarlingTest(auto.mod$residuals, null = "pnorm") 
# Really small p-value
# A significant result (p-value < 0.05) indicates that the residuals deviate from normality.


library(cmstatr)
anderson_darling_normal(x = auto.mod$residuals)
# The residuals appear to fluctuate randomly around zero, with no obvious patterns or trends. 
#This indicates that the ARIMA model has captured most of the structure in the data.
# spike around 5

# Most of the bars fall within the 95% confidence intervals (blue dashed lines), 
#indicating that the residuals have no significant autocorrelation.

#The histogram shows that the residuals are roughly symmetrically distributed around zero.

checkresiduals(auto.mod) # can also use a built-in function


#____________________________________________________________________






# New topic: fitting distributions
# Example when generating t-distributed values and comparing them to the Normal
library(MASS)
n <- 1000
df <- 5
# df <- 100
xt <- rt(n, df)
autoplot(ts(xt))

fitted.dist <- fitdistr(xt, densfun = "Normal")
seqt <- seq(from = 0, to = 1, length = 100)
plot(seqt, qnorm(seqt, fitted.dist$estimate[1], fitted.dist$estimate[2]), type="l")
lines(seqt, qt(seqt, 5), col = 2)

seqt2 <- seq(from = -5, to = 5, length = 100)
plot(seqt2, dnorm(seqt2, fitted.dist$estimate[1], fitted.dist$estimate[2]), type="l", ylim = c(0, 0.4))
lines(seqt2, dt(seqt2, 5), col = 2)

## qqplot comparisons 
qqplot(rt(n, df = 5), xt)
qqline(xt, distribution = function(p) qt(p, df=5))

qqplot(rnorm(n, mean = fitted.dist$estimate[1], sd = fitted.dist$estimate[2]), xt)
qqline(xt, distribution = function(p) qnorm(p, mean = fitted.dist$estimate[1], sd = fitted.dist$estimate[2]))

## Anderson-Darling
AndersonDarlingTest(xt, null = "pnorm", mean = fitted.dist$estimate[1], sd = fitted.dist$estimate[2])
AndersonDarlingTest(xt, null = "pt", df = 5)

## Garch fit 
library(quantmod)
googl <- getSymbols("GOOGL", from="2010-01-01", to="2022-08-03", auto.assign = F)
head(googl[, 1:5], 5)
daily_ret <- (googl$GOOGL.Close - stats::lag(googl$GOOGL.Close)) / stats::lag(googl$GOOGL.Close)
daily_ret <- data.frame(index(daily_ret), daily_ret)
colnames(daily_ret) <- c("date", "return")
rownames(daily_ret) <- 1:nrow(daily_ret)

plot(daily_ret, type="l") # change of variance across time, can try to stabilise using log returns

neg.log <- function(x) {
  y <- c()
  for(i in 1:length(x)-1) {
    y[i] <- -log(x[i+1]/x[i])
  }
  return(y)
}

tmp <- as.vector(googl$GOOGL.Close)
yt <- neg.log(tmp)

# plot the series and assess the variance 
plot(yt, type="l")

# draw histogram and compare with normal distribution 
hist(yt, xlab = "negative log-returns", prob = TRUE)
curve(dnorm(x, mean = mean(yt), sd = sd(yt)), add = TRUE)

# auto.arima at this stage
mod5 <- auto.arima(yt)
mod5
checkresiduals(mod5$residuals) # independance of residuals
Box.test(yt, lag=1, "Ljung")   # reject H0 (independance of data)

# check acf of squared residuals
ggAcf(mod5$residuals^2)
Box.test(mod5$residuals^2, lag=1, type="Ljung-Box") #formally 

# volatility check
library(fGarch)
plot(volatility(as.vector(googl$GOOGL.Close)), type="l") #varying volatility over time

# fit a Garch(1,1)
garch1 <- garchFit(~ garch(1,1), cond.dist = "std", data = yt)
plot(garch1)

# usual specification 
ar1 <- arima(yt, order = c(1,0,0))
garch2 <- garchFit(~ garch(1,1), cond.dist = "std", data = ar1$residuals)
plot(garch2)

# Simulate garch data
yt <- garchSim(garchSpec(model = list()), 1000)
plot(volatility(yt), type="l") #varying volatility over time






# Gumbel distribution 

x <- runif(length(daily_mean_delay_ts))
mu <- mean(daily_mean_delay_ts, na.rm = TRUE)
beta <- sd(daily_mean_delay_ts, na.rm = TRUE)
y <- mu - beta * log(-log(x))


# Plot the Gumbel CDF
plot(sort(y), (1:length(y)) / length(y), type = "l", main = "Gumbel CDF", xlab = "Delay", ylab = "CDF")

# Plot the density function
seqx <- seq(min(y), max(y), length.out = 100)
z <- (seqx - mu) / beta
plot(seqx, 1 / beta * exp(-z - exp(-z)), type = "l", main = "Gumbel Density", xlab = "Delay", ylab = "Density")

# Perform the Anderson-Darling test
AndersonDarlingTest(y, null = "pGumbel", loc = mu, scale = beta)

# 2. Fit a Generalized Extreme Value (GEV) distribution to daily_mean_delay_ts
library(ismev)

# Convert the time series to a data frame for easier processing
daily_mean_delay_df <- data.frame(
  date = seq(from = as.Date("YYYY-MM-DD"), by = "day", length.out = length(daily_mean_delay_ts)),
  delay = as.numeric(daily_mean_delay_ts)
)

# Extract monthly maxima
library(lubridate)
daily_mean_delay_df$year <- year(daily_mean_delay_df$date)
daily_mean_delay_df$month <- month(daily_mean_delay_df$date)

monthly_max <- daily_mean_delay_df %>%
  group_by(year, month) %>%
  summarise(max_delay = max(delay, na.rm = TRUE))

# Fit the GEV distribution
mod1 <- gev.fit(daily_mean_delay_ts)

# Diagnostics for the GEV fit
gev.diag(mod1)

# Compare the GEV fit with Gumbel distribution
library(evd)
qqplot(rGumbel(length(monthly_max$max_delay), loc = mu, scale = beta), 
       monthly_max$max_delay, main = "QQ-Plot Gumbel vs Observed")
qqline(monthly_max$max_delay, 
       distribution = function(p) qGumbel(p, loc = mu, scale = beta), col = "blue")

# 3. Return level estimation
library(extRemes)
mod3 <- fevd(monthly_max$max_delay, type = "GEV", time.units = "months")
plot(mod3)

# 4. Confidence intervals for return levels
ci.fevd(mod3, alpha = 0.05, type = "parameter")

# 5. Compute return levels for specific periods (e.g., 10 years, 100 years)
return_levels <- return.level(x = mod3, return.period = c(10, 100, 1000), do.ci = TRUE, alpha = 0.05)
print(return_levels)

# 6. Return period associated with a specific level (e.g., delay = 60 minutes)
target_delay <- 60
as.numeric(1 / (1 - pgev(target_delay, loc = mod3$results$par[1], 
                         scale = mod3$results$par[2], 
                         shape = mod3$results$par[3])))

# 7. Linear trend modeling (optional)
unique_years <- unique(monthly_max$year)
plot(unique_years, monthly_max$max_delay, type = "b", main = "Max Delay per Year", xlab = "Year", ylab = "Max Delay")

# Fit a linear model
mod4 <- lm(max_delay ~ year, data = monthly_max)

# Predict for 5 future years
future_years <- data.frame(year = max(monthly_max$year) + 1:5)
predict.lm(mod4, newdata = future_years, se = TRUE)









