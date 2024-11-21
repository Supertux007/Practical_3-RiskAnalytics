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


# to obtain the components, use seasonal() or trendcycle(), e.g. 
seasonal(stl(daily_mean_delay_ts, t.window=5, s.window="periodic"))
plot(seasonal(stl(daily_mean_delay_ts, t.window=5, s.window="periodic")))
# During summer we an increase of flights


## ARIMA models 

# first, differencing is possible using diff()
# this stabilises the mean of the series, whereas logarithms stabilise the variance. 
autoplot(daily_mean_delay_ts) # clear trend pattern 
autoplot(diff(daily_mean_delay_ts)) 
ggAcf(daily_mean_delay_ts)
ggAcf(diff(daily_mean_delay_ts))


# for the log
autoplot(daily_mean_delay_ts)
ggAcf(daily_mean_delay_ts)
autoplot(log(daily_mean_delay_ts)) #variance is smaller, yet trend and seasonality still here
ggAcf(log(daily_mean_delay_ts))

ggAcf(diff(daily_mean_delay_ts)) #differencing is not enough...

ggAcf(diff(log(daily_mean_delay_ts), 12)) #sometimes, still not enough... 

autoplot(diff(diff(log(daily_mean_delay_ts), 12), 1)) 
autoplot(log(daily_mean_delay_ts)) #but much better in terms of stationarity...


auto.mod <-auto.arima(daily_mean_delay_ts)
auto.mod

# Plot the residual
autoplot(auto.mod$residuals)

ggPacf(daily_mean_delay_ts)



# formally test, e.g. with Anderson-Darling
library(DescTools)
AndersonDarlingTest(auto.mod$residuals, null = "pnorm") 
# Really small p-value


library(cmstatr)
anderson_darling_normal(x = auto.mod$residuals)
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









