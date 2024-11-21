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







# Check for seasonality
# clear seasonal pattern and trend 
# We use a yearly seasonality for each color (need to modify to 20XX)
# No regular patern so delay may be influenced by episodic and not regular events (need to see for each year)
# year 2 is lower (Covid) we have probably less flights due to Covid
# Year 5 peak at start of the year , need to find date
# try to link with Cause of delay in other columns 
ggseasonplot(daily_max_delay_ts)




## Stationarity, trend and seasonality, t.windows = 5 (because we have 5 years of observation)
# Trend lowering around second /5 of the time but upwards rest of the time
# Clear periodic fluctuations , summer peaks then down after, seasonal pattern
# remainder (no clear pattern but some spikes, why ?) suggest one-off disruptions or anomalies 
#that are not explained by trend or seasonality
# ?grey bar on the left,  represents how much variation but ot clear on full data, need explanation
autoplot(stl(daily_max_delay_ts, t.window=18, s.window="periodic")) 
# to checked



#STL decomposition
# to obtain the components, use seasonal() or trendcycle(), e.g. 
# Need better timeline and legends
# extraction of seasonality only
# periodic patterns repeating over time
# predictability with seasonality
seasonal(stl(daily_max_delay_ts, t.window=5, s.window="periodic"))
plot(seasonal(stl(daily_max_delay_ts, t.window=5, s.window="periodic")))
# During summer we observe an increase of the delay, probably due to an augmentation of flights


## ARIMA models 

# first, differencing is possible using diff()
# clear trend pattern 
autoplot(daily_max_delay_ts) 
# trend removal 
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



# check values log like -5859
auto.mod <-auto.arima(daily_max_delay_ts)
auto.mod
# Ask for the loglike value
# maximise le log-like, 

# Plot the residual
autoplot(auto.mod$residuals)
# same peak at lag1
#The significant spikes at lag 1 and 2 in the PACF confirm the use of two AR terms ?
ggPacf(daily_max_delay_ts)



# formally test, e.g. with Anderson-Darling
library(DescTools)
AndersonDarlingTest(auto.mod$residuals, null = "pnorm") 
# Really small p-value
# A significant result (p-value < 0.05) indicates that the residuals deviate from normality.
# Can we say that if the residuals are not normally distributed then there is probably link with events that impact the dealys
# Causality proof ?

library(cmstatr)
anderson_darling_normal(x = auto.mod$residuals)
# The residuals appear to fluctuate randomly around zero, with no obvious patterns or trends. 
#This indicates that the ARIMA model has captured most of the structure in the data.
# spike around 5

# Most of the bars fall within the 95% confidence intervals (blue dashed lines), 
#indicating that the residuals have no significant autocorrelation.

#The histogram shows that the residuals are roughly symmetrically distributed around zero.
checkresiduals(auto.mod)


#____________________________________________________________________




# Fitting distributions
# Example when generating t-distributed values and comparing them to the Normal
library(MASS)

# We fit our time series on a normal distribution
fitted.dist <- fitdistr(daily_max_delay_ts, densfun = "Normal")
seqt <- seq(from = 0, to = 1, length = 100)
plot(seqt, qnorm(seqt, fitted.dist$estimate[1], fitted.dist$estimate[2]), type="l")
lines(seqt, qt(seqt, 5), col = 2)

seqt2 <- seq(from = -5, to = 5, length = 100)
plot(seqt2, dnorm(seqt2, fitted.dist$estimate[1], fitted.dist$estimate[2]), type="l", ylim = c(0, 0.4))
lines(seqt2, dt(seqt2, 5), col = 2)

# Here we can clearly observe that our observation are not following a normal distribution



## qqplot comparisons 
qqplot(rt(n, df = 5), daily_max_delay_ts)
qqline(daily_max_delay_ts, distribution = function(p) qt(p, df=5))
# Doesn't follow a t dsitribution

qqplot(rnorm(n, mean = fitted.dist$estimate[1], sd = fitted.dist$estimate[2]), daily_max_delay_ts)
qqline(daily_max_delay_ts, distribution = function(p) qnorm(p, mean = fitted.dist$estimate[1], sd = fitted.dist$estimate[2]))
# Doesn't follow a normal dsitribution


## Anderson-Darling
AndersonDarlingTest(daily_max_delay_ts, null = "pnorm", mean = fitted.dist$estimate[1], sd = fitted.dist$estimate[2])
AndersonDarlingTest(daily_max_delay_ts, null = "pt", df = 5)
# Not following the distribution as we have seen on the previous graphs


## Garch fit 
library(quantmod)

# change of variance across time, can try to stabilize using log returns
plot(daily_max_delay_ts, type="l") 

# Function for neg.log return
neg.log <- function(x) {
  y <- c()
  for(i in 1:length(x)-1) {
    y[i] <- -log(x[i+1]/x[i])
  }
  return(y)
}

# Vectorized our mean delay data
tmp <- as.vector(daily_max_delay$max_delay)
# Apply the neg.log function on our data
yt <- neg.log(tmp)

# plot the series and assess the variance 
plot(yt, type="l")
# Need to comment the variance : 

# draw histogram and compare with normal distribution by adding a curve on it
hist(yt, xlab = "negative log-returns", prob = TRUE)
yt <- na.omit(yt)
yt <- yt[!is.infinite(yt)]
curve(dnorm(x, mean = mean(yt), sd = sd(yt)), 
      from = min(yt), to = max(yt), add = TRUE, col = "blue", lwd = 2)


# auto.arima at this stage
model1 <- auto.arima(yt)
model1
# Comment on the loglike based on the teachers answers

checkresiduals(model1$residuals)
# We can say that there is no pattern between our residuals, we can see that the
# are not corrolated, but they are almost normally distributed
Box.test(yt, lag=1, "Ljung")   # reject H0 (independance of data)
# As we have a really small p-value we can reject H0 to 


# check acf of squared residuals
ggAcf(model1$residuals^2)
Box.test(model1$residuals^2, lag=1, type="Ljung-Box") #formally 
# As we have a really small p-value we can reject H0 to 
# Teacher question


# volatility check
library(fGarch)
plot(volatility(as.vector(daily_max_delay$max_delay)), type="l") #varying volatility over time
# What can we say ? Do we have an higher volatility at the end of the series ? 
# Is it among what ? why do we have a value around 6000 even if we don't 


# fit a Garch(1,1)
garch_1_1 <- garchFit(~ garch(1,1), cond.dist = "std", data = yt)
plot(garch_1_1)

# What can we conclude ? 





# usual specification 
arima_1_0_0 <- arima(yt, order = c(1,0,0))
garch2 <- garchFit(~ garch(1,1), cond.dist = "std", data = arima_1_0_0$residuals)
plot(garch2)

# What can we conclude ? 


# Simulate garch data
yt <- garchSim(garchSpec(model = list()), 1000)
plot(volatility(yt), type="l") #varying volatility over time

# ??????????????????????


#---------------------------------------------------


# Part 2 : 
# Gumbel distribution 

# Apply a Gumbel distribution to our dataset
x <- runif(length(daily_max_delay_ts))
mu <- mean(daily_max_delay_ts, na.rm = TRUE)
beta <- sd(daily_max_delay_ts, na.rm = TRUE)
y <- mu - beta * log(-log(x))
plot(y, x) # cdf of a Gumbel distribution 


# Plot the density function
seqx <- seq(min(y), max(y), length.out = 100)
z <- (seqx - mu) / beta
plot(seqx, 1 / beta * exp(-z - exp(-z)), type = "l", main = "Gumbel Density", xlab = "Delay", ylab = "Density")

# Perform the Anderson-Darling test
AndersonDarlingTest(y, null = "pGumbel", loc = mu, scale = beta)
# Which conclusion from the p-value ?
# As the p-value is 0.3572, we can not reject the Null Hypothesis and then conclude that
# A Gumbel Distribution can be confidently use to model our data

# Fitting a GEV distribution to Gumbel observations
library(ismev)
mod0 <- gev.fit(y)
# Does it fit? 
qqplot(rGumbel(length(y), loc=mu, scale=beta), y)
qqline(y, distribution=function(p) qGumbel(p, loc=mu, scale=beta), col="blue")

# We can see that it follows a Gumbel distribution

# Add a year and month column to our inital dataframe
daily_max_delay$FL_DATE <- as.Date(daily_max_delay$FL_DATE)
daily_max_delay$year <- year(daily_max_delay$FL_DATE)
daily_max_delay$month <- month(daily_max_delay$FL_DATE)


# Initialize an empty data frame for storing monthly maxima
monthly_max <- data.frame(month = numeric(0), year = numeric(0), maximum = numeric(0))

# Loop through unique years and months to extract monthly maxima
years <- unique(daily_max_delay$year)
months <- unique(daily_max_delay$month)

for (i in years) {
  for (j in months) {
    # Filter data for the current year and month
    subset <- daily_max_delay[
      daily_max_delay$year == i & daily_max_delay$month == j, ]
    
    if (nrow(subset) > 0) { # Ensure there is data for this month
      monthly_max <- rbind(monthly_max, data.frame(
        month = j, 
        year = i, 
        maximum = max(subset$max_delay, na.rm = TRUE)
      ))
    }
  }
}

# Assign column names to the monthly maxima data frame
colnames(monthly_max) <- c("month", "year", "maximum")

# Fit the first GEV model
mod1 <- gev.fit(monthly_max$maximum)

# Diagnostic plots for the first model
gev.diag(mod1)

mod2 <- gev.fit(monthly_max$maximum, monthly_max, mul = 2)

# Display the second model
print(mod2)

# Compare mod1 and mod2 using a likelihood ratio test
1 - pchisq(-2 * (mod2$nllh - mod1$nllh), 1)

# As we have an high p-value here it means that the model 1 without a trend is sufficient to 
# describe the data, the add of a linear trend doesn't improve the fit


library(extRemes)

Monthly_max <- daily_max_delay %>%
  group_by(month) %>%
  summarise(max_delay = max(max_delay, na.rm = TRUE))

mod3 <- fevd(Monthly_max$max_delay, type = "GEV", time.units = "months")


plot(mod3)
ci <- ci.fevd(mod3, alpha = 0.05, type = "parameter")
print(ci)

mod3$results$par #gives parameters of the GEV


# Wait for the answers of the teacher


# -----------------------------

library(tibble)
library(RCurl)
# get data from eHYD


# Visualise the data
plot(daily_max_delay_ts)
hist(daily_max_delay_ts, breaks = 30)
# seems to be left-skewed 


# MRL plot:
library(extRemes)
mrlplot(daily_max_delay_ts, main="Mean Residual Life Plot")
# Find the lowest threshold where the plot is somewhat linear, in our case around 45

# Clean the time series
daily_max_delay_ts <- daily_max_delay_ts[!is.na(daily_max_delay_ts) & !is.infinite(daily_max_delay_ts)]

# fitting the GPD model over a range of thresholds
threshrange.plot(daily_max_delay_ts, r = c(0, 40), nint = 41)
# Demander au prof, rien capté

# nint is the number of threshold, r the range of the thresholds 

# set threshold
th <- 40

# Visualise the threshold
layout(c(c(1)))
plot(as.vector(daily_max_delay_ts), type = 'l')
abline(h = th, col = 2)
# Tout juste



# maximum likelihood estimation
pot_mle <- fevd(as.vector(precipitation_xts$value), method = "MLE", type="GP", threshold=th)
pot_mle
# diagnostic plots
plot(pot_mle)
# return levels
rl_mle <- return.level(pot_mle, conf = 0.05, return.period= c(2,5,10,20,50,100), do.ci=T)
rl_mle

# alternative using evd
library(evd)
pot_mle_evd <- fpot(as.vector(precipitation_xts$value), threshold = th, npp = 365.25)
pot_mle_evd2 <- fpot(as.vector(precipitation_xts$value), threshold = th)
par(mfrow = c(2,2))
plot(pot_mle_evd)
confint(profile(pot_mle_evd))

# return levels with evd, e.g. 50-year
rl_mle_evd <- fpot(as.vector(precipitation_xts$value), threshold = th, npp = 365.25, mper = 50)
plot(rl_mle_evd)
# return level plots
par(mfcol=c(1,1))
# return level plot w/ MLE
plot(pot_mle, type="rl",
     main="Return Level Plot for Oberwang w/ MLE",
     ylim=c(0,200), pch=16)
loc <- as.numeric(return.level(pot_mle, conf = 0.05, return.period = 50))
segments(50, 0, 50, loc, col = 'midnightblue', lty = 6)
segments(0.01, loc, 50, loc, col = 'midnightblue', lty =6)


# Check if there is clustering of extremes (Ferro-Segers estimate)
exi(precipitation_xts$value, u = th)
# seems to indicate that there slighty is some level of clustering, but weak. 

pot_decl <- fpot(as.vector(precipitation_xts$value), threshold = th, npp = 365.25, cmax = TRUE, ulow = th, mper = 50)
# some changes in the estimates because we have less data, if there is clustering of extremes (expected)
confint(profile(pot_decl))
# compare with
confint(profile(rl_mle_evd))

plot(pot_decl)


# decluster and obtain plot, according to years for example 
years <- c()
k <- 1
for (i in 1:nrow(precipitation_xts)) {
  if (is.na(precipitation_xts$time[i])) {
    next
  } else {
    years[k] <- lubridate::year(precipitation_xts$time[i])
    k <- k+1
  }
}
years <- years - 1980

decl1 <- decluster(as.vector(precipitation_xts$value), threshold = th, groups = years, na.action = na.omit)
decl1
plot(decl1)
pot_decl1 <- fevd(decl1, threshold = th, type = "GP")
pot_decl1
plot(pot_decl1)





