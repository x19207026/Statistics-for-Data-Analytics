
rm(list=ls(all=TRUE))
library(readxl)
library(zoo)
library(Hmisc)
library(papeR)
library(timeSeries)
library(DataCombine) # for creating lags in a time series
library(fpp2)
library(forecast) # for time-series decomposing and de-seasonalizing, moving average, exponential smoothing
library(tseries) # for stationarity
library(tidyverse)
library(rvest)
library(ggthemes)
library(astsa)
library(xts)

# library(eurostat)
# GDP <- get_eurostat("TIPSAU20", time_format = "num")

# Import GDP Data #
setwd("~/Postgraduate Diploma in Data Analytics/Statistics for Data Analytics/Individual project/Project1")
GDP <- read_excel("Gross domestic product (GDP) at market prices - quarterly data [TIPSAU20] - Eurostat.xlsx",sheet = 4,range = "A10:CX39",col_names = FALSE)

# Data Pre-processing #
GDP <- GDP[-2,-2]
gdp_data <- t(GDP)
rownames(gdp_data) <- c(0:100)
colnames(gdp_data)  <- gdp_data[1,]
# col_names <- as.character(t(gdp_data[1,]))
# colnames(gdp_data) <- col_names
gdp_data <- gdp_data[-c(1,101),-c(12,19)]
Date <- as.Date(as.yearqtr(gdp_data[,1],format="%Y-Q%q"))
Date <- format(Date,"%b-%Y") # change date format of Date variable
data_cor <- as.data.frame(apply(gdp_data[,c(2:27)],2, as.numeric), stringsAsFactors=FALSE)
str(data_cor)
summary(data_cor)

# Select Data for Time Series Analysis #
greek_gdp <- ts(data_cor$EL, frequency = 4, start = c(1995, 1)) 

#=============================================================================================#
#                               TIME SERIES EXPLORATORY ANALYSIS                              #
#=============================================================================================#

# In this section, we are trying to identify possible horizontal patterns like trend, seasonality and cyclicality
# If it fluctuates around a line-mean through time, any upwards or downwards trend or any indication of seasonality or cyclicality in the data 


# The following quarterly plot of GDP shows that there is an initial increasing trend with a significant disruption(rough peak)
# during the Crisis period (2007-2010) and, after a slight slope, a gradual increase of lower level/power 
# is again observed. As a result, the variance increases as the level of GDP increases OR there is greater
# variability in the trend after the Crisis period and the observed peak.
# This plot does not show a clear pattern of seasonality and any reasonable sign of cyclicality.
# We will need to investigate it more by running statistical tests and graphs in order to provide
# more indicators of possible trend, cyclicality and seasonality as well as if our time series 
# is stationary or it needs to be transformed and converted into stationary.


par(mfrow=c(1,1))
autoplot(greek_gdp, facets = TRUE)+
  labs(title = "Quarterly GDP for Greece",
       subtitle = "In million euro (€)",
       y = "Quarterly GDP (€)",
       x = "year") +
  geom_smooth(method = "loess") +
  scale_x_continuous(breaks = seq(1995, 2019, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# We are producing below Seasonal Plot in order to identify if there is a specific relationship across the quarters
# It is similar to a time-plot, but the data are plotted for every year across
# the quarters. The graph below is strengthening our opinion that there is seasonality.
# Q1 is always lower than the others and Q2 lies close but behind Q3 and Q4.
# Although the 4th quarter seems to be the highest most of the years, after the rough peak
# in the GDP trend around 2007-10, 3rd quarter is winning among the rest three.


## Seasonal plot = 
ggseasonplot(greek_gdp) +
  labs(title = "Seasonal GDP plot for Greece",
       subtitle = "In million euro (€)",
       y = "GDP (€)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Second way of identifying seasonality/difference in the mean of each quarter
ggsubseriesplot(greek_gdp)+ # plot all years per quarter -- identify if there are similar peaks and seasonality
  labs(title = "Subseries GDP plot for Greece",
       subtitle = "In million euro (€)",
       y = "GDP (€)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# This plot strengthens our opinion on seasonality of our data. 
# While there is not so clear distinction among the quarters, the GDP levels seem to be slightly different.


## Lag plot of time series
gglagplot(greek_gdp)+
  labs(title = "Lag plot of quarterly GDP for Greece",
       subtitle = "In million euro (€)",
       y = "GDP (€)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The lag plot helps us understand if there is any autocorrelation by plotting
# each observation versus lagged one (one occured in a previous time period).
# A moderate seasonality is observed at lag 4 (1 year - 4 quarters), where
# all quarters follow a very similar path.

# Autocorrelation plot
ggAcf(greek_gdp)+
  ggtitle("Autocorrelation function plot")

# From the ACF plot, the significant autocorrelations at all lags indicates a 
# trend and/or seasonality in the time series.

# Using lag=4 in a Ljung-Box test, we can investigate if our time series is white noise, which
# means that it's purely random or it consists any trend/seasonality pattern.

# Ljung-Box test
Box.test(greek_gdp, lag = 4, type = "Ljung")

# Ljung-Box test p-value is very small < 0.01.
# This means that there is strong evidence that the
# time series is not white noise and has seasonality and/or trend.

# As the previous graphs confirmed us, by plotting the different components of time series, 
# we can observe how the trend, seasonality and error look like.

# SEASONALITY - TREND - ERROR #
decomposedRes <- decompose(greek_gdp, type="mult") # or "mult"
plot (decomposedRes) # see plot below

#=============================================================================================#
#                               TIME SERIES FORECASTING                                       #
#=============================================================================================#

# In order to build a model that can accurately forecast future values of the analysis variable, it is necessary to decide on which is the appropriate type of model for your time series.

# After our time series analysis, we will apply 4 candidate methods:
# 1. Naive (It's the simplest and most basic option where the most recent observation is the most likely outcome for the next quarter)
# 2. Average (Future forecasts are the average of the observed data)
# 3. Exponential Smoothing Method (it lies in the middle of naive and average method, but we are assigning the biggest weight to the 
#     most recent observation and the weights of all previous observations decrease exponentially back in time.
#     It is also suitable for our data as it allows for seasonality and trend to be included into the model)
# 4. ARIMA (This model incorporates not only seasonality and trend but also autocorrelation which is the correlation of observations with previous periods of time)

# Because our time series is relatively long, we will separate it into training and test set rather than using cross-validation.
# Train vs Test method is much faster than Cross-Validation. The training set starts from the beginning of 1995 up to 2014 end whereas the Test set is the remaining period of 19 quarters (observations).

train <- window(greek_gdp, end = c(2014, 4)) # Training set up to 2014 end
test <- window(greek_gdp, start = c(2015, 1)) # Test set is the remaining period with 19 quarters (observations)

# 1. NAIVE - SEASONAL NAIVE
# In order not to use simple naive method in data with seasonality, seasonal naive method will be considered as a candidate method instead.
fit.snaive <- snaive(train, h=length(test))
checkresiduals(fit.snaive) 
# The residuals seem not to be purely random and there is data pattern that can be removed.

# 2. AVERAGE
fit.aver <- meanf(train, h=length(test))

# Summarizing our simple forecasts:
autoplot(greek_gdp)+
  labs(title = "Simple Methods: Forecast values",
       subtitle = "Quarterly GDP for Greece (In million euro (€))",
       y = "Quarterly GDP (€)",
       x = "year") +
  autolayer(test, series="Test Data")+
  autolayer(fit.snaive$fitted, series="Seasonal Fitted values")+
  autolayer(fit.snaive, series="Seasonal Naive method", PI=FALSE)+
  autolayer(fit.aver, series="Average method", PI=FALSE)+
  guides(colour=guide_legend(title="Quarterly forecasts"))

# As observed from the fit and and the residuals of these 2 simple methods, we need to think more sophisticated forecasting methods for our data.

# 3. EXPONENTIAL SMOOTHING

# As our data has clear trend and a kind of seasonality, we will try Holt-Winters method which considers both of them.
# From the previous analysis, we will apply the multiplicative method.

fit.hw.mult <- hw(train,seasonal="multiplicative", h=length(test))
checkresiduals(fit.hw.mult)

# The residuals seem not to be purely random and there is data pattern that can be removed.

autoplot(greek_gdp)+
  labs(title = "Holt-Winters: Fitted and Forecast values",
       subtitle = "Quarterly GDP for Greece (In million euro (€))",
       y = "Quarterly GDP (€)",
       x = "year") +
  autolayer(fit.hw.mult$fitted, series="Fitted")+
  autolayer(fit.hw.mult, series="HW Multiplicative Test Data", PI=FALSE)+
  guides(colour=guide_legend(title="Quarterly forecasts"))

# The Holt-Winters' forecasts follow the volatility trend the original data.
# However, there is still more information in the variance that the model cannot capture.


# 4. ARIMA
# In order to capture the effects of autocorrelation, 
# it would be effective to apply an Autoregressive Integrated Moving Average (or ARIMA) model. 

# But HOW DO WE CHOOSE (p,d,q)(P,D,Q)[S]?????
# Create the training set and test set
train <- window(greek_gdp, end = c(2016, 4))
test <- window(greek_gdp, start = c(2017, 1))
autoplot(train)
# Stabilizing variation across time series using Box Cox transformation
BoxCox.lambda(train) # 0.6602493

# Find (p,d,q)(P,D,Q)
ndiffs(train) # d=1
# Take BoxCox transformation and seasonal differences:
dtrain <- diff(BoxCox(train,0.6602493),lag=3)
# Assess stationarity of the differenced series
adf.test(dtrain) #Augmented Dickey-Fuller test 
# accept the null hypothesis, so our time series is not stationary

ggtsdisplay(dtrain) # check differenced time series, acf and pacf side by side
# ACF/PACF plots. Choosing p and q
Acf(diff(train)) #  q =
Pacf(diff(train)) # p = autoregressive element = 4


# Fit a seasonal ARIMA model with lambda = 0.66 - box cox transformation 
fit <- auto.arima(train, lambda = "auto",trace=TRUE)

# After fitting ARIMA model, it is important to check if our residuals 
# are similar to white noise without outlines or patterns. 

# Plot residuals 
checkresiduals(fit)

# From the Ljung-Box Autocorrelation Test, it seems that the p-value is greater than
# 0.05 threshold which indicates no problem with autocorrelation
# This is confirmed by the ACF plot where all autocorrelations are lying within the blue lines. 
# Although there is an outlier on the positive side of the histogram, it looks pretty close to a Normal curve.
# Although the autocorrelation spikes are not so low, the point forecasts can be still good but the prediction
# intervals can be either too wide or too narrow and should not be taken into serious consideration.

fc <- forecast(fit,h=11)
autoplot(fc)+
  autolayer(test,series = "Test Data")

# Test accuracy
accuracy(fc, greek_gdp)["Test set", "MAPE"]
# Mean absolute percentage error for the test set is aound 5.8. 
# This means that the accuracy of the ARIMA(1,1,0)(1,1,0)[4] model 
# is around 94.2.

####################### EXPLANATION OF ARIMA MODEL #######################################
library(astsa)
plot(train,main="Quarterly GDP of Greece",ylab = "Quarterly GDP (million ???)", xlab = "year",type="c") # "o"
text(train,labels = 1:4,col=1:4)
# There is trend, seasonality and the 3rd and 4th quarter are usually up
# while the 1st and 2nd quarter is down. Also, there is heteroscedasticity as the trend level increases or the time passes.
decomposedRes <- decompose(train, type="mult") # or "additive"
plot(decomposedRes$seasonal,main="Seasonal Component",type="c") # "o"
text(decomposedRes$seasonal,labels = 1:4,col=1:4)
# Now, if we check the extracted seasonal component, it doesn't change
# by year to year. This is called seasonal persistence and can be taken care for seasonal 
# differencing. X(t)-X(t-4) or D=1, S=4 as the data is quarterly

plot(diff(train,4),main="Seasonal Differenced Quarterly GDP",type="c") # "o"
text(diff(train,4),labels = 1:4,col=1:4)
# We want to make the data stationary:
# First, we are logging the data in order to stabilize the variance.
# Because there is an obvious trend, differencing the data is needed
# The differenced data shows seasonal persistence with 1 cycle per year.
# This is removed by taking a seasonal difference.
ltrain <- log(train)
dltrain <- diff(log(train))
ddltrain <- diff(diff(log(train)),4)

par(mfrow=c(2,2))
plot(train,main="Quarterly GDP (Raw Data)",ylab = "Quarterly GDP (million ???)", xlab = "year") # Raw Data
# text(train,labels = 1:4,col=1:4)
plot(ltrain,main="Quarterly GDP (Log Transformation)",ylab = "Quarterly GDP (million ???)", xlab = "year") # Log transformation
# text(ltrain,labels = 1:4,col=1:4)
plot(dltrain,main="Quarterly GDP (Differenced & Log)",ylab = "Quarterly GDP (million ???)", xlab = "year") # Differenced 
# text(dltrain,labels = 1:4,col=1:4)
plot(ddltrain,main="Quarterly GDP (Differenced & Log & Seasonally Differenced)",ylab = "Quarterly GDP (million ???)", xlab = "year") # Seasonally differenced
# text(ddltrain,labels = 1:4,col=1:4)

adf.test(ddltrain)
# Based on the Augmented Dickey-Fuller Test, the p-value is smaller than 0.05.
# This means that our Null hypothesis is rejected and our time series is now stationary
# and we are able to apply our model
# Until now, we have d=1, D=1 with S=4 and the rest components will be given by the ACF and PACF plots.
acf2(ddltrain) 
# Looking at the Seasonal Lags:
# From the ACF plot, we observe that it's tailing off at lags 1s, 2s, 3s, ...(s=4). This means that Q=0.
# From the PACF plot, it seems that it's cutting off at lag 1s(s=4). This means that P=1 and suggests SAR(1)
# Looking at the Non-Seasonal Lags:
# Both ACF and PACF are cutting off at lag 1. This means that p=q=1 ARMA(1,1).

# (A)
fit.arima1 <- arima(ltrain,order=c(1,1,1),seasonal=c(1,1,0))
fc.arima1 <- forecast(fit.arima1,length(test))
sarima(ltrain,p=1,d=1,q=1,P=1,D=1,Q=0,S=4)
# The non-seasonal MA component is not significant
# As a result, we are fitting a SARIMA(1,1,0)(1,1,0)[4]
# (B)
fit.arima2 <- arima(ltrain,order=c(1,1,0),seasonal=c(1,1,0))
fc.arima2 <- forecast(fit.arima2,length(test))
sarima(ltrain,p=1,d=1,q=0,P=1,D=1,Q=0,S=4)
# When we fit the second model, we see that the non-seasonal AR component
# is not significant as the p-value > 0.05. Thus, we take it out and we fit 
# the SARIMA(0,1,0)(1,1,0)[4]
# (C)
fit.arima3 <- arima(ltrain,order=c(0,1,0),seasonal=c(1,1,0))
fc.arima3 <- forecast(fit.arima3,length(test))

checkresiduals(fit.arima1)
checkresiduals(fit.arima2)
checkresiduals(fit.arima3)
# In the residual diagnostics, everything looks good for all models but, perhaps, fit.arima2 will be our final model. 
# Let's check the forecasts, fitted values and accuracy for each model.

# autoplot(fc.arima1,main="Forecasts from SARIMA(1,1,1)(1,1,0)[4]")+
#   autolayer(log(test),series = "Test Data (Log)")
# autoplot(fc.arima2,main="Forecasts from SARIMA(1,1,0)(1,1,0)[4]")+
#   autolayer(log(test),series = "Test Data (Log)")
# autoplot(fc.arima3,main="Forecasts from SARIMA(0,1,0)(1,1,0)[4]")+
#   autolayer(log(test),series = "Test Data")

autoplot(greek_gdp,main="Forecasts of Seasonal ARIMA Models",ylab = "GDP (million ???)",xlab="year")+
  autolayer(exp(fc.arima1$mean),series="SARIMA(1,1,1)(1,1,0)[4] Forecasts",PI=FALSE)+
  autolayer(exp(fc.arima2$mean),series="SARIMA(1,1,0)(1,1,0)[4] Forecasts",PI=FALSE)+
  autolayer(exp(fc.arima3$mean),series="SARIMA(0,1,0)(1,1,0)[4] Forecasts",PI=FALSE)+
  autolayer(exp(fitted(fit.arima1)),series="SARIMA(1,1,1)(1,1,0)[4] Fit")+
  autolayer(exp(fitted(fit.arima2)),series="SARIMA(1,1,0)(1,1,0)[4] Fit")+
  autolayer(exp(fitted(fit.arima3)),series="SARIMA(0,1,0)(1,1,0)[4] Fit")+
  guides(colour=guide_legend(title="Methods"))
# In terms of fit in the training and test sets, their differences seem to be immaterial.

accuracy(fc.arima1,log(greek_gdp))
accuracy(fc.arima2,log(greek_gdp)) #best model
accuracy(fc.arima3,log(greek_gdp)) 


# Accuracy Measures#
# MAPE #
accuracy(fit.snaive,greek_gdp)["Training set","MAPE"]
accuracy(fit.snaive,greek_gdp)["Test set","MAPE"]
accuracy(fit.aver,greek_gdp)["Training set","MAPE"]
accuracy(fit.aver,greek_gdp)["Test set","MAPE"]
accuracy(fit.hw.mult,greek_gdp)["Training set","MAPE"]
accuracy(fit.hw.mult,greek_gdp)["Test set","MAPE"]


accuracy(exp(fc.arima1$mean),greek_gdp)["Test set","MAPE"]
accuracy(exp(fc.arima2$mean),greek_gdp)["Test set","MAPE"]
accuracy(exp(fc.arima3$mean),greek_gdp)["Test set","MAPE"]


autoplot(greek_gdp,ylab = "GDP (million ???)",xlab="year")+
  autolayer(exp(fc.arima2$mean),series="SARIMA(1,1,0)(1,1,0)[4] Forecasts",PI=FALSE)+
  autolayer(fit.hw.mult, series="HW Forecasts", PI=FALSE)+
  autolayer(exp(fitted(fit.arima2)),series="SARIMA(1,1,0)(1,1,0)[4] Fit")+
  autolayer(fit.hw.mult$fitted, series="H-W Fit")+
  guides(colour=guide_legend(title="Methods"))


fit.arima.best <- arima(log(greek_gdp),order=c(1,1,0),seasonal=c(1,1,0))
fc.arima.best <- forecast(fit.arima.best,20)
sarima(log(greek_gdp),p=1,d=1,q=0,P=1,D=1,Q=0,S=4)

autoplot(greek_gdp,ylab = "GDP (million ???)",xlab="year")+
  autolayer(exp(fitted(fit.arima.best)),series="SARIMA(1,1,0)(1,1,0)[4] Fit")+
  autolayer(exp(fc.arima.best$mean),series="SARIMA(1,1,0)(1,1,0)[4] Forecasts",PI=FALSE)+
  guides(colour=guide_legend(title="Method"))






forecast1 <- function(x,h){
  forecast(arima(x,order=c(1,1,1),seasonal=c(1,1,0)),h)
}
e1 <- tsCV(ltrain,forecast1, h=1)
rmse1 <- sqrt(mean(exp(e1)^2, na.rm=TRUE))
mae1 <- mean(abs(exp(e1)), na.rm=TRUE)

e1 <- tsCV(log(greek_gdp), forecastfunction=forecast1, h=19)
# Compute the MSE values and remove missing values
mse1 <- colMeans(e1^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:19, MSE = exp(mse1)) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()


forecast2 <- function(x,h){
  hw(x,seasonal="multiplicative", h)
}

e2 <- tsCV(greek_gdp, forecastfunction=forecast2, h=19)
# Compute the MSE values and remove missing values
mse2 <- colMeans(e2^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:19, MSE = mse2) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()



