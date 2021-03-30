# ARIMA.R by Wayne Zeng, March 2021
# Forecasting using Seasonal Arima for Bachelor Thesis

# Import Utilities File
source("util.R")

# Plot Time Series as well as ACF, PACF using ggtsdisplay
ggtsdisplay(deaths_10_18, xlab="Year", ylab="no. of Deaths per Week", main="Swiss Weekly Deaths (65+) from 2010-2018")

# Perform differencing once, resulting in a zero-mean stationary time series
deaths_18_diff <- diff(deaths_10_18, differences=1)
ggtsdisplay(deaths_18_diff, xlab="Year", main="1-Differenced Swiss Deaths 2010-2018")

# Initial guess using SARIMA(0,1,1)x(0,1,1). checkresiduals plots the time series,
# ACF and residual histogram.
fit_init <- Arima(deaths_10_18,order=c(0,1,1), seasonal=c(0,1,1))
fit_init
res_init <- residuals(fit_init)
checkresiduals(res_init, xlab="Year", main="Initial ARIMA Estimation of Residuals 2010-2018")
# Perform Shapiro-Wilk test to check for normality of noise
shapiro.test(res_init)
# forecasts using ARIMA model and plots the forecast with 80% and 95% quantiles
arima_forecast_init <- forecast(fit_init, h=52)
autoplot(window(deaths_10_19, start=2010), main="Forecast of ARIMA(0,1,1)(0,1,1)[52], 2018-2019") +
  autolayer(arima_forecast_init, PI=TRUE)
# Calculates RMSE between forecast and real data
accuracy(arima_forecast_init, deaths_10_19)

# Guess using auto.arima, results in modelling with SARIMA
fit_auto <- auto.arima(deaths_10_18)
fit_auto
res_auto <- residuals(fit_auto)
checkresiduals(res_auto, xlab="Year", main="Automatic ARIMA Estimation of Residuals 2010-2018")
# Perform Shapiro-Wilk test to check for normality of noise
shapiro.test(res_auto)
# Forecast auto model
arima_forecast_auto <- forecast(fit_auto, h=52)
autoplot(window(deaths_10_19, start=2010), main="Forecast of ARIMA(1,0,0)(1,1,0)[52] with drift, 2018-2019") + 
  autolayer(arima_forecast_auto, PI=TRUE) +  xlab("Year") + ylab("Deaths per Week")

# stepwise=FALSE and approximation=FALSE makes auto.arima look harder,
# however the code does not stop running. Execute at own risk.
# pre2020_auto2 <- auto.arima(pre2020_ts, stepwise=FALSE, approximation=FALSE)

# Calculates RMSE between forecast and real data
accuracy(arima_forecast_auto, deaths_10_19)
