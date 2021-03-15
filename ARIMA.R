# ARIMA.R by Wayne Zeng, March 2021
# Forecasting using Seasonal Arima for Bachelor Thesis

# Import Utilities File
source("util.R")

# Plot Time Series as well as ACF, PACF
# plot.ts(deaths_10_18)
tsdisplay(deaths_10_18, xlab="Year", ylab="no. of Deaths per Week", main="Swiss Deaths from 2010-2018")

# Perform differencing once, resulting in a zero-mean stationary time series
deaths_18_diff <- diff(deaths_10_18, differences=1)
tsdisplay(deaths_18_diff, xlab="Year", main="1-Differenced Swiss Deaths 2010-2018")

# Initial guess using SARIMA(0,1,1)x(0,1,1)
fit_init <- Arima(deaths_10_18,order=c(0,1,1), seasonal=c(0,1,1))
fit_init
res_init <- residuals(fit_init)
plotForecastErrors(res_init)
tsdisplay(res_init, xlab="Year", main="Initial ARIMA Estimation of Residuals 2010-2018")
arima_forecast_init <- forecast(fit_init, h=52)
plot(arima_forecast_init, xlab="Year", ylab="Deaths per Week")

# Calculates RMSE between forecast and real data
accuracy(arima_forecast_init, deaths_10_19)
write.csv(accuracy(arima_forecast_init, deaths_10_19),"accuracy_ar_init.csv", row.names = TRUE)

# Guess using auto.arima, results in modelling with SARIMA
fit_auto <- auto.arima(deaths_10_18)
fit_auto
res_auto <- residuals(fit_auto)
plotForecastErrors(res_auto)
tsdisplay(res_auto, xlab="Year", main="Automatic ARIMA Estimation of Residuals 2010-2018")
arima_forecast_auto <- forecast(fit_auto, h=52)
plot(arima_forecast_auto, xlab="Year", ylab="Deaths per Week")

# Calculates RMSE between forecast and real data
accuracy(arima_forecast_auto, deaths_10_19)
write.csv(accuracy(arima_forecast_auto, deaths_10_19),"accuracy_ar_auto.csv", row.names = TRUE)

# stepwise=FALSE and approximation=FALSE makes auto.arima look harder,
# however the code does not stop running. Execute at own risk.
# pre2020_auto2 <- auto.arima(pre2020_ts, stepwise=FALSE, approximation=FALSE)

