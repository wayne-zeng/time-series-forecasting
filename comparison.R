# comparison.R by Wayne Zeng, March 2021
# Comparison of SARIMA and HW for Bachelor Thesis

# Import previous files
source("util.R")
source("ARIMA.R")
source("HoltWinters.R")

# Plot all three forecasts and original data onto plot
autoplot(window(deaths_10_19, start=2010)) +
  autolayer(arima_forecast_init, series="ARIMA_init", PI=FALSE) +
  autolayer(arima_forecast_auto, series="auto.arima", PI=FALSE) +
  autolayer(hw_forecast_18_19, series="Holt-Winters", PI=FALSE)
  S+ xlab("Years after 2010") + ylab("Deaths per Week") +
  + title("Forecasts for 2018-2019") +
  guides(colour=guide_legend(title="Forecast"))

# Make accuracies matrix to calculate RMSE
accuracies <- rbind(accuracy(arima_forecast_init, deaths_10_19)[,"RMSE"],
                    accuracy(arima_forecast_auto, deaths_10_19)[,"RMSE"],
                    accuracy(hw_forecast_18_19, deaths_10_19)[,"RMSE"])
colnames(accuracies) <- c("Training set RMSE", "Test set RMSE")
rownames(accuracies) <- c("Initial Arima Forecast", "Auto Arima Forecast", "Holt-Winters Forecast")

write.csv(accuracies,"accuracies.csv", row.names = TRUE)


# Now make predictions for 2020 based on data from 2010-19,
# errors will be large, showing effect of Covid-19

### ARIMA ###
# Perform differencing once, resulting in a zero-mean stationary time series
deaths_19_diff <- diff(deaths_10_19, differences=1)

# Initial guess using SARIMA(0,1,1)x(0,1,1)
fit_init_19 <- Arima(deaths_10_19, order=c(0,1,1), seasonal=c(0,1,1))
fit_init_19
arima_forecast_init_19 <- forecast(fit_init_19, h=52)

# Guess using auto.arima, results in modelling with SARIMA
fit_auto_19 <- auto.arima(deaths_10_19)
fit_auto_19
arima_forecast_auto_19 <- forecast(fit_auto_19, h=52)


## HW ##
# Training
hw_forecast_19 <- HoltWinters(deaths_10_19)
# Forecast training data to 2020, plot data
hw_forecast_19_20 <- forecast(hw_forecast_19, h=52)

# Plot all three forecasts and original data onto plot
autoplot(window(deaths, start=2010)) +
  autolayer(arima_forecast_init_19, series="ARIMA_init", PI=FALSE) +
  autolayer(arima_forecast_auto_19, series="auto.arima", PI=FALSE) +
  autolayer(hw_forecast_19_20, series="Holt-Winters", PI=FALSE)
S+ xlab("Years after 2010") + ylab("Deaths per Week") +
  + title("Forecasts for 2019-2020") +
  guides(colour=guide_legend(title="Forecast"))

# Make accuracies matrix
# accuracies_20 <- rbind(accuracy(arima_forecast_init_19, deaths)[,"RMSE"],
#                     accuracy(arima_forecast_auto_19, deaths)[,"RMSE"],
#                     accuracy(hw_forecast_19_20, deaths)[,"RMSE"])
# colnames(accuracies) <- c("Training set RMSE", "Test set RMSE")
# rownames(accuracies) <- c("Initial Arima Forecast", "Auto Arima Forecast", "Holt-Winters Forecast")

