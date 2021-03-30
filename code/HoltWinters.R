# HoltWinters.R by Wayne Zeng, March 2021
# Forecasting using HW for Bachelor Thesis

# Import Utilities File
source("util.R")

# Forecast Using data from 2010-2018 as training data, 2019 as test data.

# Training
hw_forecast_18 <- HoltWinters(deaths_10_18)
hw_forecast_18
# Plot to see if smoothing looks correct
plot(hw_forecast_18, xlab="Year", ylab="Deaths per Week")
# View Residuals
hw_res <- residuals(hw_forecast_18)
checkresiduals(hw_res, xlab="Year", main="Holt-Winters Estimation of Residuals 2010-2018")
shapiro.test(hw_res)

# Forecast training data to 2019, plot data
hw_forecast_18_19 <- forecast(hw_forecast_18, h=52)
# autoplot(hw_forecast_18_19, xlab="Year", ylab="Deaths per Week")
autoplot(window(deaths_10_19, start=2010), main = "HW Forecast 2018-2019: alpha = .6041, beta = 0, gamma=.7080") +
  autolayer(arima_forecast_auto, PI=TRUE) + xlab("Year") + ylab("Deaths per Week")

# Calculates RMSE between forecast and real data
accuracy(hw_forecast_18_19, deaths_10_19)
