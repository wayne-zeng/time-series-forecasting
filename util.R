# util.R by Wayne Zeng, March 2021
# Utilities for Bachelor Thesis
# The function plotForecastErrors was taken from section 2.5 of
# https://buildmedia.readthedocs.org/media/pdf/a-little-book-of-r-for-time-series/latest/a-little-book-of-r-for-time-series.pdf

# stats and forecast libraries required for SARIMA and Holt-Winters
library(stats)
library(forecast)

# Import Swiss deaths data from 2020
df_20 <- read.csv("https://www.bfs.admin.ch/bfsstatic/dam/assets/16104419/appendix", sep=";")
# Import Swiss deaths data from 2010-2019
df_10_19 <- read.csv("https://www.bfs.admin.ch/bfsstatic/dam/assets/12607335/master", sep=";")

# Filtering out data for over 65s for 2010-2019, 2010-2018 and 2020.
old_10_19 <- subset(df_10_19, Age=="65+")
old_10_18 <- subset(old_10_19, CY!=2018)

# 2020 data has different format to previous years, so column titles were adjusted.
old_20 <- subset(df_20, Year==2020 & Age=="65+     ")
colnames(old_20) <- colnames(old_10_19)

# Concatenate all data
old <- rbind(old_10_19, old_20)

# Extracts deaths data and converts data to a time series.
# frequency=52 to ensure that the period is over 1y=52w.
deaths_10_19 <- ts(old_10_19$NumberOfDeaths, start=2010, frequency=52)
deaths_10_18 <- ts(old_10_18$NumberOfDeaths, start=2010, frequency=52)
deaths <- ts(old$NumberOfDeaths, start=2010, frequency=52)


plotForecastErrors <- function(forecasterrors)
{ # Slightly modified helper function from A Little Book of R.
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

