# util.R by Wayne Zeng, March 2021
# Utilities for Bachelor Thesis

# stats, forecast and ggplot2 libraries required for SARIMA and Holt-Winters
library(stats)
library(forecast)
library(ggplot2)

# Import Swiss deaths data from 2020
df_20 <- read.csv("https://www.bfs.admin.ch/bfsstatic/dam/assets/16104419/appendix", sep=";")
# Import Swiss deaths data from 2010-2019
df_10_19 <- read.csv("https://www.bfs.admin.ch/bfsstatic/dam/assets/12607335/master", sep=";")

# Filtering out data for over 65s for 2010-2019, 2010-2018 and 2020.
old_10_19 <- subset(df_10_19, Age=="65+")
old_10_18 <- subset(old_10_19, CY!=2018)

# Data from 2020 has different format to previous years, so column titles were adjusted.
old_20 <- subset(df_20, Year==2020 & Age=="65+     ")
colnames(old_20) <- colnames(old_10_19)

# Concatenate all data to one dataset
old <- rbind(old_10_19, old_20)

# Extracts deaths data and converts data to a time series object.
# frequency=52 to ensure that the period is over 1y=52w.
deaths_10_19 <- ts(old_10_19$NumberOfDeaths, start=2010, frequency=52)
deaths_10_18 <- ts(old_10_18$NumberOfDeaths, start=2010, frequency=52)
deaths <- ts(old$NumberOfDeaths, start=2010, frequency=52)
