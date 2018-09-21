# Imports
library(fpp2)
library(urca)

# Load Data
data <- read.csv("./data/temp_emissions_1960_2014.csv", header = TRUE, sep = ",")
summary(data)

# From Wisse
# Create monthly time series of 'LandAverageTemperature' variable
temp <- ts(data$LandAverageTemperature, start=1960, frequency=12)


# auto correlation function plot in order to see if the
# data is stationary or not
# Analysis of the linear relationship between lagged values
# of a time series.
# The plot shows a clear pattern. 
# Clearly shows a season pattern It however does not show any trends
ggAcf(temp,main="", lag=48)
temp %>% ur.kpss() %>% summary()

# Seasonal differencing of the data

cbind("average temp" = temp,
      "log avg temp" = log(temp),
      "Annual change in log temp" = diff(log(temp),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Differencing")

differencedData = diff(log(temp),12)

ggtsdisplay(differencedData)
# test for stationarity

differencedData %>% ur.kpss() %>% summary()
ggAcf(differencedData,main="", lag=48)

nsdiffs(temp)
nsdiffs(differencedData)
