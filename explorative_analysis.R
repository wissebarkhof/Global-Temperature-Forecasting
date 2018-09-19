# Load Data
data <- read.csv('data/temp_emissions_1960_2014.csv', header = TRUE, sep = ",")
summary(data)

# Library from https://otexts.org/fpp2/
library(fpp2)

# Create monthly time series of 'LandAverageTemperature' variable
temp <- ts(data$LandAverageTemperature, start=1960, frequency=12)

# Plot Time Series
autoplot(temp) +
  ggtitle("Average Land Temperature") +
  xlab("Year") +
  ylab("Celsius")

# Seasonal Plot
ggseasonplot(temp, year.labels=TRUE, year.labels.left=TRUE)+ 
  ylab("Celsius") +
  ggtitle("Seasonal plot: Average Land Temperature")  

# Why is there seasonality in avg GLOBAL temperature?

# Seasonal Subseries Plot
ggsubseriesplot(temp) +
  ylab("Celsius") +
  ggtitle("Seasonal Subseries plot: Average Land Temperature")  

# Scatter Plot
qplot(LandAverageTemperature, world_emission, data=as.data.frame(data)) +
  ylab("World Emission") + xlab("Average Temperature")     

# Some naive methods
autoplot(naive(temp, 120)) +
  ylab("Celsius") +
  ggtitle("Naive Average Land Temperature")  
  
# Taking account for seasonality
autoplot(snaive(temp, 120)) +
  ylab("Celsius") +
  ggtitle("Seasonal Naive Average Land Temperature")  

# Simple Exponential Smoothing
(fc <- ses(temp, h=12))
autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Celsius") + xlab("Year")
