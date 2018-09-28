# Load Data
globaltemp <- read.csv('data/temp_emissions_1960_2014.csv', header = TRUE, sep = ",")
summary(globaltemp)

# Library from https://otexts.org/fpp2/
library(fpp2)

datecols <- names(globaltemp)
datecols[datecols != 'dt' & datecols != 'world_emission']
# Create monthly time series of 'LandAverageTemperature' variable
for (col in datecols) {
  globaltemp[col] <- ts(globaltemp[col], start=1960, frequency=12)
}

globaltemp$world_emission = ts(globaltemp$world_emission, start=1960, frequency=1)

# linear model
linmodel <- lm(LandAndOceanAverageTemperature ~ dt, data = globaltemp)



# Plot Time Series
autoplot(globaltemp$LandAndOceanAverageTemperature) +
  ggtitle("Average Land Temperature") +
  xlab("Year") +
  ylab("Celsius")

# Seasonal Plot
ggseasonplot(globaltemp$LandAndOceanAverageTemperature, year.labels=TRUE, year.labels.left=TRUE)+ 
  ylab("Celsius") +
  ggtitle("Seasonal plot: Average Land Temperature")  

# Why is there seasonality in avg GLOBAL temperature?

# Seasonal Subseries Plot
ggsubseriesplot(globaltemp$LandAndOceanAverageTemperature) +
  ylab("Celsius") +
  ggtitle("Seasonal Subseries plot: Average Land Temperature")  

# Scatter Plot
qplot(LandAverageTemperature, world_emission, data=as.data.frame(globaltemp)) +
  ylab("World Emission") + xlab("Average Temperature")     

# Some naive methods
naive_fc <- naive(globaltemp$LandAndOceanAverageTemperature, 120)
autoplot(naive_fc) +
  ylab("Celsius") +
  ggtitle("Naive Average Land Temperature")  
  
# Taking account for seasonality
snaive_fc <- snaive(globaltemp$LandAndOceanAverageTemperature, 120)
autoplot(snaive_fc) +
  ylab("Celsius") +
  ggtitle("Seasonal Naive Average Land Temperature")  

# Simple Exponential Smoothing
(fc <- ses(globaltemp$LandAndOceanAverageTemperature, h=12))
autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Celsius") + xlab("Year")
