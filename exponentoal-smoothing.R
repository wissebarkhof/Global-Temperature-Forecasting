# Library from https://otexts.org/fpp2/
library(fpp2)

# Load data
temp_train <- ts(read.csv('data/temp_emissions_1960_2004.csv', header = TRUE), 
                 start = 1960, frequency = 12)
temp_test <- ts(read.csv('data/temp_emissions_2005_2014.csv', header = TRUE), 
                start = 2005, frequency = 12)

# Variable of interest
var <- 'LandAverageTemperature'

autoplot(temp_train[, var])

# 10 year naive seasonal forcast
snaive_train <- snaive(temp_train[,var], h=120)

snaive_fc <- forecast(snaive_train, 120)
autoplot(snaive_fc) +
  ylab("Celsius") +
  ggtitle("Seasonal Naive Average Land Temperature")  

# Naive RMSE
accuracy(snaive_fc, temp_test[,var])

# Exponential Smoothing (holt-winter) Additive
hw_train <- hw(temp_train[, var], seasonal="additive", 120)

autoplot(hw_train) + xlab('Year') + ylab('Temperature Celsius')

autoplot(temp_test[, var]) +
  autolayer(hw_train, series="HW additive forecasts", PI=FALSE) +
  xlab("Year") +
  ylab("Temperature Celsius") +
  ggtitle("Holt-Winter Forcast on the last 10 years of data")

# Holt-Winter's Method RMSE
accuracy(hw_train, temp_test[,var])

# Exponential Smoothing (holt-winter) Additive
hw_train_m <- hw(temp_train[, var], seasonal="multiplicative", 120)

autoplot(hw_train_m) + xlab('Year') + ylab('Temperature Celsius')

autoplot(temp_test[, var]) +
  autolayer(hw_train_m, series="HW additive forecasts", PI=FALSE) +
  xlab("Year") +
  ylab("Temperature Celsius") +
  ggtitle("Holt-Winter Forcast on the last 10 years of data")

# Holt-Winter's Method RMSE
accuracy(hw_train_m, temp_test[,var])

# Differenct States
require(gridExtra)

mult <- autoplot(hw_train$model$states[, c('l', 'b', 's1')], facets=TRUE) + 
  xlab('Years') + 
  ylab('Temperature') +
  ggtitle('Additive Holt-Winter states')

add <- autoplot(hw_train_m$model$states[, c('l', 'b', 's1')], facets=TRUE) + 
  xlab('Years') + 
  ylab('Temperature') +
  ggtitle('Multiplicative Holt-Winter states')

grid.arrange(mult, add, ncol=2)  



