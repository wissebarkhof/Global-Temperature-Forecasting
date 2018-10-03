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


# ARIMA analysis of temperatur
# quarterly European retail trade data from 1996 to 2011.
temp %>% ggtsdisplay()

# The test statistic is bigger than the 1% critical value
# Indicating that the null hypothesis is rejected meaning
# that the data is not stationary
#KPSS test
temp %>% ur.kpss() %>% summary()

#In this case we have one large spike at lag 12 in AFC and on gradual (exponensial)
#decrease in the PACF at 12,24 36 etc indicating that we need a seasonal differencing at 12
temp %>% diff(lag=12) %>% ggtsdisplay()

# In this case we have on large spike at lag 1 and a greadual decrease
# in the ACF, where we can see that only two spikes are significant in the PACF
# this indicates an AR(2) term in the model.
temp %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

#This looks alright so we do a test of the data 

model1 <- Arima(temp, order=c(2,1,0), seasonal=c(0,1,0), lambda=0)
fit1 <- model1


model1 %>% residuals() %>% ggtsdisplay()

checkresiduals(fit1)
# ACF and PACF indicate that there is stil a need to include some 
# non seasonal terms. Because we see that we have a sharp spike in the ACF
# at 12 and a slow decreasing of in the PACF at every 12, 14, 36..

model2 <- Arima(temp, order=c(2,1,0), seasonal=c(0,1,1), lambda=0)
(fit2 <- model2)

model2 %>% residuals() %>% ggtsdisplay()

# Here we have a sharp spike in the ACF and the decrease is gradual in the PACF

model3 <- Arima(temp, order=c(2,1,1), seasonal=c(0,1,1), lambda=0)
fit3 <- model3

model3 %>% residuals() %>% ggtsdisplay()

# KPSS test
# Tiny test statistic well within 1 % and as such well within
# expected range for stationary data.
temp %>% diff(lag=12) %>% diff() %>% ur.kpss() %>% summary()

# I try to add more terms to decrease the model, there seems to be some seasonal spikes left
# Looking at the AIC to see if it increases.

# all increases after the above seem to decrease the AIC, but this is by 1 or 2. Not sure that is worth the added complexity
model4 <- Arima(temp, order=c(2,1,1), seasonal=c(2,1,2), lambda=0)
(fit4 <- model4)

model4 %>% residuals() %>% ggtsdisplay()


# Forcast 
fit3 %>% forecast(h=12) %>% autoplot()


# checking Rs ARIMA
fit<-auto.arima(temp, stepwise=FALSE, approximation=FALSE, lambda=0)
summary(fit)
