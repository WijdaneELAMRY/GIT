library(tidyverse)

set.seed(197273)
options(max.print = 1000000)
hawai <- read.csv("hawai.csv")
hawai
 str(hawai)
hawai$time<-format(date_decimal(hawai$time), "%Y-%m-%d")
hawai

point_depart <- c(year(min(hawai$time)), month(min(hawai$time)))
print(point_depart)
frequence <- 12
hawai_ts <- ts(
  data = hawai$CO2,
  start = point_depart,
  frequency = frequence
)

hawai_ts


# Split data into training and test 

install.packages("caret")
library(caret)

set.seed(68017)

id_tr <- createDataPartition(hawai$CO2, p = 0.7, list = FALSE)
id_tr
hawai_tr <- hawai [id_tr, ]
hawai_te <- hawai [-id_tr, ]
hawai_tr
hawai_te

# Build Model

install.packages("fable")
library(fable)

install.packages("forecast",dependencies = TRUE)
library(forecast)

install.packages("tseries")
library(tseries)

plot.ts(hawai_tr$CO2)
acf(hawai_tr$CO2)
adf.test(hawai$CO2)

hawaimodel= auto.arima(hawai_ts,ic="aic", trace= TRUE)
hawaimodel
acf(ts(hawaimodel$residuals))
Pacf(ts(hawaimodel$residuals))
hawai_forecast= forecast(hawaimodel, c(95), h=40)
hawai_forecast
plot(hawai_forecast)

#Validate ARIMA Model :
Box.test(hawai_forecast$residuals, lag=5)
