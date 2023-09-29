#install library
library(readr)
library(TSA)
library(tseries)
library(forecast)
library(lmtest)

#Definisikan data
data <- GIAA_JK
ts <- ts(data[,5], start = c(2018,6),frequency = 12)

#Melihat visualisasi data
tsdisplay(ts)

#Uji stasioneritas data
adf.test(ts)

#EACF untuk spesifikasi model
eacf(ts)

#Differencing
dts <- diff(ts, 1)

#Visualisasi data hasil dIff
tsdisplay(dts)

#Uji stasioneritas data diff
adf.test(dts)

#EACF data diff
eacf(dts)

#Calon model: ARIMA(0,1,1), ARIMA(1,1,1), dan ARIMA(0,1,2)
model1 <- Arima(ts, order = c(0,1,1), include.constant = TRUE)
model2 <- Arima(ts, order = c(1,1,1), include.constant = TRUE)
model3 <- Arima(ts, order = c(0,1,2), include.constant = TRUE)
cbind(model1, model2, model3)

#Estimasi Parameter
model1 <- Arima(ts, order = c(0,1,1), include.constant = TRUE)
parameter <- coef(model1)
parameter

miu_cap <- parameter["drift"]
miu_cap

residuals <- residuals(model2)
sigma_kuadrat <- var(residuals)
sigma_kuadrat