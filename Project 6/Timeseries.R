## ETS Model ( Exponential Time Series Model)

# upload a Time series data

data_ch<- read_excel(file.choose())
data_booking <- read_excel(file.choose())

View(data_booking)
View(data_ch)


head(elecequip)
# convert the data in time serie class

ddata_ch_ts <- data_ch[,-1]
data_ch_ts <- ts(ddata_ch_ts, start = c(2001,1),end = c(2012,8), frequency = 96)
plot(data_ch_ts)

data_booking_ts <- ts(data_booking[,3], start = c(2006,1), end = c(2015,12), frequency = 120)
plot(data_booking_ts)

#decompose the time series data
ts_ft1 <- decompose(data_ch_ts, type="additive")
plot(ts_ft1)
ts_ft2 <- decompose(data_ch_ts, type="multiplicative")
plot(ts_ft2)

ts_f1 <- decompose(data_booking_ts, type="additive")
plot(ts_f1)
ts_f2 <- decompose(data_booking_ts, type="multiplicative")
plot(ts_f2)


# Seasonal Adjustment of the time series data
data_ch_ts_adj <- seasadj(stl(data_ch_ts, s.window="periodic"))
plot(data_ch_ts_adj)

#Seasonal Adjustment LOESS approach
fitloess <- stl(data_ch_ts, t.window=96, s.window="periodic", robust = TRUE)
plot(fitloess)
# Forecasting , A short-cut approach is to use the stlf() function. The following code will decompose the time series using STL, 
#forecast the seasonally adjusted series, and return the reseasonalised forecasts.
fcast <- stlf(data_ch_ts, method='naive')
plot(fcast)


fcast1 <- stlf(data_booking_ts, method='naive')
plot(fcast1)


#Exponential smoothing
#Holt's linear trend method
fc <- holt(data_booking_ts, h=60)
plot(fc)

## Holt's linear trand with damped trend

fc2 <- holt(data_booking_ts, damped = TRUE, phi = 0.98, h=60)
plot(fc2)

# Cross validation of models

e1 <- tsCV(data_booking_ts, ses, h=60)
e2<- tsCV(data_booking_ts, holt, h=60)
e3<- tsCV(data_booking_ts, holt, damped=TRUE, h=60)

# Comapre MSE 
SES_MSE <- mean(e1^2, na.rm=TRUE)
Holt_MSE <- mean(e2^2,na.rm=TRUE)
Damped_MSE <-mean(e3^2, na.rm=TRUE)


# Comapre MAE 

SES_MAE <- mean(abs(e1), na.rm=TRUE)
Holt_MAE <- mean(abs(e2),na.rm=TRUE)
Damped_MAE <-mean(abs(e3), na.rm=TRUE)

CV_table <- as.list.data.frame(c(SES_MAE,SES_MSE,Holt_MAE,Holt_MSE,Damped_MAE,Damped_MSE))
CV_table
fc[["model"]]

# Extent Holt Winter's Method for seasonality, limited to 24 

ft3 <- dshw(data_booking_ts)
ft4 <- dshw(data_ch_ts)
plot(ft3)
plot(ft4)

## Estimating ETS models.The ets() function in R

ets1 <- ets(data_booking_ts, model= "ZZZ",damped = NULL)
plot(ets1)
summary(ets1)


# ARIMA Models..



#####


data_sales<- read_csv(file.choose())
data_sales


data_sales_ts <- ts(data_sales[,2], start = c(2012,3),end = c(2015,12), frequency = 46)
plot(data_sales_ts)

data_decom <- decompose(data_sales_ts, type = "additive")
autoplot(data_decom)

fitloess_data <- stl(data_sales_ts, t.window=NULL, s.window="periodic", robust = FALSE)
autoplot(fitloess_data)+ggtitle("STL Decomposition of Sales Data")



