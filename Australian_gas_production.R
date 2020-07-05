#Mini project -Australian Gas Time series dataset
rm(list=ls())
setwd("D:/BABI/Time_series_M7/Mini_Project")
library("forecast")
Ts_obj=ts(gas,start=c(1970,1),frequency = 12)
summary(Ts_obj)
monthplot(Ts_obj,main="Monthly gas production plot")
seasonplot(Ts_obj)
ggseasonplot(Ts_obj, year.labels=TRUE, year.labels.left=TRUE,main="Seasonal plot-Monthly Gas Production") 
plot(Ts_obj,main="Time Series plot")#no seasonality
#Inference from the plot-data are high in the month of july and low at jan month
#Multiplicative model
Log_Ts_obj=log(Ts_obj)
Log_Ts_obj_dec=stl(Log_Ts_obj, s.window=7)
Decompose_obj=exp(Log_Ts_obj_dec$time.series)
plot(Decompose_obj,main="Decomposed data")
#de-seasonalise
deseason_obj=Decompose_obj[,2]+Decompose_obj[,3]
ts.plot(deseason_obj, Ts_obj, col=c("red", "blue"), main="Comparison of Gas and Deseasonalized Gas")

########################################
library("tseries")

adf.test(Ts_obj)
Diff_obj=diff(Ts_obj)
plot(Diff_obj,main="Differenced data")
#NUll Hypothesis:Time series Non-stationary
#Alternate Hypothesis:Time series stationary
adf.test(Diff_obj)#now it is stationary
?adf.test
#Data Split
Train_data=window(Ts_obj,start=c(1970,1),end=c(1993,12),frequency=12)
Test_data=window(Ts_obj,start=c(1994,1),frequency=12)
#ACF plot
acf(Diff_obj,lags=50,main="ACF plot")
#PACF plot
pacf(Diff_obj,lags=50,main="PACF plot")
#Fitting the ARIMA Model
Gas_arima_fit=arima(Train_data,c(1,1,1))
Gas_arima_fit
#To check the adequacy of the model-plot the residual values
hist(Gas_arima_fit$residuals,col="blue")
acf(Gas_arima_fit$residuals)
#Comparison of actual and fitted value
Fitted_values=fitted(Gas_arima_fit)
ts.plot(Train_data,Fitted_values,col=c("red","blue"),main="Comparison of actual and fitted values")
#Ljung-Box test
Box.test(Gas_arima_fit$residuals, lag=30, type="Ljung-Box")
#Auto arima functions
Auto_arima_model=auto.arima(Train_data,ic="aic",trace=TRUE)
Auto_arima_model
#Ljung-Box test
Box.test(Auto_arima_model$residuals, lag=30, type="Ljung-Box")
ts.plot(Train_data,fitted(Auto_arima_model),col=c("red","blue"),main="Auto Arima:Comparison of actual and fitted values")

#Forecast - 20 periods for arima model
Predicted_arima=forecast(Gas_arima_fit,h=20)
autoplot(Predicted_arima)
accuracy(Predicted_arima,Test_data[1:20])
Vec<- cbind(Test_data[1:20],Predicted_arima$mean)
ts.plot(Vec, col=c("blue", "red"), main="Gas Production: Actual vs Forecast")
#Auto arima model
Predicted_auto_arima=forecast(Auto_arima_model,h=20)
Vec1<- cbind(Test_data[1:20],Predicted_auto_arima$mean)
ts.plot(Vec1, col=c("blue", "red"), main="Gas Production: Actual vs Forecast")
accuracy(Predicted_auto_arima,Test_data[1:20])

#Forecasting 12 periods
#Fit with seasonality-auto arima model
Fit_with_season=auto.arima(Train_data,seasonal = TRUE)
Predicted_arima_12=forecast(Fit_with_season,h=12)
summary(Predicted_arima_12)
autoplot(Predicted_arima_12)
Vec2<- cbind(Test_data[1:12],Predicted_arima_12$mean)
ts.plot(Vec2, col=c("blue", "red"), main="Gas Production with seasonality(Auto Arima): Actual vs Forecast")


#Fit with seasonality- arima model
Fit_with_season_1=arima(Train_data,c(1,1,1),seasonal =list(order=c(1,1,1),period=12))
Predicted_arima_12_1=forecast(Fit_with_season_1,h=12)
summary(Predicted_arima_12_1)
autoplot(Predicted_arima_12_1,main="Arima model")
Vec3<- cbind(Test_data[1:12],Predicted_arima_12_1$mean)
ts.plot(Vec3, col=c("blue", "red"), main="Gas Production with seasonality(Arima): Actual vs Forecast")



###Model Accuracy
#Auto arima model
accuracy(Predicted_arima_12,Test_data[1:12])
#Arima model
accuracy(Predicted_arima_12_1,Test_data[1:12])
