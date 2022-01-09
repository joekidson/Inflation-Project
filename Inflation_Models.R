# set up
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(tsbox)
library(tsDyn)
library(stringr)
library(ggfortify)
library(zoo)

# load example data amd set key parameters for training/testing models
raw_data<-read_csv("Data/CPIT_quarterly_1989Q1_2021Q3.csv")
start_year <- 1989
start_q <- 1
end_row <- nrow(raw_data)
test_year <- 2018
test_q <- 1
test_row <- which(raw_data$Quarter == sprintf("%i Q%i", test_year, test_q))
test_size <- end_row - test_row + 1

# 1. Single inflation time series models
nfor<-1 # forecast horizon - may want to play around with this
dp<-ts(raw_data$Inflation,start=c(start_year,start_q,1),frequency=4)
data<-dp

#recursive forecasting 
rmse_tar=matrix(nrow=test_size,ncol=1) #will hold TAR RMSE
fe_tar=matrix(nrow=test_size,ncol=1) #will hold TAR forecast error
rmse_star=matrix(nrow=test_size,ncol=1) #will hold STAR RMSE
fe_star=matrix(nrow=test_size,ncol=1) #will hold STAR forecast error
rmse_ar=matrix(nrow=test_size,ncol=1) #will hold AR RMSE
fe_ar=matrix(nrow=test_size,ncol=1) #will hold AR forecast error
rmse_av=matrix(nrow=test_size,ncol=1) #will hold mean forecast RMSE
fe_av=matrix(nrow=test_size,ncol=1) #will hold mean forecast error


tarf=matrix(nrow=test_size,ncol=1)
starf=matrix(nrow=test_size,ncol=1)
arf=matrix(nrow=test_size,ncol=1)
avf=matrix(nrow=test_size,ncol=1)
outturn=matrix(nrow=test_size,ncol=1)
dd<-time(data)  ##dd are the dates

tt<-1
for(t in (test_row-1):(end_row-1)) { #loop from 2018Q1 to 2021Q3 for estimation #76:148
  print(t)
  training<-window(data,c(start_year, start_q),dd[t]) #estimation sample
  testdata<-window(data,dd[t+1],dd[t+nfor]) #forecast sample
  #TAR Estimation and Forecasting
  #sel<-selectSETAR(training,mL=2,mH=2,include='const',trace=TRUE,thDelay=1:2,plot=FALSE)
  tar1<-setar(training, mL=2, mH=2, thDelay=1, include="const")
  #tar1$coefficients
  p<-predict(tar1, n.ahead=nfor, type="MC", n.boot=200)
  fc_tar<-p$pred #extract forecast
  tarf[tt]<-fc_tar[nfor]
  outturn[tt]<-testdata[nfor]
  fe<-accuracy(fc_tar,testdata)
  rmse_tar[tt]<-fe[,"RMSE"]
  fcx<-ts(fc_tar,start=dd[t+1],frequency=4)
  e1<-fcx-testdata #forecast error
  fe_tar[tt]<-e1[nfor]
  
  #STAR model EStimation and Forecasting
  star1<-lstar(training, mL=2, mH=2, thDelay=1, include="const")
  #summary(star1)
  p<-predict(star1, n.ahead=nfor, type="MC", n.boot=200)
  fc_star<-p$pred #extract GDP growth forecast
  starf[tt]<-fc_star[nfor]
  fe<-accuracy(fc_star,testdata)
  rmse_star[tt]<-fe[,"RMSE"]
  fcx<-ts(fc_star,start=dd[t+1],frequency=4)
  e1<-fcx-testdata #forecast error
  fe_star[tt]<-e1[nfor]
  
  #AR estimation and forecasting
  ar3<-Arima(training,c(5,0,0)) #AR(2) for dy arima(p,d,q)
  fc1 <- forecast(ar3, nfor) #forecast from AR
  fe1<-accuracy(fc1,testdata) #RMSE
  arf[tt]<-fc1$mean[nfor]
  e2<-fc1$mean-testdata #Forecast error
  rmse_ar[tt]<-fe1[,"RMSE"][2]
  fe_ar[tt]<-e2[nfor]
  
  #average forecast
  fc_av=(fc_tar+fc_star+fc1$mean)/3
  fe2<-accuracy(fc_av,testdata) #RMSE
  avf[tt]<-fc_av[nfor]
  e3<-fc_av-testdata #Forecast error
  rmse_av[tt]<-fe2[,"RMSE"]
  fe_av[tt]<-e3[nfor]
  
  
  
  tt=tt+1
}

rmse_df <- data.frame(rmse_tar, rmse_star, rmse_ar, rmse_av)
colnames(rmse_df) <- c("TAR", "STAR", "AR", "Average")
rmse <- ts(rmse_df, start=dd[test_row], frequency=4)

f_df <- data.frame(tarf, starf, arf, avf, outturn)
colnames(f_df) <- c("TARF", "STARF", "ARF", "Average", "Actual")
forecast <- ts(f_df, start=dd[test_row],frequency=4)

# clean up these plots
autoplot(rmse, facets=FALSE)
autoplot(forecast, facets = FALSE)

df_rmse <- data.frame(date=as.Date(as.yearqtr(time(rmse))), as.matrix(rmse))
head(df_rmse)
df_rmse <- df_rmse %>%
  pivot_longer(!date, values_to = "RMSE", names_to = "Model")
df_rmse %>%
  ggplot(aes(x = date, y = RMSE, group = Model, colour = Model)) + 
  geom_line() +
  labs(title = "RMSE of single variable inflation models 2018 Q1 - 2021 Q3")

df_forecast <- data.frame(date=as.Date(as.yearqtr(time(forecast))), as.matrix(forecast))
head(df_forecast)
df_forecast <- df_forecast %>%
  pivot_longer(!date, values_to = "forecast", names_to = "Model")
df_forecast %>%
  ggplot(aes(x = date, y = forecast, group = Model, colour = Model)) + 
  geom_line() + 
  labs(title = "Forecasts of single variable inflation models 2018 Q1 - 2021 Q3")

# create plot of future forecast.
# should be class code for this
# will want one on best models and other forecasts

# what would the plots be for longer forecast horizons? - same but can't go as far, so have to adapt code




# -------------- OLD

# par(mfrow=(c(1,2)))
# plot(rmse1)
# lines(rmse2,lty = 2, col = "red")
# lines(rmse3,lty = 3, col = "blue")
# lines(rmse4,lty = 4, col = "yellow")
# legend("topleft", legend=c("TAR", "STAR","AR","average"),
#        col=c("black", "red","blue","pink"), lty=1:4, cex=0.1)
# title('RMSE across time')
# 
# plot(tarf,col="blue")
# lines(starf,lty = 2, col = "red")
# lines(arf,lty = 3, col = "red")
# lines(avf,lty = 4, col = "pink")
# lines(outturn,lty = 5, col = "green")
# legend(2006.5, -0.1, legend=c("TAR", "STAR","AR","average","data"),
#        col=c("blue", "red","blue","pink","green"), lty=1:5, cex=0.2)
# title('forecasts')
