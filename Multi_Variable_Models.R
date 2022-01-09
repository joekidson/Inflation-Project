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
library(dplyr)

# load up the data
raw <- read_csv("Data/var_test_data.csv")
head(raw)

# load and join in data that needs to be differenced
raw2 <- read_csv("Data/var_test_data_diff.csv")
head(raw2)
raw2 <- raw2 %>% mutate(
  diff_GDP = GDP - lag(GDP),
  GDP_growth = 100*diff_GDP/GDP
)

raw$GDP_growth <- raw2$GDP_growth[2:nrow(raw2)]
View(raw)

# key variables
start_year <- 1989
start_q <- 1
end_row <- nrow(raw)
test_year <- 2018
test_q <- 1
test_row <- which(raw$Quarter == sprintf("%i Q%i", test_year, test_q))
test_size <- end_row - test_row + 1
nfor<-1 # used in loop below as number of quarters forecast ahead

data <- ts(raw[-1], start = c(start_year, start_q), frequency = 4)

#recursive forecasting
rmse_var=matrix(nrow=test_size,ncol=1) #will hold VAR RMSE
rmse_ar=matrix(nrow=test_size,ncol=1) #will hold AR RMSE
rmse_ma=matrix(nrow=test_size,ncol=1) #will hold MA RMSE
rmse_av=matrix(nrow=test_size,ncol=1)
varf=matrix(nrow=test_size,ncol=1)
arf=matrix(nrow=test_size,ncol=1)
maf=matrix(nrow=test_size,ncol=1)
avf=matrix(nrow=test_size,ncol=1)
actual=matrix(nrow=test_size,ncol=1)

dd=time(data)  ##dd are the dates
tt<-1

# Q1/2
for(t in (test_row-1):(end_row-1)) { #loop from 2007Q1 to 2011Q4 for estimation
  
  training<-window(data,c(start_year, start_q),dd[t]) #estimation sample
  testdata<-window(data,dd[t+1],dd[t+nfor]) #forecast sample
  
  #VAR(3) Estimation and Forecasting
  var1=VAR(training,p=3,type="const",season=NULL,exog=NULL) #estimate VAR
  p <- predict(var1, n.ahead=nfor) #forecast
  fc=p$fcst[[1]][,"fcst"] #extract CPI growth forecast
  fe<-accuracy(fc,testdata[,1])
  rmse_var[tt]<-fe[,"RMSE"]
  varf[tt]<-fc
  actual[tt] <- testdata[,1][1]
  
  #AR(3) estimation and forecasting
  ar3<-Arima(training[,1],c(3,0,0)) #AR(3) for dp arima(p,d,q)
  fc1 <- forecast(ar3, nfor) #forecast from AR
  fe1<-accuracy(fc1,testdata[,1]) #RMSE
  rmse_ar[tt]<-fe1[,"RMSE"][2]
  arf[tt] <- fc1$mean[nfor]
  
  #MA(1) model
  ma3<-Arima(training[,1],c(0,0,3)) #MA(3) for dp arima(p,d,q)
  fc1 <- forecast(ma3, nfor) #forecast from MA
  fe1<-accuracy(fc1,testdata[,1]) #RMSE
  rmse_ma[tt]<-fe1[,"RMSE"][2]
  maf[tt] <- fc1$mean[nfor]
  
  # shift forward a quarter for next loop
  tt=tt+1
}

rmse_df <- data.frame(rmse_var, rmse_ar, rmse_ma)
colnames(rmse_df) <- c("VAR", "AR", "MA")
rmse <- ts(rmse_df, start=dd[test_row], frequency=4)

f_df <- data.frame(varf, arf, maf, actual)
colnames(f_df) <- c("VARF", "ARF", "MAF", "Actual")
forecast <- ts(f_df, start=dd[test_row],frequency=4)

head(f_df)

# plots
df_rmse <- data.frame(date=as.Date(as.yearqtr(time(rmse))), as.matrix(rmse))
head(df_rmse)
df_rmse <- df_rmse %>%
  pivot_longer(!date, values_to = "RMSE", names_to = "Model")
df_rmse %>%
  ggplot(aes(x = date, y = RMSE, group = Model, colour = Model)) + 
  geom_line() +
  labs(title = "RMSE of multi variable inflation models + baseline 2018 Q1 - 2021 Q3")

df_forecast <- data.frame(date=as.Date(as.yearqtr(time(forecast))), as.matrix(forecast))
head(df_forecast)
df_forecast <- df_forecast %>%
  pivot_longer(!date, values_to = "forecast", names_to = "Model")
df_forecast %>%
  ggplot(aes(x = date, y = forecast, group = Model, colour = Model)) + 
  geom_line() + 
  labs(title = "Forecasts of multi variable inflation models + baseline 2018 Q1 - 2021 Q3")
