# now compare future forecasts of selected models
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


# load datasets
start_year <- 1989
start_q <- 1
nfor<-12 # forecast horizon - may want to play around with this


# single variable
raw_data<-read_csv("Data/CPIT_quarterly_1989Q1_2021Q3.csv")
dp<-ts(raw_data$Inflation,start=c(start_year,start_q,1),frequency=4)
data<-dp


# multi variable
raw <- read_csv("Data/var_test_data.csv")
raw2 <- read_csv("Data/var_test_data_diff.csv")
raw2 <- raw2 %>% mutate(
  diff_GDP = GDP - lag(GDP),
  GDP_growth = 100*diff_GDP/GDP
)
raw$GDP_growth <- raw2$GDP_growth[2:nrow(raw2)]
data2 <- ts(raw[-1], start = c(start_year, start_q), frequency = 4)


# train models on all data
ar3<-Arima(data,c(5,0,0)) #AR(2) for dy arima(p,d,q)
var1=VAR(data2,p=3,type="const",season=NULL,exog=NULL) #estimate VAR


# create predictions for next nfor periods
fc1 <- forecast(ar3, nfor) #forecast from AR
pred1 <- data.frame(date=as.Date(as.yearqtr(time(fc1$mean))), as.matrix(fc1$mean))
colnames(pred1) <- c('date', 'inflation')
pred1$model <- "AR"

p <- forecast(var1, nfor)$forecast$Inflation
pred2 <- data.frame(date=as.Date(as.yearqtr(time(p$mean))), as.matrix(p$mean))
colnames(pred2) <- c('date', 'inflation')
pred2$model <- "VAR"

# plot predictions
df_plot <- data.frame(date=as.Date(as.yearqtr(time(data))), as.matrix(data))
colnames(df_plot) <- c('date', 'inflation')
df_plot$model <- "actual"
head(df_plot)

df_plot <- rbind(df_plot, pred1, pred2)

df_plot %>%
  ggplot(aes(x = date, y = inflation, group = model, colour = model)) + 
  geom_line() + 
  labs(title = "Future forecasts of inflation models")

ggsave("Plots/future_forecast_comparison", device ="png")

# create predictions with uncertainty measures?