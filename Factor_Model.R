#Set Working Directory and Load Libraries
library(dplyr)
library(vars)
library(tidyverse)
library(nowcasting)
library(forecast)
library(magrittr)

#Read in data from excel. X matrix is for factors (choosing the columns).
#Y,P,R are the variables for the simple BVAR - change these depending on our data. Need to be in same excel file as the factors.

nfor<-1 #Used in loop below for how far to forecast ahead
df<-read.csv("Data/disagg_data_merged2.csv",1) #read from excel file
df_wide <- df %>%
  dplyr::select(index_date, item_id, item_index) %>%
  pivot_wider(id_cols = index_date, names_from = item_id, values_from = item_index)
include <- c("01", "04", "07", "10")
df_wide2 <- df_wide %>% filter(substr(index_date, 5, 6) %in% include)
df_nas <- df_wide2 %>%
  dplyr::select(!index_date) %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "item_index", values_to = "nas")
df_keep <- df_nas %>% filter(nas == 0)
df_wide2 <- df_wide2 %>% dplyr::select(index_date, df_keep$item_index)

X<-as.matrix((df_wide2[,2:ncol(df_wide2)]))  #X is large set of factors.

df1<-read.csv("Data/CPIH_quarterly_2005Q2_2019Q3.csv",1)


Y<-df1$Inflation
Y<-ts(Y,start=c(2005,2,1),frequency=4) #Y is Inflation


#Format factors as time series
X<-ts(X,start=c(2005,2,1),frequency=4)
dd=time(X)  ##dd are the dates


end_row <- nrow(df1)
test_year <- 2012    #Choose the start of the testing sample
test_q <- 1          #Starting Quarter
test_row <- 28       #Corresponding row for 2012 Q1
test_size <- end_row - test_row + 1 #Calculating size of rows in testing sample

rmse_favar=matrix(nrow=test_size,ncol=1) #will hold AR RMSE
fe_favar=matrix(nrow=test_size,ncol=1) #will hold AR forecast error
actuals=matrix(nrow=test_size,ncol=1)
tt<-1
for(t in (test_row-1):(end_row-1)){ #Training Sample - Test Row-1 is end of first training period. Last training period goes up to penultimate quarter (so nrow-1).
x_training<-window(X,c(2005,2),dd[t]) #estimation sample      
y_training<-window(Y,c(2005,2),dd[t]) #estimation sample      
x_training<-scale(x_training)  # Standardising line 47 - so that the variables with large values (i.e. millions) don't dominate in the algorithm. Does it by subtracting mean and dividing by Standard Dev.
y_testdata<-window(Y,dd[t+1],dd[t+nfor]) #forecast sample - select quarter ahead of training data

#print(y_testdata[nfor])

#estimate and forecast factor var - FACTOR MODEL
tmp<-ICfactors(x_training,rmax=10,type=1)   #Choosing optimum number of factors from X, with a maximum of 10.
K<-tmp$r_star
tmp1<-prcomp(x_training,rank.=K)    #prcomp function will estimate the factors using principal components. Takes in the dataset and the number of factors (rank), which is chosen by the bai ng criteria.
FF<-tmp1$x #factors
FF<-ts(FF,start=c(2005,2,1),frequency=4)     #converge the estimated factors into a time series, to use in a VAR.
YY<-cbind(y_training,FF) #Data for the factor augmented VAR - consists of Y and the estimated factors.
var2=VAR(YY,p=2,type="const",season=NULL,exog=NULL) #estimate VAR using OLS, and 2 lags.
p <- predict(var2, n.ahead=nfor) #forecast the VAR
fc=p$fcst[[1]][,"fcst"] #extract inflation forecast - selecting forecast of the first variable (inflation), one we are interested in.
fe<-accuracy(fc,y_testdata[nfor])   #save the forecasts and the RMSE.
rmse_favar[tt]<-fe[,"RMSE"]
fe_favar[tt]<-fc
actuals[tt]<-y_testdata[nfor]
tt<-tt+1
}

#We will want to change this slightly - Haroon said to add the factors to the BVAR with all the macro variables. This code only adds the factors to the Y variable.
#Next step is to forecast as usual, will want to use Sabiha's BVAR code for this

rmse_df <- data.frame(rmse_favar)
colnames(rmse_df) <- c("FVAR")
rmse <- ts(rmse_df, start=dd[test_row], frequency=4)

f_df <- data.frame(fe_favar, actuals)
colnames(f_df) <- c("FVARF", "Actual")
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

ggsave("Plots/fvar_RMSE_comparison", device ="png")

df_forecast <- data.frame(date=as.Date(as.yearqtr(time(forecast))), as.matrix(forecast))
head(df_forecast)
df_forecast <- df_forecast %>%
  pivot_longer(!date, values_to = "forecast", names_to = "Model")
df_forecast %>%
  ggplot(aes(x = date, y = forecast, group = Model, colour = Model)) + 
  geom_line() + 
  labs(title = "Forecasts of multi variable inflation models + baseline 2018 Q1 - 2021 Q3")

ggsave("Plots/fvar_forecast_comparison", device ="png")

