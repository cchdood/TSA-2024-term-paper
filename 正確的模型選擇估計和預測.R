setwd("C:/TSA/Data")
library(forecast)
library(stats)
library(tidyverse)
library(fable)
library(tseries)

#### 


############################################################
############################################################
################  Some Useful Functions  ###################
############################################################


########################  Fourier  #########################

## This function returns a series of fourier terms
# k 是諧波的最高層次數；p為週期波的最大週期；t_start和
# t_end共同決定series的長度。t_start預設為0，不過可以
# 賦予其他數字去調整相位


fourier_term <- function(k = 4, p, t_start = 0, t_end) {
  
  series = NULL
  t = c(t_start:t_end)
  
  for (i in c(1:k)) {
    
    sin_wave = sin(2 * pi * t * i / p)
    cos_wave = cos(2 * pi * t * i / p)
    series = cbind(series, sin_wave, cos_wave)
    
  }
  
  return(series)
  
}

###################  Differencing  #########################

## 差分函式
# d is the order of differencing
# l is the lag of every differencing

differencing <- function(series_temp, d = 1, l = 1) {
  
  series = series_temp
  for (i in c(1:d)) {
    
    h = length(series)
    series = (series - c(rep(0, l), series[1:h - l]))[(l+1):h]
    
  }
  
  return(series)
  
}

############################################################
############################################################
########################  Data  ############################
############################################################

# read weekly data
weekly = read_csv("weekly.csv")


n = length(weekly$X) %/% 2      # training data length
X = weekly$X[1:n]               # 週車禍頻率資料 for training
lagX = c(1, differencing(X))    # note that, the first value 
                                # doesn't affect the holiday variable
Holiday = weekly$D[1:n]         # 春節變數
ts = c(1:n)                     # time term, for detrending

# define period for year & month seasonality
year_period = 365.25 / 7         
month_period = year_period / 12

# 傅立葉項，年/月傅立葉波各設最高6個諧波
four_y = fourier_term(k = 6, p = year_period, 0, n-1)
four_m = fourier_term(k = 6, p = month_period, 0, n-1)

############################################################
############################################################
###################  Model Selection  ######################
############################################################

best_fit = arima(X,  order = c(0, 0, 0), method = "ML", include.mean = TRUE)      ## Candidate 1: constant-only
best_param = c(0, 0, 0, 0)    ## To record the p, q, k1, k2 for the optimal model

for (p in c(0:5)) {                # p = order of AR terms
  for (q in c(0:5)) {              # q = order of MA terms
    
    for (cons in c(TRUE)) {  # include constant or not
      
      for (k1 in c(0:6)) {          # k1 = the level of 諧波 for year fourier term
        for (k2 in c(0:6)) {        # k2 = the level of 諧波 for month fourier term
          
          # include both linear trend & 春節變數
          fit = arima(X,  order = c(p, 0, q), xreg = cbind(four_y[,0:(k1*2)], four_m[,0:(k2*2)], ts, Holiday), method = "CSS-ML", include.mean = cons)
          if (fit$aic < best_fit$aic) {
            best_fit = fit
            best_param = c(p, q, k1, k2)
          }
          fit = arima(X,  order = c(p, 0, q), xreg = cbind(four_y[,0:(k1*2)], four_m[,0:(k2*2)], ts), method = "CSS-ML", include.mean = cons)
          if (fit$aic < best_fit$aic) {
            best_fit = fit
            best_param = c(p, q, k1, k2)
          }
          
          print(c(p, q, k1, k2, cons))
          
        }
      }
    }
  }
}



############################################################
############################################################
###################  Diagnose  ######################
############################################################

best_fit



e_hat = best_fit$residuals
plot(c(1 : length(e_hat)), e_hat, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
acf(e_hat, main = "ACF Plot", lag.max = 100)
pacf(e_hat, main = "PACF Plot", lag.max = 100)

Box.test(e_hat, lag = 10, type = "Ljung-Box")
Box.test(e_hat, lag = 20, type = "Ljung-Box")
Box.test(e_hat, lag = 30, type = "Ljung-Box")
Box.test(e_hat, lag = 40, type = "Ljung-Box")
Box.test(e_hat, lag = 50, type = "Ljung-Box")
Box.test(e_hat, lag = 60, type = "Ljung-Box")

## fitted value
xreg_train <- cbind(rep(1, n), four_y[,1:6], ts, Holiday)
beta = c(best_fit$coef[9:length(best_fit$coef)])
adjusted_X = X - xreg_train %*% beta
X_fitted = adjusted_X
lagX_1 <- c(rep(0, 1), X[1:(n - 1)])
lagX_2 <- c(rep(0, 2), X[1:(n - 2)])
lagX_3 <- c(rep(0, 3), X[1:(n - 3)])
lagX_4 <- c(rep(0, 4), X[1:(n - 4)])

adf.test(X_fitted, k = 20)

############################################################
############################################################
#####################  Forecasting #########################
############################################################

# 定義測試資料
X_test <- weekly$X[(n + 1):length(weekly$X)]
test_n <- length(X_test)  # 測試資料的長度

# 定義測試期間的時間索引和其他相關變數
ts_test <- c((n+1):(n+test_n))
lagX_test1 <- c(tail(X, 1), X_test[1:(test_n - 1)])
Holiday_test <- weekly$D[(n + 1):length(weekly$D)]
four_y_test = fourier_term(k = 3, p = year_period, ts_test[1] - 1, ts_test[length(ts_test)] - 1)

# 組合測試資料的回歸變數
xreg_test <- cbind(rep(1, test_n), four_y_test, ts_test, Holiday_test)
beta = c(best_fit$coef[9:length(best_fit$coef)])
adjusted_X_test = X_test - xreg_test %*% beta

lagX_adj_test1 = c(tail(adjusted_X, 1), adjusted_X_test[1:(test_n - 1)])
lagX_adj_test2 = c(tail(adjusted_X, 2), adjusted_X_test[1:(test_n - 2)])
lagX_adj_test3 = c(tail(adjusted_X, 3), adjusted_X_test[1:(test_n - 3)])
lagX_adj_test4 = c(tail(adjusted_X, 4), adjusted_X_test[1:(test_n - 4)])
ar_terms_test = cbind(lagX_adj_test1, lagX_adj_test2, lagX_adj_test3, lagX_adj_test4)

ma = tail(e_hat, 4)
mas = c(ma)
theta = rev(best_fit$coef[5:8])
fitted_noma = ar_terms_test %*% best_fit$coef[1:4]

for (i in c(1:length(adjusted_X_test))) {
  fit = fitted_noma[i] + sum(ma * theta)
  ma = c(ma[2:length(ma)], (adjusted_X_test[i] - fit))
  mas = c(mas, (adjusted_X_test[i] - fit))
}
lagma_1 = c(tail(best_fit$residuals, 1), mas[1:(test_n - 1)])
lagma_2 = c(tail(best_fit$residuals, 2), mas[1:(test_n - 2)])
lagma_3 = c(tail(best_fit$residuals, 3), mas[1:(test_n - 3)])
lagma_4 = c(tail(best_fit$residuals, 4), mas[1:(test_n - 4)])
ma_terms_test = cbind(lagma_1, lagma_2, lagma_3, lagma_4)

arma_terms = cbind(ar_terms_test, ma_terms_test)
coef_arma = best_fit$coef[1:8]
forecasted_val = arma_terms %*% coef_arma
X_forecasted = forecasted_val + xreg_test %*% beta

plot(c((n + 1) : (n + test_n)), X_forecasted, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
lines(c((n + 1) : (n + test_n)), X_test, type = "l", col="red")

e_hat_test = X_test - X_forecasted
oosR2 = 1 - sum((e_hat_test)^2) / sum((X_test - mean(X))^2)

plot(c((n + 1) : (n + test_n)), e_hat_test, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")


############################################################
############################################################
###########  Forecasting 2 - 沒有季節性調整   ##############
############################################################


for (p in c(0:5)) {                # p = order of AR terms
  for (q in c(0:5)) {              # q = order of MA terms
    
    for (cons in c(TRUE)) {  # include constant or not
      
      for (k1 in c(0:0)) {          # k1 = the level of 諧波 for year fourier term
        for (k2 in c(0:6)) {        # k2 = the level of 諧波 for month fourier term
          
          # include both linear trend & 春節變數
          fit = arima(X,  order = c(p, 0, q), xreg = cbind(four_y[,0:(k1*2)], four_m[,0:(k2*2)], ts), method = "CSS-ML", include.mean = cons)
          if (fit$aic < best_fit$aic) {
            best_fit = fit
            best_param = c(p, q, k1, k2)
          }
          
          print(c(p, q, k1, k2, cons))
          
        }
      }
    }
  }
}

# 定義測試資料
X_test <- weekly$X[(n + 1):length(weekly$X)]
test_n <- length(X_test)  # 測試資料的長度

# 定義測試期間的時間索引和其他相關變數
ts_test <- c((n+1):(n+test_n))
lagX_test1 <- c(tail(X, 1), X_test[1:(test_n - 1)])

# 組合測試資料的回歸變數
xreg_test <- cbind(rep(1, test_n), ts_test)
beta = c(best_fit$coef[6:length(best_fit$coef)])
adjusted_X_test = X_test - xreg_test %*% beta

lagX_adj_test1 = c(tail(adjusted_X, 1), adjusted_X_test[1:(test_n - 1)])
ar_terms_test = cbind(lagX_adj_test1)

ma = tail(e_hat, 4)
mas = c(ma)
theta = rev(best_fit$coef[2:5])
fitted_noma = ar_terms_test %*% best_fit$coef[1]

for (i in c(1:length(adjusted_X_test))) {
  fit = fitted_noma[i] + sum(ma * theta)
  ma = c(ma[2:length(ma)], (adjusted_X_test[i] - fit))
  mas = c(mas, (adjusted_X_test[i] - fit))
}
lagma_1 = c(tail(best_fit$residuals, 1), mas[1:(test_n - 1)])
lagma_2 = c(tail(best_fit$residuals, 2), mas[1:(test_n - 2)])
lagma_3 = c(tail(best_fit$residuals, 3), mas[1:(test_n - 3)])
lagma_4 = c(tail(best_fit$residuals, 4), mas[1:(test_n - 4)])
ma_terms_test = cbind(lagma_1, lagma_2, lagma_3, lagma_4)

arma_terms = cbind(ar_terms_test, ma_terms_test)
coef_arma = best_fit$coef[1:5]
forecasted_val = arma_terms %*% coef_arma
X_forecasted = forecasted_val + xreg_test %*% beta

plot(c((n + 1) : (n + test_n)), X_forecasted, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
lines(c((n + 1) : (n + test_n)), X_test, type = "l", col="red")

e_hat_test = X_test - X_forecasted
oosR2 = 1 - sum((e_hat_test)^2) / sum((X_test - mean(X))^2)

plot(c((n + 1) : (n + test_n)), e_hat_test, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")



############################################################
############################################################
#####################  STL #########################
############################################################

modelstl = lm(X ~ Holiday)
ts_data <- ts(modelstl$residuals, start = c(2012, 1), frequency = 52)
stl_result <- stl(ts_data, s.window = "periodic")
plot(stl_result)

seasonal_component <- stl_result$time.series[, "seasonal"]
trend_component <- stl_result$time.series[, "trend"]
residual_component <- stl_result$time.series[, "remainder"]

y = residual_component
y = c(y)
plot(c(1 : length(y)), y, type = "o", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
acf(y, main = "ACF Plot", lag.max = 25)
pacf(y, main = "PACF Plot", lag.max = 25)


