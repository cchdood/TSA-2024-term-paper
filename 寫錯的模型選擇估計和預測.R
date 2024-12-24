setwd("C:/TSA/Data")
library(forecast)
library(stats)
library(tidyverse)

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
    
    sin_wave = sin(2 * pi * t * k / p)
    cos_wave = cos(2 * pi * t * k / p)
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
Holiday = weekly$D[1:n] * lagX  # 春節變數
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

############ Model 1: ##############################
model0 = lm(X ~ ts + Holiday)

model1 = lm(X ~ four_y + four_m + ts + Holiday)

model2 = lm(X ~ four_y + four_sy + four_m + ts + Holiday)

model3 = lm(X ~ four_2y + four_y + four_sy + four_m + ts + Holiday)

model4 = lm(X ~ four_y + four_sy + four_m + ts + Holiday)

model5 = lm(X ~ four_m + ts + Holiday)


Y0 = model0$residuals
Y1 = model1$residuals
Y2 = model2$residuals
Y3 = model3$residuals
Y4 = model4$residuals
Y5 = model5$residuals

# Time Series Plot
plot(ts, Y0, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
acf(Y0, main = "ACF Plot", lag.max = 25)
pacf(Y0, main = "PACF Plot", lag.max = 25)

plot(ts, Y1, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
acf(Y1, main = "ACF Plot", lag.max = 25)
pacf(Y1, main = "PACF Plot", lag.max = 25)

plot(ts, Y2, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
acf(Y2, main = "ACF Plot", lag.max = 25)
pacf(Y2, main = "PACF Plot", lag.max = 25)

plot(ts, Y3, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
acf(Y3, main = "ACF Plot", lag.max = 25)
pacf(Y3, main = "PACF Plot", lag.max = 25)

plot(ts, Y4, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
acf(Y4, main = "ACF Plot", lag.max = 25)
pacf(Y4, main = "PACF Plot", lag.max = 25)

plot(ts, Y5, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
acf(Y5, main = "ACF Plot", lag.max = 25)
pacf(Y5, main = "PACF Plot", lag.max = 25)

Y_d = differencing(d = 1, series_temp = X)
plot(c(1 : length(Y_d)), Y_d, type = "o", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
acf(Y_d, main = "ACF Plot", lag.max = 25)
pacf(Y_d, main = "PACF Plot", lag.max = 25)

Y4_d = differencing(d = 1, series_temp = Y4)
plot(c(1 : length(Y4_d)), Y4_d, type = "o", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
acf(Y4_d, main = "ACF Plot", lag.max = 25)
pacf(Y4_d, main = "PACF Plot", lag.max = 25)


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

best_fit = arima(X,  order = c(0, 0, 0), xreg = ts, method = "ML", include.mean = FALSE)

fit = arima(X, xreg = cbind(ts, Holiday), max.p = 15, max.d = 3, max.q = 15)
if (fit$aic < best_fit$aic) {
  best_fit = fit
}
for (i in c(1:6)) {
  fit = ARIMU(X, xreg = cbind(four_y[,1:(i*2)], ts, Holiday), max.p = 15, max.d = 3, max.q = 15)
  if (fit$aic < best_fit$aic) {
    best_fit = fit
  }
  fit = auto.arima(X, xreg = cbind(four_m[,1:(i*2)], ts, Holiday), max.p = 15, max.d = 3, max.q = 15)
  if (fit$aic < best_fit$aic) {
    best_fit = fit
  }
  fit = auto.arima(X, xreg = cbind(four_y[,1:(i*2)], ts), max.p = 15, max.d = 3, max.q = 15)
  if (fit$aic < best_fit$aic) {
    best_fit = fit
  }
  fit = auto.arima(X, xreg = cbind(four_m[,1:(i*2)], ts), max.p = 15, max.d = 3, max.q = 15)
  if (fit$aic < best_fit$aic) {
    best_fit = fit
  }
  for (j in c(1:6)) {
    fit = auto.arima(X, xreg = cbind(four_y[,1:(i*2)], four_m[,1:(j*2)], ts, Holiday), max.p = 15, max.d = 3, max.q = 15)
    if (fit$aic < best_fit$aic) {
      best_fit = fit
    }
    fit = auto.arima(X, xreg = cbind(four_y[,1:(i*2)], four_m[,1:(j*2)], ts), max.p = 15, max.d = 3, max.q = 15)
    if (fit$aic < best_fit$aic) {
      best_fit = fit
    }
  }
}

e_hat = best_fit$residuals
plot(c(1 : length(e_hat)), e_hat, type = "o", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
acf(e_hat, main = "ACF Plot", lag.max = 100)
pacf(e_hat, main = "PACF Plot", lag.max = 100)

checkresiduals(best_fit)
Box.test(e_hat, lag = 10, type = "Ljung-Box")
Box.test(e_hat, lag = 20, type = "Ljung-Box")
Box.test(e_hat, lag = 30, type = "Ljung-Box")
Box.test(e_hat, lag = 40, type = "Ljung-Box")
Box.test(e_hat, lag = 50, type = "Ljung-Box")
Box.test(e_hat, lag = 60, type = "Ljung-Box")

# 定義測試資料
X_test <- weekly$X[(n + 1):length(weekly$X)]
test_n <- length(X_test)  # 測試資料的長度

# 定義測試期間的時間索引和其他相關變數
ts_test <- c((n+1):(n+test_n))
lagX_test1 <- c(tail(X, 1), X_test[1:(test_n - 1)])
lagX_test2 <- c(tail(X, 2), X_test[1:(test_n - 2)])
lagX_test3 <- c(tail(X, 3), X_test[1:(test_n - 3)])
Holiday_test <- weekly$D[(n + 1):length(weekly$D)] * lagX_test1

# 測試資料的 Fourier 項
fsin_y1_test <- sin(2 * pi * ts_test / year_period)
fsin_y2_test <- sin(2 * pi * ts_test * 2 / year_period)
fsin_y3_test <- sin(2 * pi * ts_test * 3 / year_period)
fcos_y1_test <- cos(2 * pi * ts_test / year_period)
fcos_y2_test <- cos(2 * pi * ts_test * 2 / year_period)
fcos_y3_test <- cos(2 * pi * ts_test * 3 / year_period)
four_y_test <- cbind(fsin_y1_test, fcos_y1_test, fsin_y2_test, fcos_y2_test, fsin_y3_test, fcos_y3_test)

# 組合測試資料的回歸變數
xreg_test <- cbind(lagX_test1, lagX_test2, lagX_test3, rep(1, test_n), four_y_test, ts_test, Holiday_test)
beta = c(best_fit$coef[1:3], best_fit$coef[5:length(best_fit$coef)])

ma = tail(e_hat, 1)
mas = c(ma)
fitted_noma = xreg_test %*% beta

for (i in c(1:length(X_test))) {
  fit = fitted_noma[i] + ma
  ma = X_test[i] - fit
  mas = c(mas, ma)
}
ma_lag = mas[1:length(X_test)]

xreg_test = cbind(lagX_test1, lagX_test2, lagX_test3, ma_lag, rep(1, test_n), four_y_test, ts_test, Holiday_test)
beta = best_fit$coef
forecasted_val = xreg_test %*% beta

plot(c((n + 1) : (n + test_n)), forecasted_val, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")
lines(c((n + 1) : (n + test_n)), X_test, type = "l", col="red")

e_hat_test = X_test - forecasted_val
oosR2 = 1 - sum((e_hat_test)^2) / sum((X_test - mean(X))^2)

plot(c((n + 1) : (n + test_n)), e_hat_test, type = "l", col = "blue",
     xlab = "Time", ylab = "Value", main = "Time Series Plot")


