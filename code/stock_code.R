library(astsa)
library(forecast)

#Load Data
load('stocks.Rdata')

#Model 1 - First Difference
differenced = diff(stocks$price)

acf(differenced)
pacf(differenced)

#Model 1 Cross-Validation
train_splits = seq(604,1194-10,10)
rmses = c()
sses = c()
for (index in train_splits){
  train = stocks$price[1:index]
  test = stocks$price[(index+1):(index+10)]
  
  differenced_train = diff(train)
  mean_diff = mean(differenced_train)
  predictions = train[index] + (1:10)*mean_diff
  sse = sum((predictions - test)^2)
  sses = c(sses,sse)
  rmse = sqrt(mean((predictions - test)^2))
  rmses = c(rmses,rmse)
}
sum(sses)
mean(rmses)

#Model 2 - auto.arima

auto.arima(stocks$price, trace = TRUE, allowdrift = FALSE)
AutoArimaStock <- sarima(stocks$price, p=3,d=1,q=1)

#Model 2 Cross-Validation
sumSquares <- c(0)
rootMSE <- c(0)
for(entry in seq(604,1184,10)){
  train_set <- stocks$price[1:entry]
  test_set <- stocks$price[c(entry+1, entry+2, entry+3, entry+4, entry+5, entry+6, entry+7, entry+8, entry+9, entry+10)]
  #
  forecastModelEquation <- arima(train_set, order=c(3,1,1), include.mean = FALSE)
  forecastModel <- predict(forecastModelEquation, n.ahead=10)$pred
  
  #
  SSE <- sum((test_set-forecastModel)^2)
  RMSE <- sqrt(mean((test_set-forecastModel)^2))
  
  #
  sumSquares <- c(sumSquares, SSE)
  rootMSE <- c(rootMSE, RMSE)
}
sum(sumSquares)
mean(rootMSE)

#Model 3 - Weekday and Monthly Indicators
i_model <- lm(data = stocks, price ~ index + I(index^2) + I(month) * I(weekday)) 
summary(i_model)

# plot indicator model on the original data
plot.ts(stocks$price, type = 'l', main = "Stocks Time Series", ylab = "Price", xlab = "Time")
lines(i_model$fitted.values, col = 'red', type = 'l')

# residual plot
plot(i_model$residuals, type = 'l', ylab = "Residuals", xlab = "Time", main = "Residual Plot for Indicator Model")
abline(h = 0)

# acf & pacf of residuals
acf(i_model$residuals)
pacf(i_model$residuals)

#Model 3 Cross-Validation
rmse <- c()
sse <- c()
start <- 604
end <- length(stocks$price)
time <- stocks$index

for (i in seq(start, end - 10, 10)) {
  train_set <- stocks[1:i,]
  test_set <- stocks[c( i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8, i + 9, i + 10), ]
  
  # indicator model forecasting
  i_model <- lm(data = train_set, price ~ index + I(index^2) + I(month) * I(weekday)) 
  
  #inidicator only models seasonality and quadratic trend is modeled by time variable
  i_model_forecast <- predict(i_model, test_set)
  
  # residual forecasting with AR(1) model
  residuals <- i_model$fitted.values - train_set$price
  forecast_resid <- sarima.for(residuals, n.ahead = 10, p = 1, d = 0, q = 0)$pred
  
  # RMSE calculation
  rmse1 <- sqrt(mean(((i_model_forecast + forecast_resid) - test_set$price)^2))
  sse1 <- sum(((i_model_forecast + forecast_resid) - test_set$price)^2) 
  rmse <- c(rmse, rmse1)
  sse <- c(sse, sse1)
}
mean(rmse)
sum(sse)

#Model 4 - Quadratic and Sinusoid Interaction

#periodogram used to identify index of significant frequency
pgram <- abs(fft(sp)[2:nrow(stocks)])^2/nrow(stocks)
plot(pgram[c(1:floor(nrow(stocks)/2))], type = "h", ylab = "Significance")

#index with largest value
j <- which(pgram==max(pgram[c(1:floor(nrow(stocks)/2))]))

#frequency
f <- j/nrow(stocks)

#period
t <- 1/f

#summary
info <- c(j,f,t)
names(info) <- c("Max_index","Frequency", "Period")
info

#sinusoids with frequency found above
single_sin <- sin(2*pi*time*f)
single_cos <- cos(2*pi*time*f)

#fit with cross of quadratic and seasonal trend components
qs_model <- lm(price ~ time * time2 * single_sin * single_cos, data = stocks)

#note the complexity
summary(qs_model)

#plot qs_model
plot(time, sp, type = "l", ylab="Price", xlab = "Time")
lines(time, qs_model$fitted.values, col = "red")

plot(qs_model$residuals, main = "Residuals for qs_model", type = "l")
abline(h=0, col = "red")

par(mfrow = c(2,1))
acf(qs_model$residuals, lag.max = 200)
pacf(qs_model$residuals, lag.max = 200)

#fit AR(1) to the residuals
ar1 <- sarima(qs_model$residuals,1,0,0)

#Model 4 Cross-Validation

window <- seq(604,nrow(stocks)-10,10)
rmse <- c()
for (i in window) {
  #define training and testing sets
  train <- stocks$price[1:i]
  test <- stocks$price[(i+1):(i+10)]
  #specify time range
  range <- 1:i
  #periodogram to find optimal frequency for given test set
  #fit with cross of quadratic and seasonal trend components
  qsm <- lm(train ~ range * I(range^2) * I(sin(2*pi*range*f))* I(cos((2*pi*range*f))))
  #fit AR(1) to the residuals
  ar_residuals <- arima(qsm$residuals, order = c(1,0,0), include.mean = FALSE)
  #predict using test set to measure quality of predictions
  predictions <- predict(qsm, data.frame(range = ((i+1):(i+10)))) + predict(ar_residuals, n.ahead = 10)$pred
  #compute rmse
  rmse <- append(rmse, sqrt(mean((predictions - test)^2)))
}

#average rmse across all windows
rmse
mean(rmse)

#Model 5 - Log VST, Quadratic, AR(1)

#Take Log
stocks$log_price = log(stocks$price)

#Fit Quadratic Model
quad_model = lm(stocks$log_price ~ stocks$index + I(stocks$index^2))
summary(quad_model)

#Check Residuals
acf(quad_model$residuals)
pacf(quad_model$residuals)

#Model AR(1) on Residuals
residuals_ar = arima(quad_model$residuals, order = c(1,0,0), include.mean = FALSE)

#Model 5 Cross-Validation
train_splits = seq(604,1194-10,10)
rmses = c()
sses = c()
for (index in train_splits){
  train = stocks$price[1:index]
  test = stocks$price[(index+1):(index+10)]
  
  log_train = log(train)
  x = 1:index
  quad_model = lm(log_train ~ I(x) + I(x^2) )
  
  residuals_ar = arima(quad_model$residuals, order = c(1,0,0), include.mean = FALSE)
  
  log_predictions = predict(quad_model, data.frame(x = ((index+1):(index+10))))
  + predict(residuals_ar,n.ahead = 10)$pred
  
  predictions = exp(log_predictions)
  
  sse = sum((predictions - test)^2)
  sses = c(sses,sse)
  rmse = sqrt(mean((predictions - test)^2))
  rmses = c(rmses,rmse)
}
sum(sses)
mean(rmses)

#Model 5 Forecast

x = stocks$index
quad_model = lm(stocks$log_price ~ x + I(x^2))
residuals_ar = arima(quad_model$residuals, order = c(1,0,0), include.mean = FALSE)
log_forecast = predict(quad_model, data.frame(x = ((nrow(stocks)+1):(nrow(stocks)+10))))
+ predict(residuals_ar,n.ahead = 10)$pred
forecast = exp(log_forecast)

#Plots

#Figure 1
par(mfrow = c(1,2))
plot(stocks$date, stocks$price, type = "l", ylab = "Price", xlab = "Date", main = "Stocks")
pgram <- abs(fft(stocks$price)[2:nrow(stocks)])^2/nrow(stocks)
plot(pgram[c(1:floor(nrow(stocks)/2))], type = "h", xlab = 'Index', ylab = 'Intensity',
     main = 'Periodogram')

#Figure 2
differenced = diff(stocks$price)
par(mfrow = c(1,2))
plot(stocks$date[2:nrow(stocks)],differenced,type = 'l', ylab = "Difference", xlab = "Date",
     main = expression(bold("First Difference "*D[t])))
acf(differenced, main = expression(bold("Sample ACF of "*D[t])))

#Figure 3
ppq = 4
rs = arima(stocks$price, order = c(3,1,1))$residuals
nlag = 20
pval = rep(0,nlag)
#adapted from sarima function
for (i in (ppq+1):nlag){
  u = Box.test(rs, i, type = "Ljung-Box")$statistic
  pval[i] = pchisq(u, i-ppq, lower.tail=FALSE)
}
par(mfrow = c(1,2))
acf(rs, main = 'Sample ACF of Residuals')
plot( (ppq+1):nlag, pval[(ppq+1):nlag], xlab = "Lag H", ylab = "p-value", ylim = c(-.1, 1),
      main = "P-Values for Ljung-Box Test Statistic")
abline(h = 0.05, lty = 2, col = "blue")

#Figure 4
time <- 1:nrow(stocks)
time2 <- time^2
# model creation
i_model <- lm(stocks$price ~ time + I(time^2) + I(stocks$month) * I(stocks$weekday))
# plotting fitted indicator model on price data
plot(x = stocks$date, y = stocks$price, type = 'l', xlab = 'Date', ylab = 'Price',
     main = 'Fit of Model 3')
lines(x =stocks$date, y= i_model$fitted.values, col = 'red', type = 'l')

#Figure 5
par(mfrow = c(1,2))
plot(x = stocks$date, y = i_model$residuals, type = 'l', xlab = 'Date', ylab = 'Residual',
     main = 'Residuals of Model 3')
lines(x = stocks$date, y= rep(0, nrow(stocks)))
pacf(i_model$residuals, main = 'Partial ACF of Residuals')

#Figure 6
j <- which(pgram==max(pgram[c(1:floor(nrow(stocks)/2))]))
#frequency
f <- j/nrow(stocks)
#period
t <- 1/f

#sinusoids with frequency found above
single_sin <- sin(2*pi*time*f)
single_cos <- cos(2*pi*time*f)

#fit with cross of quadratic and seasonal trend components
qs_model <- lm(price ~ time * time2 * single_sin * single_cos, data = stocks)

#plot qs_model
plot(stocks$date, stocks$price, type = "l", ylab="Price", xlab = "Time", main = "Fit of Model 4")
lines(stocks$date, qs_model$fitted.values, col = "red")

#Figure 7
stocks$log_price = log(stocks$price)
quad_model = lm(stocks$log_price ~ stocks$index + I(stocks$index^2))
par(mfrow = c(1,2))
plot(x = stocks$date, y = stocks$log_price,type = 'l', xlab = 'Date',
     ylab = expression("Log("*Y[t]*")"), main = 'Log of Price')
lines(x = stocks$date, y = quad_model$fitted.values, col = 'red')

plot(x = stocks$date, y = stocks$price,type = 'l', xlab = 'Date', ylab = expression(Y[t]),
     main = 'Price Without Transform')
lines(x = stocks$date, y = exp(quad_model$fitted.values), col = 'red')

#Figure 8
x = stocks$index
quad_model = lm(stocks$log_price ~ x + I(x^2))
residuals_ar = arima(quad_model$residuals, order = c(1,0,0), include.mean = FALSE)
log_forecast = predict(quad_model, data.frame(x = ((nrow(stocks)+1):(nrow(stocks)+10))))
+ predict(residuals_ar,n.ahead = 10)$pred
forecast = exp(log_forecast)

plot(x = 1175:1194, y = stocks$price[1175:1194], type = 'l', xlim = c(1175, 1204),
     ylim = c(min(stocks$price[(1194-20):1194]) - .2, max(forecast)+.2), xlab = 'Index',
     ylab = 'Stock Price', main = 'Model Forecast, Next 10 Trading Days')
lines(x = 1194:1203, y = forecast, col = 'red')