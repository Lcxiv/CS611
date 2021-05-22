library(TSA)
library(tseries)
library(tidyquant)
library(forecast)
library(boot)


########################################2###################################
set.seed(1234)

y.sim <- arima.sim(n=200, list(order = c(1,3,1), ar = 0.6, ma = -0.7), sd = sqrt(10))+
  100
plot(y.sim)
ndiffs(y.sim)
auto.arima(y.sim)
acf(diff(y.sim))

arima(y.sim, order= c(1,3,1), method = 'ML')
arima(y.sim, order= c(1,3,1), method = 'CSS')
arima(y.sim, order= c(1,3,1), method = 'CSS-ML')

########################################3###################################
#2 reasons: 
  # relationship between samples of autocorrelation values and coefficients are highly non-linear
# solve non-linear systems 
# sampling variations: values of acf that are not usable -> no real solutions
# hard, almost impossible to solve even though they are simple to create

#B
########################################3###################################

#b

ar_1 <- arima.sim(n=200, list(order = c(1,0,0), ar = 0.6), sd = sqrt(10))+
  100
arima(ar_1, order= c(1,0,0), method = 'ML')

ar_2 <- arima.sim(n=200, list(order = c(2,0,0), ar = c(0.6,0.3), sd = sqrt(10)))+
  100
arima(ar_2, order= c(2,0,0), method = 'ML')

########################################4###################################


getSymbols("AAPL", from = '2020-12-01',
           to = "2021-04-27",warnings = FALSE,
           auto.assign = TRUE)
data <- AAPL$AAPL.Close

#A
candleChart(AAPL)
chartSeries(data)
plot(data)

#B
acf(data)
Box.test(data)
adf.test(data)

#C
auto.arima(data)
eacf(data)
#MA(1) seems to be the best fit

#D
AAPL_MA <- auto.arima(data)
#useful for s-e of time-series coeffs 

boot <- tsboot(AAPL_MA)
auto.arima(data, method = 'ML')
table(boot$t[, 1])

#E
data_res <- AAPL_MA$residuals
Box.test(data_res,  type = "Box-Pierce", fitdf = 0)
Box.test(data_res, type = "Ljung-Box", fitdf = 0)
