library(TSA)
library(tseries)
library(tidyquant)
library(forecast)
library(boot)


########################################2###################################
set.seed(2254)

y.sim <- arima.sim(n=500, list(order = c(2,1,2), ar = c(0.6, -0.2), ma = c(-0.7, -0.1), sd = sqrt(6)) +
                     22
plot(y.sim)
ndiffs(y.sim)
auto.arima(y.sim)


arima(y.sim, order= c(2,1,2), method = 'ML')
arima(y.sim, order= c(2,1,2), method = 'CSS')
arima(y.sim, order= c(2,1,2), method = 'CSS-ML')

########################################4###################################

getSymbols("GOOGL", from = '2020-04-01',
           to = "2021-04-27",warnings = FALSE,
           auto.assign = TRUE)
data <- GOOGL$GOOGL.Close

#A
candleChart(GOOGL)
chartSeries(data)
plot(data)

#There seems to be an upward trend as well as some seasonal stationarity from the plot. 
#B
acf(data)
Box.test(data)
adf.test(data)

#From the first test, the data seems stationary, however, the adf test is not significant or even close to be. 

#C
auto.arima(data)
eacf(data)
#ARIMA(2,1,3) seems to be the best model 

#D
GOOGL_MA <- auto.arima(data)
#useful for s-e of time-series coeffs 

#boot <- arima.boot(arima.fit = GOOGL)
auto.arima(data, method = 'ML')

goog.fun <- function(tsb) 
{    ar.fit <- ar(tsb, order.max=25)
c(ar.fit$order, mean(tsb), tsb)
}

boot <- tsboot(GOOGL_MA$residuals, goog.fun, R=99, l=20, sim="geom")
table(boot$t[,1])

#E
data_res <- GOOGL_MA$residuals
Box.test(data_res,  type = "Box-Pierce", fitdf = 0)
Box.test(data_res, type = "Ljung-Box", fitdf = 0)

#Both tests show that the p-value is non significant. Therefore, we cannot say that the residuals behave in an indepdendent manner. 
#I do believe that this is directly correlated with the seasonal trends/patterns that we can observe
