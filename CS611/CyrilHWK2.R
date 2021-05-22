library(tidyquant)
library(forecast)

getSymbols("PFE", from = '2019-01-01',
           to = "2021-12-12",warnings = FALSE,
           auto.assign = TRUE)

# Time series graph with forecst
candleChart(PFE,multi.col=TRUE,theme="white")

model=auto.arima(PFE$PFE.Close)
forecast(model,20)
plot(forecast(model,20))

# Candlestick graphs
chart_Series(PFE,name="PFE")
chart_Series(PFE,name="PFE",subset="2021-01/2021-03")
