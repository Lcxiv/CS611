library(TSA)
data(oil.price)

plot(diff(log(oil.price)))
model <- arima(diff(log(oil.price)), order = c(0,0,1), method = 'ML')
acf(model)
Box.test(model)