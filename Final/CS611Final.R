library(tidyquant)
library(forecast)
library(qmao)
library(quantmod)
library(zoo)
require(plyr)
library(DataCombine)
library(TSA)

#####################################
#1A

ticks <- c('FB','AAPL', 'INTC', 'IBM', 'NVDA', 'SIRI', 'MRNA', 'GOOGL', 'AMZN', 'WFC', 'GE', 'EBAY', 'VZ', 'DIS', 'NKE', 'WMT', 'NFLX', 'TSLA', 'TGT')
getSymbols(ticks,
           from = '2016-01-01',
           to = "2021-5-18",warnings = FALSE,
           auto.assign = TRUE)


ClosePrices <- do.call(merge, lapply(ticks, function(x) Cl(get(x))))
#slide_index(ClosePrices, i, ~.ClosePrices, .before = 1)

#Creating different windonws non-overlapping

a<-rollapply(ClosePrices$FB.Close, 100, sd)
b <- rollapply(ClosePrices$AAPL.Close, 200, sd)
c <- rollapply(ClosePrices$INTC.Close, 300, sd)
d <- rollapply(ClosePrices$INTC.Close, 400, sd)
e <- rollapply(ClosePrices$IBM.Close, 500, sd)
f <- rollapply(ClosePrices$NVDA.Close, 600, sd)
g <- rollapply(ClosePrices$SIRI.Close, 700, sd)
h <- rollapply(ClosePrices$MRNA.Close, 1150, sd)
i <- rollapply(ClosePrices$GOOGL.Close, 900, sd)
j <- rollapply(ClosePrices$WFC.Close, 1000, sd)
k <- rollapply(ClosePrices$GE.Close, 1100, sd)
l <- rollapply(ClosePrices$VZ.Close, 1200, sd)
m <- rollapply(ClosePrices$DIS.Close, 1300, sd)
n <- rollapply(ClosePrices$NKE.Close, 150, sd)
o <- rollapply(ClosePrices$WMT.Close, 250, sd)
p <- rollapply(ClosePrices$NFLX.Close, 350, sd)
q <- rollapply(ClosePrices$TSLA.Close, 450, sd)
r <- rollapply(ClosePrices$TGT.Close, 550, sd)

#Creating a dataframe foreach 
aa <- data.frame(t(arimaorder(auto.arima(a))))
bb <- data.frame(t(arimaorder(auto.arima(b))))
cc <- data.frame(t(arimaorder(auto.arima(c))))
dd <- data.frame(t(arimaorder(auto.arima(d))))
ee <- data.frame(t(arimaorder(auto.arima(e))))
ff <- data.frame(t(arimaorder(auto.arima(f))))
gg <- data.frame(t(arimaorder(auto.arima(g))))
hh <- data.frame(t(arimaorder(auto.arima(h))))
ii <- data.frame(t(arimaorder(auto.arima(i))))
jj <- data.frame(t(arimaorder(auto.arima(j))))
kk<- data.frame(t(arimaorder(auto.arima(k))))
ll <- data.frame(t(arimaorder(auto.arima(l))))
mm <- data.frame(t(arimaorder(auto.arima(m))))
nn <- data.frame(t(arimaorder(auto.arima(n))))
oo <- data.frame(t(arimaorder(auto.arima(o))))
pp <- data.frame(t(arimaorder(auto.arima(p))))
qq <- data.frame(t(arimaorder(auto.arima(q))))
rr <- data.frame(t(arimaorder(auto.arima(r))))

#Joining all the stocks into one dataframe

df <- join_all(list(aa,bb,cc,dd,ee,ff,gg,hh,ii,jj,kk,ll,mm,nn,oo,pp,qq,rr), type = 'full')

#Calculating the frequency of the different models based on the dataframe created

df$frequency <- row.freq(df)
freq <- table(df)
View(freq)


######################################################################################

#1B 

ticks <- c('FB','AAPL', 'INTC', 'IBM', 'NVDA', 'SIRI', 'MRNA', 'GOOGL', 'AMZN', 'WFC', 'GE', 'EBAY', 'VZ', 'DIS', 'NKE', 'WMT', 'NFLX', 'TSLA', 'TGT')
getSymbols(ticks,
           from = '2019-05-15',
           to = "2021-5-18",warnings = FALSE,
           auto.assign = TRUE)

ClosePrices <- do.call(merge, lapply(ticks, function(x) Cl(get(x))))

#Same principle as 1A

goodness <- data.frame(good_arima = NA, bad_arima = NA, good_HW = NA, bad_HW = NA)

#Adding counts based on the p-value

count_good_AR <- 0
count_bad_AR <-0
count_good_HW <- 0
count_bad_HW <-0
total_count <- 0
num <- 400

#Using a while loop to decrease the window by one each iteration until finding the first stock price

while(num>2){
  a <-rollapply(ClosePrices$FB.Close, num, sd)
  b <- rollapply(ClosePrices$AAPL.Close, num, sd)
  c <- rollapply(ClosePrices$INTC.Close, num, sd)
  d <- rollapply(ClosePrices$INTC.Close, num, sd)
  e <- rollapply(ClosePrices$IBM.Close, num, sd)
  f <- rollapply(ClosePrices$NVDA.Close, num, sd)
  g <- rollapply(ClosePrices$SIRI.Close, num, sd)
  h <- rollapply(ClosePrices$MRNA.Close, num, sd)
  i <- rollapply(ClosePrices$GOOGL.Close, num, sd)
  j <- rollapply(ClosePrices$WFC.Close, num, sd)
  k <- rollapply(ClosePrices$GE.Close, num, sd)
  l <- rollapply(ClosePrices$VZ.Close, num, sd)
  m <- rollapply(ClosePrices$DIS.Close, num, sd)
  n <- rollapply(ClosePrices$NKE.Close, num, sd)
  o <- rollapply(ClosePrices$WMT.Close, num, sd)
  p <- rollapply(ClosePrices$NFLX.Close, num, sd)
  q <- rollapply(ClosePrices$TSLA.Close, num, sd)
  r <- rollapply(ClosePrices$TGT.Close, num, sd)

  
  list_data <- list(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
  
  #For loop doing the calculations for each iteration of the window
  for (i in seq_along(list_data)){
    
    list_data[[i]]<- DropNA(list_data[[i]])
    
    #plot(forecast(list_data[[i]], h = 10))
    #plot(HoltWinters(list_data[[i]], gamma = FALSE))
    test_AR <- Box.test(forecast(auto.arima(list_data[[i]]), h = 10), type = "Box-Pierce")
    HW <- forecast(HoltWinters(list_data[[i]], gamma = F))
    test_HW <- Box.test(forecast(HW, h = 10), type = "Box-Pierce")
    if(test_AR[[3]] < .05){
      count_good_AR <- count_good_AR + 1
    }else{
      count_bad_AR <- count_bad_AR + 1
    }
    if(test_HW[[3]] < .05){
      count_good_HW <- count_good_HW + 1
    }else{
      count_bad_HW <- count_bad_HW + 1
    }
    total_count <- total_count +1
    
  }

  num <- num-1
  #adding the results to the dataframe to calculate the percentages
  goodness$good_arima <- count_good_AR
  goodness$bad_arima <- count_bad_AR
  good_HW <- count_good_HW
  bad_HW <- count_bad_HW
}
goodness$Ar_Percentage_good_models <- count_good_AR/total_count*100
goodness$HW_Percentage_good_models <- count_good_HW/total_count*100
View(goodness)


##########################################

#10.9
#A

data("airpass")

log_data<- log(airpass)

plot(airpass, type='o',ylab='Air Passengers')

plot(log_data, type='o',ylab='Log(Air Passengers)')

#The log is useful to picture a better upward trend without being affected too much by outliers

plot(diff(airpass),type='o',ylab='Difference of Log(Air Passengers)')
plot(diff(log_data),type='o',ylab='Difference of Log(Air Passengers)')

#This seems to show a stationary process, however, we have a lot of outliers

#C
diff_log <- diff(log_data)
points(diff(diff_log,lag=12),x=time(diff(diff_log,lag=12)),
       pch=as.vector(season(diff(diff_log,lag=12))))
#As the data shows the monts, we can see that there is not really any seasonal pattern beside January being on average lower

#D

acf(diff(diff_log),lag=12)

#Lag one seems suspicious as it is way higher than the others

#E

fit <- arima(log_data,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))

#F

tsdiag(fit)
#doesn't work with ACF for some reasons
#No specific issues with the ACF or any outliers 

hist(residuals(fit),xlab='Residuals')
#normal distribution around 0 mean 

#G

plot(fit,n1=c(1969,1),n.ahead=24)
#Still an upward trend as it grows over time, it seems to follow the seasonal trends as well 
