library(TSA)

##########################################6.21#############################################

set.seed(512)
y.sim <- arima.sim(n=60,list(order = c(1,3,1), ar=0.6,ma=-0.7), rand.gen = mu)



# 1
# -0.5/ 1+ 0.25 = -0.25
# 
# 2
#
acf(y.sim, lag.max = 1)[1]



#3
#Using different samples, we still get the same patterns for the lags > 1

#4
r1=rep(NA,10000) 
for (k in 1:10000) {series=arima.sim(n=48, list(ar=0.7))
  r1[k]=acf(series,lag.max=1,plot=F)$acf[1]}
hist(r1); mean(r1); sd(r1); median(r1)



##########################################6.22#############################################

#A

#lag1 = 0.9 and lag5 = (0.7)^5 = 0.16807
y.sim <- arima.sim(n=48,list(ar = c(0.9)))
acf(y.sim)[1:5]

#B

#lag1 = 0.6 and lag5 = (0.7)^5 = 0.07776
y.sim <- arima.sim(n=48,list(ar = c(0.6)))
acf(y.sim)[1:5]

#C

#lag1 = 0.3 and lag5 = (0.7)^5 = 0.00243
y.sim <- arima.sim(n=48,list(ar = c(0.3)))
acf(y.sim)[1:5]

#D

#A: r1 = .06, r5 = .4
#B: r1 = .12, r5 = .21
#C: r1 = .14, r5 = .16

#All of these values, when added to the lags, show that they are pretty close to the 
#actual results. We can see that small thetas with small lags have the best precision.

##########################################6.30#############################################

#a
y.sim=arima.sim(n=100,list(ar=0.8,ma=-0.4))

plot(y=ARMAacf(ar=c(0.8),ma=c(-.4),lag.max=20)[-1],x=1:20,xlab='Lag',ylab='ACF',type='h',ylim=c(-.2,.6)); abline(h=0)

#b

acf(y.sim)

#We observe the same pattern and seem to match pretty well as we see a decrease over time as the lags
#increase 

#c

eacf(y.sim)

#As we can observe, the first circle is on 1,1, which means that the best model for this time-series
# is an ARMA(1,1)


#d
#a

y.sim=arima.sim(n=100,list(ar=0.8,ma=-0.4))
acf(y.sim)
eacf(y.sim)
#This time,we observe that an AR(1) might be better

#b

y.sim=arima.sim(n=100,list(n=48, ar=0.8,ma=-0.4))
acf(y.sim)
eacf(y.sim)
#An ARMA(1,1) seems to fit the model the best as well

#c 

y.sim=arima.sim(n=100,list(n=200, ar=0.8,ma=-0.4))
acf(y.sim)
eacf(y.sim)

#An ARMA(1,1) is the best answer, however, as we increase the sample size, the results seems to vary
# more often between the best models to use.


##########################################6.37#############################################
data(larain)
eacf(log(larain))

#the EACF does not seem to show much of a  model to adopt which could mean that the data
# is not showing a clear pattern to follow. In other words, this could mean it is more 
# representative of randomness. 