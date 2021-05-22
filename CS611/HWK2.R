
#1
library(quantmod)

getSymbols(c("TSLA",'ILMN'), src="yahoo", from = '2015-01-01', to = '2021-01-01')

#2
candleChart(TSLA,multi.col=TRUE,theme="white")
candleChart(ILMN,multi.col=TRUE,theme="white")
a <- merge(TSLA,ILMN)
candleChart(a)

##Answer

#The two time-series seem to be affected by multicollinearity or at least to be
#very similar as ILMN is the reflect of TSLA but lower. 

#3 
library(forecast)
library(dplyr)
ts_TSLA <- ts(TSLA$TSLA.Close, frequency =  7)
ts_ILMN <- ts(ILMN$ILMN.Close, frequency = 7)
#Shows the data quarterly
TSLA_Qtr <- ts(TSLA$TSLA.Close, frequency = 4, start = c(2015,01), end = c(2021,01))
best_model
#ARIMA(1,0,0)
forecast1 <- forecast(best_model, h = 12)
plot(forecast1)
plot(TSLA$TSLA.Close)

###########################TSLA##############################

##2015
TSLA_Q1_2015 <- ts(ts_TSLA, frequency = 7, start = c(2015,01,01), end = c(2015,03,31) )
auto.arima(TSLA_Q1_2015)
TSLA_Q2_2015 <- ts(ts_TSLA, frequency = 7, start = c(2015,04,01), end = c(2015,06,30) )
auto.arima(TSLA_Q2_2015)
TSLA_Q3_2015 <- ts(ts_TSLA, frequency = 7, start = c(2015,07,01), end = c(2015,09,30) )
auto.arima(TSLA_Q3_2015)
TSLA_Q4_2015 <- ts(ts_TSLA, frequency = 7, start = c(2015,10,01), end = c(2015,12,31) )
auto.arima(TSLA_Q4_2015)
##2016
TSLA_Q1_2016 <- ts(ts_TSLA, frequency = 7, start = c(2016,01,01), end = c(2016,03,31) )
auto.arima(TSLA_Q1_2016)
TSLA_Q2_2016 <- ts(ts_TSLA, frequency = 7, start = c(2016,04,01), end = c(2016,06,30) )
auto.arima(TSLA_Q2_2016)
TSLA_Q3_2016 <- ts(ts_TSLA, frequency = 7, start = c(2016,07,01), end = c(2016,09,30) )
auto.arima(TSLA_Q3_2016)
TSLA_Q4_2016 <- ts(ts_TSLA, frequency = 7, start = c(2016,10,01), end = c(2016,12,31) )
auto.arima(TSLA_Q4_2016)
##2017
TSLA_Q1_2017 <- ts(ts_TSLA, frequency = 7, start = c(2017,01,01), end = c(2017,03,31) )
auto.arima(TSLA_Q1_207)
TSLA_Q2_2017 <- ts(ts_TSLA, frequency = 7, start = c(2017,04,01), end = c(2017,06,30) )
auto.arima(TSLA_Q2_2017)
TSLA_Q3_2017 <- ts(ts_TSLA, frequency = 7, start = c(2017,07,01), end = c(2017,09,30) )
auto.arima(TSLA_Q3_2017)
TSLA_Q4_2017 <- ts(ts_TSLA, frequency = 7, start = c(2017,10,01), end = c(2017,12,31) )
auto.arima(TSLA_Q4_2017)
##2018
TSLA_Q1_2018 <- ts(ts_TSLA, frequency = 7, start = c(2018,01,01), end = c(2018,03,31) )
auto.arima(TSLA_Q1_2018)
TSLA_Q2_2018 <- ts(ts_TSLA, frequency = 7, start = c(2018,04,01), end = c(2018,06,30) )
auto.arima(TSLA_Q2_2018)
TSLA_Q3_2018 <- ts(ts_TSLA, frequency = 7, start = c(2018,07,01), end = c(2018,09,30) )
auto.arima(TSLA_Q2_2018)
TSLA_Q4_2018 <- ts(ts_TSLA, frequency = 7, start = c(2018,10,01), end = c(2018,12,31) )
auto.arima(TSLA_Q2_2018)
##2019
TSLA_Q1_2019 <- ts(ts_TSLA, frequency = 7, start = c(2019 ,01,01), end = c(2019 ,03,31) )
auto.arima(TSLA_Q1_2019 )
TSLA_Q2_2016 <- ts(ts_TSLA, frequency = 7, start = c(2019 ,04,01), end = c(2019 ,06,30) )
auto.arima(TSLA_Q2_2019 )
TSLA_Q3_2016 <- ts(ts_TSLA, frequency = 7, start = c(2019 ,07,01), end = c(2019 ,09,30) )
auto.arima(TSLA_Q3_2019 )
TSLA_Q4_2016 <- ts(ts_TSLA, frequency = 7, start = c(2019 ,10,01), end = c(2019 ,12,31) )
auto.arima(TSLA_Q4_2019 )
##2020
TSLA_Q1_2020 <- ts(ts_TSLA, frequency = 7, start = c(2020,01,01), end = c(2020,03,31) )
auto.arima(TSLA_Q1_2020)
TSLA_Q2_2020 <- ts(ts_TSLA, frequency = 7, start = c(2020,04,01), end = c(2020,06,30) )
auto.arima(TSLA_Q2_2020)
TSLA_Q3_2020 <- ts(ts_TSLA, frequency = 7, start = c(2020,07,01), end = c(2020,09,30) )
auto.arima(TSLA_Q3_2020)
TSLA_Q4_2020 <- ts(ts_TSLA, frequency = 7, start = c(2020,10,01), end = c(2020,12,31) )
auto.arima(TSLA_Q4_2020)
##2021
TSLA_Q1_2021 <- ts(ts_TSLA, frequency = 7, start = c(2021,01,01), end = c(2021,03,31) )
auto.arima(TSLA_Q1_2021)
TSLA_Q2_2021 <- ts(ts_TSLA, frequency = 7, start = c(2021,04,01), end = c(2021,06,30) )
auto.arima(TSLA_Q2_2021)
TSLA_Q3_2021 <- ts(ts_TSLA, frequency = 7, start = c(2021,07,01), end = c(2021,09,30) )
auto.arima(TSLA_Q3_2021)
TSLA_Q4_2021 <- ts(ts_TSLA, frequency = 7, start = c(2021,10,01), end = c(2021,12,31) )
auto.arima(TSLA_Q4_2021)


###########################ILMN##############################

##2015
ILMN_Q1_2015 <- ts(ts_ILMN, frequency = 7, start = c(2015,01,01), end = c(2015,03,31) )
auto.arima(ILMN_Q1_2015)
ILMN_Q2_2015 <- ts(ts_ILMN, frequency = 7, start = c(2015,04,01), end = c(2015,06,30) )
auto.arima(ILMN_Q2_2015)
ILMN_Q3_2015 <- ts(ts_ILMN, frequency = 7, start = c(2015,07,01), end = c(2015,09,30) )
auto.arima(ILMN_Q3_2015)
ILMN_Q4_2015 <- ts(ts_ILMN, frequency = 7, start = c(2015,10,01), end = c(2015,12,31) )
auto.arima(ILMN_Q4_2015)
##2016
ILMN_Q1_2016 <- ts(ts_ILMN, frequency = 7, start = c(2016,01,01), end = c(2016,03,31) )
auto.arima(ILMN_Q1_2016)
ILMN_Q2_2016 <- ts(ts_ILMN, frequency = 7, start = c(2016,04,01), end = c(2016,06,30) )
auto.arima(ILMN_Q2_2016)
ILMN_Q3_2016 <- ts(ts_ILMN, frequency = 7, start = c(2016,07,01), end = c(2016,09,30) )
auto.arima(ILMN_Q3_2016)
ILMN_Q4_2016 <- ts(ts_ILMN, frequency = 7, start = c(2016,10,01), end = c(2016,12,31) )
auto.arima(ILMN_Q4_2016)
##2017
ILMN_Q1_2017 <- ts(ts_ILMN, frequency = 7, start = c(2017,01,01), end = c(2017,03,31) )
auto.arima(ILMN_Q1_207)
ILMN_Q2_2017 <- ts(ts_ILMN, frequency = 7, start = c(2017,04,01), end = c(2017,06,30) )
auto.arima(ILMN_Q2_2017)
ILMN_Q3_2017 <- ts(ts_ILMN, frequency = 7, start = c(2017,07,01), end = c(2017,09,30) )
auto.arima(ILMN_Q3_2017)
ILMN_Q4_2017 <- ts(ts_ILMN, frequency = 7, start = c(2017,10,01), end = c(2017,12,31) )
auto.arima(ILMN_Q4_2017)
##2018
ILMN_Q1_2018 <- ts(ts_ILMN, frequency = 7, start = c(2018,01,01), end = c(2018,03,31) )
auto.arima(ILMN_Q1_2018)
ILMN_Q2_2018 <- ts(ts_ILMN, frequency = 7, start = c(2018,04,01), end = c(2018,06,30) )
auto.arima(ILMN_Q2_2018)
ILMN_Q3_2018 <- ts(ts_ILMN, frequency = 7, start = c(2018,07,01), end = c(2018,09,30) )
auto.arima(ILMN_Q2_2018)
ILMN_Q4_2018 <- ts(ts_ILMN, frequency = 7, start = c(2018,10,01), end = c(2018,12,31) )
auto.arima(ILMN_Q2_2018)
##2019
ILMN_Q1_2019 <- ts(ts_ILMN, frequency = 7, start = c(2019 ,01,01), end = c(2019 ,03,31) )
auto.arima(ILMN_Q1_2019 )
ILMN_Q2_2016 <- ts(ts_ILMN, frequency = 7, start = c(2019 ,04,01), end = c(2019 ,06,30) )
auto.arima(ILMN_Q2_2019 )
ILMN_Q3_2016 <- ts(ts_ILMN, frequency = 7, start = c(2019 ,07,01), end = c(2019 ,09,30) )
auto.arima(ILMN_Q3_2019 )
ILMN_Q4_2016 <- ts(ts_ILMN, frequency = 7, start = c(2019 ,10,01), end = c(2019 ,12,31) )
auto.arima(ILMN_Q4_2019 )
##2020
ILMN_Q1_2020 <- ts(ts_ILMN, frequency = 7, start = c(2020,01,01), end = c(2020,03,31) )
auto.arima(ILMN_Q1_2020)
ILMN_Q2_2020 <- ts(ts_ILMN, frequency = 7, start = c(2020,04,01), end = c(2020,06,30) )
auto.arima(ILMN_Q2_2020)
ILMN_Q3_2020 <- ts(ts_ILMN, frequency = 7, start = c(2020,07,01), end = c(2020,09,30) )
auto.arima(ILMN_Q3_2020)
ILMN_Q4_2020 <- ts(ts_ILMN, frequency = 7, start = c(2020,10,01), end = c(2020,12,31) )
auto.arima(ILMN_Q4_2020)
##2021
ILMN_Q1_2021 <- ts(ts_ILMN, frequency = 7, start = c(2021,01,01), end = c(2021,03,31) )
auto.arima(ILMN_Q1_2021)
TSLA_Q2_2021 <- ts(ts_ILMN, frequency = 7, start = c(2021,04,01), end = c(2021,06,30) )
auto.arima(ILMN_Q2_2021)
ILMN_Q3_2021 <- ts(ts_ILMN, frequency = 7, start = c(2021,07,01), end = c(2021,09,30) )
auto.arima(ILMN_Q3_2021)
ILMN_Q4_2021 <- ts(ts_ILMN, frequency = 7, start = c(2021,10,01), end = c(2021,12,31) )
auto.arima(ILMN_Q4_2021)

##Answer
#The way I divided it into quarters does not seem to fit correctly and prevents me from getting
#results since all of them have the same value and I don't know why.
#However, if we only use auto.arima, we obtain a better model with better values.
#Lastly, frequency dictates the seasonality and the computing power needed to find the best models.
#Therefore, high frequency such as daily may take a tremendous time to compute but will end up 
#more accurate. It is also important to note that we can have more that one frequency as well.

auto.arima(TSLA$TSLA.Close)
auto.arima(ILMN$ILMN.Close)

#4
best_model_TSLA <- auto.arima(ts_TSLA)
forecast_TSLA <- forecast(best_model, h = 12)
plot(forecast_TSLA)

best_model_ILMN <- auto.arima(ts_ILMN)
forecast_ILMN <- forecast(best_model, h = 12)
plot(forecast_ILMN)


