#install.packages("TSA")
data(rwalk)
rwalk

#Moving Averages (MA)
library('TSA')
data(ma1.2.s)
plot(ma1.2.s)
plot(x=zlag(ma1.2.s),y=ma1.2.s)#n-1
plot(x=zlag(ma1.2.s,2),y=ma1.2.s)#n-2
