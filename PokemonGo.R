library(lubridate)
library(ggplot2)
library(forecast)

pokemongo <- read.csv("pokemongo.csv")

# Fix dates
pokemongo$Date <- mdy(pokemongo$Day)

ggplot(pokemongo, aes(Date, Trend)) + geom_line() +
  scale_x_date(format = "%d") + xlab("") + ylab("Google Trend")

ggplot(pokemongo, aes(Date, Trend)) + 
  geom_line() + 
  xlab("") + 
  ylab("Google Trend")

# Time Series Transformation
poke.ts <- ts(pokemongo$Trend, frequency = 7)
poke.diff <- diff(poke.ts)

a <- as.numeric(poke.diff)
a <- append(a, 0, 90)
pokemongo$Trend.diff <- a

# Pretty graph
ggplot(pokemongo, aes(Date)) + 
  geom_line(aes(y = Trend, colour = "var0")) + 
  geom_line(aes(y = Trend.diff, colour = "var1")) +
  xlab("") +
  ylab("Google Trend")+
  theme(legend.position = "none")

# ARIMA
acf(poke.diff, lag.max=20)  
pacf(poke.diff, lag.max=20)

poke.arima <- arima(poke.ts, order=c(0,1,0))

poke.forecast <- forecast.Arima(poke.arima, h=7)

plot.forecast(poke.forecast)
  
# Neural Network
poke.nnet <- nnetar(poke.ts)

poke.nnet.forecast <- forecast(poke.nnet, 7)
plot(poke.nnet.forecast)

# Bring it all home
net.pred <- as.numeric(poke.nnet.forecast$mean)
arima.pred.high <- as.numeric(poke.forecast$upper[,1])
arima.pred.low <- as.numeric(poke.forecast$lower[,1])

next7 <- seq(as.Date("2016-09-01"), as.Date("2016-09-07"), by="days")
a <- cbind(next7, net.pred, arima.pred.high, arima.pred.low)
poke.future <- as.data.frame(a)
poke.future$next7 <- as.Date(poke.future$next7)

ggplot(poke.future, aes(next7)) + 
  geom_line(aes(y = arima.pred.high, colour = "var0")) + 
  geom_line(aes(y = arima.pred.low, colour = "var1")) +
  geom_line(aes(y = net.pred, colour = "var2")) +
  geom_point(aes(y = arima.pred.high, colour = "var0")) + 
  geom_point(aes(y = arima.pred.low, colour = "var1")) +
  geom_point(aes(y = net.pred, colour = "var2")) +
  xlab("") +
  ylab("Google Trend")+
  theme(legend.position = "none")

