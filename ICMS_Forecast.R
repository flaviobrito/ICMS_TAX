#put the data into a time series
library(forecast)
icms <- read.csv("C:/Users/flavi/OneDrive/Source/Repos/aval_tech/data/ICMS.csv", sep=";")
attach(icms)
serie = ts(Indice, frequency=12, start=c(2005,1), end=c(2016,6))

#subset the time series from 2014 forward using window commd
serie2  <-  window(serie, start=c(2014,1), end=c(2016, 6))

# fit the stl model using only the s.window argument
fit <-  stl(serie2,s.window = "periodic")
plot(serie2, col="gray",
     main="Arrecadação do ICMS",
     ylab="Valor", xlab="Tempo", type ="l")

lines(fit$time.series[,2],col="red",ylab="Trend")
plot(fit)

#plota a Sazonalidade
monthplot(fit$time.series[,"seasonal"], main="", ylab="Seasonal")

plot(serie2, col="grey",
     main="Arrecadação do ICMS",
     ylab="Valor", xlab="Tempo",type ="l")
lines(seasadj(fit),col="red",ylab="Seasonally adjusted")

#Médias Móveis
ma(serie2, order=5)

plot(serie2, col="grey",
     main="Arrecadação do ICMS - MA(5)",
     ylab="Valor", xlab="Tempo",type ="l")
lines(ma(serie2,5),col="red")

serie3 <- window(serie2,start=2014)
plot(serie3)

ma4 <- ma(serie3, order=4, centre=FALSE)
plot(ma4)
plot(serie3, col="grey",
     main="Arrecadação do ICMS - MA(4)",
     ylab="Valor", xlab="Tempo",type ="l")
lines(ma(serie3,4),col="red")

ma2x4 <- ma(serie3, order=4, centre=TRUE)
plot(ma2x4)
lines(ma(serie3, order=6), col="red")

# serie2 is the time series
fit <- decompose(serie2, type="multiplicative")
plot(fit)

#“Seasonal and Trend decomposition using Loess”
# fit the stl model using only the s.window argument
fit <-  stl(serie2,s.window = "periodic")

fit <- stl(serie2, t.window=12, s.window="periodic", robust=TRUE)
plot(fit)

#Forecasting with decomposition
#shows naïve forecasts of the seasonally adjusted
fit <- stl(serie2, t.window=15, s.window="periodic", robust=TRUE)
eeadj <- seasadj(fit)
plot(naive(eeadj), xlab="Tempo",
     main="Naive forecasts of seasonally adjusted data")

#shows random walk forecasts of the seasonally adjusted
fcast <- forecast(fit, method="naive")
plot(fcast, ylab="Valor")

#Auto Arrima
fitdt<-auto.arima(serie2)
summary(fitdt)
plot(forecast(fitdt,h=6))
fit7=forecast(fitdt,h=6)
list(fit7)
Box.test(fitdt$residuals,type = "Ljung-Box")

library(astsa)
library(ggplot2)

diff3 = diff(serie2,3)
acf2(diff3, 6)

sarima(serie2, 1,0,0,0,1,1,12)
sarima(serie2, 0,0,0,0,1,0,12)

#Forecast
sarima.for(serie2, 6, 1,0,0,0,1,1,12)
sarima.for(serie2, 6, 0,0,0,0,1,1,12)

themodel = arima(serie2, order = c(1,0,0), seasonal = list(order = c(0,1,1), period = 6))
themodel
predict(themodel, n.ahead=6)

flowm = matrix(serie2, ncol=12,byrow=TRUE)
col.means=apply(flowm,2,mean)
plot(col.means,type="b", main="Médias Mensais", xlab="Mês", ylab="Média")

png(file='eqexp.png', width=600 )
df_plot <- serie2
autoplot(ts(df_plot), ncol=2, xlab='Tempo', colour='variable') + 
  guides(colour=FALSE)          #  shuts off legend

plot(serie2, ylab=expression(M[~t]~~~~(Number~Buried)), xaxt="no", type='n')
grid(lty=1, col=gray(.9))
lines(serie2, col=rgb(0,0,.9))
text(2016, 2546.80, 'Bad Year', col=rgb(.5,0,.5), cex=1.25)   # just for fun
#arrows(1973.5, 130, 1973, 127, length=0.05,  angle=30, col=rgb(.5,0,.5))   
arrows(2016, 2546.80, 2016, 2481.19, length=0.05,  angle=30, col=rgb(.5,0,.5)) 

autoplot(serie2, ylab='Temperature Deviations', xlab='Time', colour=4) +  # this is enough for a quick plot
  geom_point(aes(x=time(gtemp), y=gtemp), color=4)             +  # all this just to add points
  theme_bw()      


#Backcasting 

x <- serie2
h <- 90
f <- frequency(x)
# Reverse time
revx <- ts(rev(x), frequency=f)
# Forecast
fc <- forecast(auto.arima(revx), h)
plot(fc)
# Reverse time again
fc$mean <- ts(rev(fc$mean),end=tsp(x)[1] - 1/f, frequency=f)
fc$upper <- fc$upper[h:1,]
fc$lower <- fc$lower[h:1,]
fc$x <- x
# Plot result
plot(fc, xlim=c(tsp(x)[1]-h/f, tsp(x)[2]))


#HoltWinters()
hw <- HoltWinters(serie2, beta=FALSE, gamma=FALSE)
hw

hw$fitted
plot(hw)

# sum-of-squared-errors 
hw$SSE

hw2 <- HoltWinters(serie2, beta=FALSE, gamma=FALSE, l.start=2663.55)

fc <- forecast.HoltWinters(hw, h=8)
fc$fitted
plot.forecast(fc)

m <- HoltWinters(serie)
p <- predict(m, 6, prediction.interval = TRUE, level = 0.95)

plot(m, p)

plot(icms$Indice,
     ylab = "Valor",
     xlab = "Ano",
     type = "l")

# Automated forecasting using an exponential model
#Como a série não é estacionária temos que tirar a sazonalidade (diferença sazonal)
tsdisplay(
  diff(
    icms$Indice,
    6))

fit <- ets(Indice)
    
plot(fit$fitted)
    
# Automated forecasting using an ARIMA model
fit <- auto.arima(Indice)
    
 #Como a série não é estacionária temos que tirar a sazonalidade (diferença sazonal)
    
 tsdisplay(diff(fit,6))
    
plot(Indice, type = 'l' )

