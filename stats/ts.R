# TODO: Add comment
# Author: williamyizhu
###############################################################################
rm(list=ls(all=TRUE)) 
options(width = 438L)

# http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
#read data from internet source
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)

#transform the data into time series data, note the use of "frequency" argument
kingstimeseries <- ts(kings)
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
rainseries <- ts(rain,start=c(1813))

#plot the time series data
plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)
plot.ts(rainseries)

#log-transformed time series data
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

#Decomposing Non-Seasonal Data
#A non-seasonal time series consists of a trend component and an irregular component. 
#Decomposing the time series involves trying to separate the time series into these components, 
#that is, estimating the the trend component and the irregular component.
#To estimate the trend component of a non-seasonal time series that can be described using an additive model, 
#it is common to use a smoothing method, such as calculating the simple moving average of the time series.
#simple moving average function
library("TTR")
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

#Decomposing Seasonal Data
#A seasonal time series consists of a trend component, a seasonal component and an irregular component. 
#Decomposing the time series means separating the time series into these three components: that is, estimating these three components.
birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

#Seasonally Adjusting
#The seasonally adjusted time series now just contains the trend component and an irregular component.
#If you have a seasonal time series that can be described using an additive model, 
#you can seasonally adjust the time series by estimating the seasonal component, 
#and subtracting the estimated seasonal component from the original time series.
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

#Simple Exponential Smoothing
#If you have a time series that can be described using an additive model with constant level and no seasonality, 
#you can use simple exponential smoothing to make short-term forecasts.
#The simple exponential smoothing method provides a way of estimating the level at the current time point. 
#Smoothing is controlled by the parameter alpha; for the estimate of the level at the current time point. The value of alpha; lies between 0 and 1. 
#Values of alpha that are close to 0 mean that little weight is placed on the most recent observations when making forecasts of future values.
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts

#The output of HoltWinters() tells us that the estimated value of the alpha parameter is about 0.024. 
#This is very close to zero, telling us that the forecasts are based on both recent and less recent observations 
#(although somewhat more weight is placed on recent observations).
#The forecasts made by HoltWinters() are stored in a named element of this list variable called “fitted”.
summary(rainseriesforecasts)
plot(rainseriesforecasts)

#When using the forecast.HoltWinters() function, as its first argument (input), 
#you pass it the predictive model that you have already fitted using the HoltWinters() function. 
#For example, in the case of the rainfall time series, we stored the predictive model made using HoltWinters() in the variable “rainseriesforecasts”. 
#You specify how many further time points you want to make forecasts for by using the “h” parameter in forecast.HoltWinters(). 
library("forecast")
rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)
plot.forecast(rainseriesforecasts2)

#The in-sample forecast errors are stored in the named element “residuals” of the list variable returned by forecast.HoltWinters(). 
#If the predictive model cannot be improved upon, there should be no correlations between forecast errors for successive predictions. 
#In other words, if there are correlations between forecast errors for successive predictions, 
#it is likely that the simple exponential smoothing forecasts could be improved upon by another forecasting technique.
#To figure out whether this is the case, we can obtain a correlogram of the in-sample forecast errors for lags 1-20. 
#We can calculate a correlogram of the forecast errors using the “acf()” function in R. 
#To specify the maximum lag that we want to look at, we use the “lag.max” parameter in acf().
plot(rainseriesforecasts2$residuals)
acf(rainseriesforecasts2$residuals, lag.max=20)

#You can see from the sample correlogram that the autocorrelation at lag 3 is just touching the significance bounds. 
#To test whether there is significant evidence for non-zero correlations at lags 1-20, we can carry out a Ljung-Box test. 
#This can be done in R using the “Box.test()”, function. 
#The maximum lag that we want to look at is specified using the “lag” parameter in the Box.test() function. 
#Here the Ljung-Box test statistic is 17.4, and the p-value is 0.6, 
#so there is little evidence of non-zero autocorrelations in the in-sample forecast errors at lags 1-20.
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")

# http://www.stat.pitt.edu/stoffer/tsa3/R_toot.htm
###############################################################################



# http://fisher.utstat.toronto.edu/~hadas/STA457/Lecture%20notes/R_armasimulation.pdf
###############################################################################
#1. Simulate 100 observations from an AR(2) Process 
ar.sim <- arima.sim(model=list(ar=c(.9,-.2)), n=100) 
#Make a time series plot of the data
ts.plot(ar.sim) 
#Calculate the Sample Autocorrelation Function
ar.acf <- acf(ar.sim,type="correlation", plot=TRUE) 
ar.acf 
#Calculate the Sample Partial Autocorrelation Function
ar.pacf<-acf(ar.sim, type="partial", plot=TRUE) 

#2. Simulate 100 observations from an MA(2) Process 
ma.sim <- arima.sim(model=list(ma=c(-.7,.1)), n=100) 
#Make a time series plot of the data
ts.plot(ma.sim) 
#Calculate the Sample Autocorrelation Function
ma.acf <- acf(ma.sim, type="correlation", plot=TRUE) 
#Calculate the Sample Partial Autocorrelation Function
ma.pacf<-acf(ma.sim, type="partial", plot=TRUE)

#3. Simulate 100 observations from an ARMA(2,2) Process 
arma.sim <- arima.sim(model=list(ar=c(.9,-.2), ma=c(-.7,.1)), n=100) 
#Make a time series plot of the data 
ts.plot(arma.sim) 
#Calculate the Sample Autocorrelation Function 
arma.acf <- acf(arma.sim, type="correlation", plot=TRUE) 
#Calculate the Sample Partial Autocorrelation Function 
arma.pacf <- acf(arma.sim, type="partial", plot=TRUE) 
	
# http://www.statoek.wiso.uni-goettingen.de/veranstaltungen/zeitreihen/sommer03/ts_r_intro.pdf
###############################################################################
#The dataset can be downloaded from http://134.76.173.220/tui.zip as a zipped Excel–file)
tui <- read.csv(paste(getwd(), "/stats/tui.csv", sep=""), header=TRUE, dec=",", sep=";")

plot(tui$close, type="l", lwd=2, col="red", xlab="time", ylab="closing values", main="TUI AG")

#Assume that we want to plot differences of the logarithms of the returns. In order to do so, 
#we need two operators, log( ) which takes logarithms and diff( ) which computes differences of a given object:
plot(diff(log(tui$close)), type="l")

#draw a histogram and to compare it with e.g. the density of the normal distribution
hist(diff(tui[,4]), prob=T, ylim=c(0,0.6), xlim=c(-5,5), col="red")
lines(density(diff(tui[,4])), lwd=2)
#create a normal distribution with tui[,4] properties
mu <- mean(diff(tui[,4]))
sigma <- sd(diff(tui[,4]))
x <- seq(-4,4, length=100)
y <- dnorm(x, mu, sigma)
lines(x, y, lwd=2, col="blue")

#normality test
#There are various means for testing normality of given data. 
qqnorm(diff(tui[,4])) 
abline(0, 1)


x <- diff(log(tui[,5]))
ks.test(x, "pnorm", mean(x), sd(x))

shapiro.test(x)

#Linear Filtering of Time Series, plot original data and moving average, which can be seen as a filter
plot(tui[,5], type="l")
tui.1 <- filter(tui[,5], filter=rep(1/5,5))
tui.2 <- filter(tui[,5], filter=rep(1/25,25))
tui.3 <- filter(tui[,5], filter=rep(1/81,81))
lines(tui.1, col="red")
lines(tui.2, col="purple")
lines(tui.3, col="blue")

#Decomposition of Time Series
beer <- read.csv(paste(getwd(), "/stats/beer.csv", sep=""), header=T, dec=",", sep=";")
beer <- ts(beer[,1], start=1956, freq=12)

plot(stl(log(beer), s.window="periodic"))


#Chapter 3 Exponential Smoothing
beer <- read.csv(paste(getwd(), "/stats/beer.csv", sep=""), header=T, dec="," ,sep=";")
beer <- ts(beer[,1], start=1956, freq=12)

beer.hw <- HoltWinters(beer)
predict(beer.hw,n.ahead=12)
plot(beer, xlim=c(1956,1999))
lines(predict(beer.hw, n.ahead=48), col=2)

plot(beer)
plot(beer.hw$fitted, col="red")

#Chapter 4 ARIMA–Models
#The function arima.sim( ) was used to simulate ARIMA(p,d,q)–models ; in
#the first line 1000 observations of an ARIMA(2,0,0)–model (i.e. AR(2)–model)
#were simulated and saved as sim.ar. Equivalently, the second line simulated
#1000 observations from a MA(2)–model and saved them to sim.ma.
sim.ar <- arima.sim(list(ar=c(0.4,0.4)), n=1000)
sim.ma <- arima.sim(list(ma=c(0.6,-0.4)), n=1000)
par(mfrow=c(2,2))
acf(sim.ar, main="ACF of AR(2) process")
acf(sim.ma, main="ACF of MA(2) process")
pacf(sim.ar, main="PACF of AR(2) process")
pacf(sim.ma, main="PACF of MA(2) process")

data(LakeHuron)
fit<-arima(LakeHuron,order=c(1,0,1))
#R has the function tsdiag( ), which produces a diagnostic plot of a fitted time series model:
tsdiag(fit)

Box.test(fit$residuals, lag=1)

#Prediction of ARIMA–Models
fit <- arima(LakeHuron, order=c(1,0,1))
LH.pred <- predict(fit, n.ahead=8)
plot(LakeHuron, xlim=c(1875,1980), ylim=c(575,584))
LH.pred <- predict(fit,n.ahead=8)
lines(LH.pred$pred, col="red")
lines(LH.pred$pred+2*LH.pred$se, col="red", lty=3)
lines(LH.pred$pred-2*LH.pred$se, col="red", lty=3)

#auto.arima
library(forecast)
auto.arima(LakeHuron)










