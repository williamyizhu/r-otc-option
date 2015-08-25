# TODO: Add comment

# forecast future realized volatility, how to model volatility, i.e., volatility used to calculate delta, therefore, over/under hedging
# position delta <- realized volatility, which has nothing to do with in the moneyness 
# realized_volatility_data = tail(data, n=60), need to find a better way to decide
# change hedging volatility during the period

# Author: William Yizhu
###############################################################################
rm(list=ls(all=TRUE)) 
options(width = 438L)
#library("RQuantLib")
library("tseries")
library("rugarch")
#################################### underlying contract and period ####################################
underlying = "m1409"
#underlying = "m1501"
#underlying = "m1505"
#underlying = "c1501"
#underlying = "rb1410"

#read csv data
fpath = paste(getwd(), "/", underlying, ".csv", sep="")
data = read.csv(fpath, row.names=1)

contract_date = as.Date(c("2013-02-01"))
settlement_date = as.Date(c("2013-08-31"))	

#-------------------------------------------------------------------------------
#select the data between valuation period
#use the settlement price of contract day as the price for pricing the otc product
#use the settlement price of settlement day as the price for settlement of the otc product
data$date = as.Date(data$date)
#ideally, the agreement is signed on Friday and Friday's settlement price for pricing
#"contract_date" must be contain in the "data", the settlement price on "contract_date" is used for pricing
for (i in 1:30) {
	if (any(data$date == contract_date)) {
		break
	} else {
		contract_date = contract_date - 1
	}
}

valuation_period = (data$date >= contract_date) & (data$date <= settlement_date)
#first trading day after settlement day, close all hedging position on this day, e.g., trade on open price
index = which(valuation_period == TRUE)
if (tail(index, n=1) == dim(data)[1]) {
#	if the last date in the "data" is on the same day as or earlier than the settlement_date, then hedging is still in process
#	e.g., if settlement_date is 2014-07-04, and same as last date in the "data", then the position will be cleared on 2014-07-05
#	i.e., we need to wait until the end of 2014-07-05, to know the final hedging result, final_date = 2014-07-05
	print(paste("contract date = ", contract_date, "|", "settlement date = ", settlement_date, "|", "position has NOT expired yet, hedging in process"))
	is_position_expired = FALSE
} else {
	print(paste("contract date = ", contract_date, "|", "settlement date = ", settlement_date, "|", "position has expired"))
	is_position_expired = TRUE
#	final_date is not the same as settlement_date
	valuation_period[index[length(index)] + 1] = TRUE
}

#save all valuation data into a new dataset
valuation_data = data[valuation_period, ]

#calculate log return and realized volatility for the current period
log_return = diff(log(valuation_data$settlement), lag=1)
realized_volatility = sqrt(252 * mean(log_return^2))






gfit.fg <- garchFit(data=log_return, cond.dist="std")
coef(gfit.fg)
plot(sqrt(252) * gfit.fg@sigma.t, type="l")



m = predict(gfit.fg, n.ahead=20, plot=TRUE, conf=0.95, nx=100)



gspec.ru <- ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="std")


gfit.rugarch <- ugarchfit(gspec.ru, log_return)
summary(gfit.rugarch)
coef(gfit.rugarch)

plot(sqrt(252) * gfit.rugarch@fit$sigma, type='l')



model <- garch(log_return, order=c(1,1)) 
summary(model)

plot(sqrt(252) * model$fitted.values[, 1], type="l")

plot(model)


#
#
n <- 1100
a <- c(0.1, 0.5, 0.2)  # ARCH(2) coefficients
e <- rnorm(n)  
x <- double(n)
x[1:2] <- rnorm(2, sd = sqrt(a[1]/(1.0-a[2]-a[3]))) 
for(i in 3:n)  # Generate ARCH(2) process
{
	x[i] <- e[i]*sqrt(a[1]+a[2]*x[i-1]^2+a[3]*x[i-2]^2)
}
x <- ts(x[101:1100])
x.arch <- garch(x, order = c(0,2))  # Fit ARCH(2) 
summary(x.arch)                     # Diagnostic tests
plot(x.arch)                        
#

data(EuStockMarkets)  
dax <- diff(log(EuStockMarkets))[,"DAX"]
dax.garch <- garch(dax)  # Fit a GARCH(1,1) to DAX returns
summary(dax.garch)       # ARCH effects are filtered. However, 
#plot(dax.garch)          # conditional normality seems to be violated

coef(dax.garch)

# plot in-sample volatility estimates
plot(sqrt(252) * dax.garch$fitted.values[, 1], type="l")
