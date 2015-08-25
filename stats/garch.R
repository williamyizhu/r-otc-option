# TODO: Add comment
# 
# Author: williamyizhu
#################################################################################################
rm(list=ls(all=TRUE)) 
options(width = 438L)
library("fGarch")
library("tseries")
library("rugarch")
library("bayesGARCH")

#################################### underlying contract and period ####################################
#exchange = "DCE"
#underlying = "m1409"
#delivery_month = "05"
##underlying = "m1501"
##underlying = "m1505"
##underlying = "c1501"
##underlying = "rb1410"

##----------------------------------------- if using pre-downloaded data -----------------------------------------
##read csv data
#fpath = paste(getwd(), "/", underlying, ".csv", sep="")
#data = read.csv(fpath, row.names=1)
#
##change the date to "Date" format
#data$date = as.Date(data$date)

#################################### create data set under Wind (www.wind.com.cn) ####################################
exchange = "DCE"
underlying = "m"
#[year + delivery month]
contract_year_month = "1501"
delivery_month = substr(contract_year_month, 3, 4)
#most recent year expressed as [20XX], and the number of years of historical data
year_end = as.numeric(paste("20", substr(contract_year_month, 1, 2), sep=""))
number_of_years = 10

#variables inside ["C:/Users/William Yizhu/Documents/workspace/r-option/get_dataset.R"] are all local to this file
source(paste(getwd(), "/get_dataset.R", sep=""), echo=FALSE, encoding="GBK")
print("---------------------------------------------------------------------------")
print("head(data)")
print("---------------------------------------------------------------------------")
print(head(data))
print("---------------------------------------------------------------------------")
print("tail(data)")
print("---------------------------------------------------------------------------")
print(tail(data))

#################################### prepare dataset for garch analysis ####################################
#calculate log return and create a data.frame
return_date = data[-1,]$date
settlement = data[-1,]$settlement

#transform return_date to find delivery month
aa = strsplit(as.character(return_date), "-")
bb = as.matrix(aa)
cc = t(apply(bb, 1, unlist))
return_date.df = data.frame(cc)
colnames(return_date.df) = c("year", "month", "day")

#log_return on "2014-08-01" is log return of "2014-08-01" over "2014-07-31"
log_return = diff(log(data$settlement), lag=1)
#log(a/b) = log(a) - log(b) = log_return; return = (a/b) - 1 = exp(log_return) - 1
daily_return = exp(log_return) - 1
#create a dataframe with both daily return and logged daily return
return.df = data.frame(return_date, settlement, return_date.df, daily_return, log_return)

#only include non-delivery month data for garch analysis
is_delivery_month = (return.df$month != delivery_month)
return.df = cbind(return.df, is_delivery_month)
garch.df = return.df[return.df$is_delivery_month,]

#----------------------------------------- diagnostics analysis -----------------------------------------
par(mfcol=c(2,2))
#check for outliers and normality assumption
plot(garch.df$return_date, garch.df$log_return, xlab="return date", ylab="log return")

#qqnorm is a generic function the default method of which produces a normal QQ plot of the values in y. 
#qqline adds a line to a ¡°theoretical¡±, by default normal, quantile-quantile plot which passes through the probs quantiles, by default the first and third quartiles. 
#qqplot produces a QQ plot of two datasets. 
qqnorm(garch.df$log_return)
qqline(garch.df$log_return)

#hist and density plot
dd = density(garch.df$log_return)
hist(garch.df$log_return, prob=TRUE, ylim=c(min(dd$y), max(dd$y)), xlab="log return")
lines(dd$x, dd$y, col="red", lwd=1)
#normal density plot
xfit = seq(min(garch.df$log_return), max(garch.df$log_return), length=1000) 
yfit = dnorm(xfit, mean=mean(garch.df$log_return), sd=sd(garch.df$log_return)) 
lines(xfit, yfit, col="blue", lwd=1)

#Shapiro-Wilk test for Normality
#This p-value tells you what the chances are that the sample comes from a normal distribution. 
#The lower this value, the smaller the chance. Statisticians typically use a value of 0.05 as a cutoff, 
#so when the p-value is lower than 0.05, you can conclude that the sample deviates from normality.
#http://en.wikipedia.org/wiki/Normality_test
shapiro.test(garch.df$log_return)

summary(garch.df$log_return)
garch.df[which(garch.df$log_return==max(garch.df$log_return)),]
garch.df[which(garch.df$log_return==min(garch.df$log_return)),]
#garch.df = garch.df[-535,]

################################################## garch analysis ##################################################
#----------------------------------------- fGarch package -----------------------------------------
#commodity price may not have a normal distribution, cauchy distribution is more likely
fit.fgarch = garchFit(~garch(1,1), data=garch.df$log_return, trace=FALSE)
coef(fit.fgarch)
#diagnostic plots
par(mfcol=c(4,4))
for (i in 1:13) {
	plot(fit.fgarch, which=i)
}
#plot in-sample volatility estimates
plot(sqrt(252)*fit.fgarch@sigma.t, type="l")

#generate normal distribtuion random numbers with mean and sd equal to "garch.df$log_return"
ref = rnorm(length(garch.df$log_return), mean=mean(garch.df$log_return), sd=sd(garch.df$log_return))
ref.garch = garchFit(~garch(1,1), data=ref, trace=TRUE)
plot(sqrt(252)*ref.garch@sigma.t, type="l")
#diagnostic plots
par(mfcol=c(4,4))
for (i in 1:13) {
	plot(ref.garch, which=i)
}
shapiro.test(ref)

#simulated garch(1,1) data, which has a normal distribution
ss = garchSim(n=1000)
ss.fgarch = garchFit(~garch(1,1), data=ss, trace=FALSE)
#diagnostic plots
par(mfcol=c(4,4))
for (i in 1:13) {
	plot(ss.fgarch, which=i)
}
shapiro.test(ss)

#----------------------------------------- rugarch package -----------------------------------------
spec.rugarch = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="std")
fit.rugarch <- ugarchfit(spec.rugarch, garch.df$log_return)
coef(fit.rugarch)

#plot in-sample volatility estimates
plot(sqrt(252)*fit.rugarch@fit$sigma, type='l')


#----------------------------------------- tseries package -----------------------------------------
#problem with "FALSE CONVERGENCE"
fit.ts = garch(garch.df$log_return)
coef(fit.ts)
# plot in-sample volatility estimates
plot(sqrt(252)*fit.ts$fitted.values[,1], type="l")

#data generated from "fGarch" package, still generates a "FALSE CONVERGENCE" warning
mm = garchSim(n=1000)
garch(mm)

#----------------------------------------- bayesGarch package -----------------------------------------
#Error in uniroot(fn.neg.alpha, interval = c(alpha.min, alpha.max), tol = .Machine$double.eps,  : 
#  f() values at end points not of opposite sign
fit.bayesGARCH = bayesGARCH(garch.df$log_return, control = list(n.chain = 2, l.chain = 500))

## LOAD DATA
data(dem2gbp)
y <- dem2gbp[1:750]

## RUN THE SAMPLER (2 chains)
## NB: CONSIDER LARGER l.chain!
MCMC <- bayesGARCH(y, control=list(n.chain=2, l.chain=500))

## MCMC ANALYSIS (using coda)
plot(MCMC)
autocorr.diag(MCMC)
gelman.diag(MCMC)
1-rejectionRate(MCMC)

## FORM THE POSTERIOR SAMPLE
smpl <- formSmpl(MCMC, l.bi=100)

## POSTERIOR STATISTICS
summary(smpl)
smpl <- as.matrix(smpl)
pairs(smpl)

shapiro.test(y)

#----------------------------------------- summary plots for all garch packages -----------------------------------------
plot(sqrt(252)*fit.fgarch@sigma.t, col="red", type="l")
lines(sqrt(252)*fit.rugarch@fit$sigma, col="blue", type='l')
legend("topleft", legend=c("fgarch","rugarch"), text.col=c("red","blue"), pch=c(15,15), col=c("red","blue"))

#http://unstarched.net/r-examples/rugarch/a-short-introduction-to-the-rugarch-package/
#################################################################################################
require(rugarch)
data(sp500ret)
plot.ts(sp500ret)

# create a cluster object to be used as part of this demonstration
cluster = makePSOCKcluster(15)

spec = ugarchspec()
show(spec)

nrow(expand.grid(GARCH = 1:14, VEX = 0:1, VT = 0:1, Mean = 0:1, ARCHM = 0:2, ARFIMA = 0:1, MEX = 0:1, DISTR = 1:10))

spec = ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(2, 1)), distribution = 'std')
setstart(spec) < - list(shape = 5)
setbounds(spec)

fit = ugarchfit(spec, sp500ret[1:1000, , drop = FALSE], solver = 'hybrid')


#Filtering new data: ugarchfilter
spec = getspec(fit)
setfixed(spec) < - as.list(coef(fit))
filt1 = ugarchfilter(spec, sp500ret[1:1200, ], n.old = 1000)
filt2 = ugarchfilter(spec, sp500ret[1001:1200, ])

#http://www.r-bloggers.com/a-practical-introduction-to-garch-modeling/
#################################################################################################


gfit.fg <- garchFit(data$settlement, cond.dist="std")
coef(gfit.fg)

# plot in-sample volatility estimates
plot(sqrt(252) * gfit.fg@sigma.t, type="l")



#calculate log return and realized volatility for the current period
log_return = diff(log(data$settlement), lag=1)
daily_return = diff(data$settlement, lag=1)
realized_volatility = sqrt(252 * mean(log_return^2))

gfit.ts <- garch(log_return)
gfit.ts <- garch(daily_return)
coef(gfit.ts)

# plot in-sample volatility estimates
plot(sqrt(252) * gfit.ts$fitted.values[, 1], type="l")


data(EuStockMarkets)  
dax <- diff(log(EuStockMarkets))[,"DAX"]
dax.garch <- garch(dax)  # Fit a GARCH(1,1) to DAX returns
summary(dax.garch)       # ARCH effects are filtered. However, 
plot(dax.garch)          # conditional normality seems to be violated



N = 200
x.vec = as.vector(garchSim(garchSpec(rseed = 1985), n = N)[,1])
garchFit(~ garch(1,1), data = x.vec, trace = FALSE)






w_wsd_data = w.wsd("510300.OF","close","2010-07-10","2014-08-10")
w_wsd_data = w.wsd("SHIBORON.IR", "close", "2013-07-10", "2014-08-10")




library("MASS") 
fitdistr(garch.df$log_return, "cauchy") 

mm = fitdistr(garch.df$log_return, "normal") 


fitdistr(garch.df$log_return, "gamma")



