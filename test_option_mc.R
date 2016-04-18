# TODO: Add comment

###############################################################################
rm(list=ls(all=TRUE)) 
options(width = 438L)
library("RQuantLib")
library("fExoticOptions")
library("fAsianOptions")
library("fOptions")
source(paste(getwd(), "/option_mc.R", sep=""), echo=FALSE, encoding="GBK")

rValue = c("sigma","call_value","put_value","call_delta","put_delta","call_vega","put_vega")

#=============================================================================================================================================================
# TODO # development in progress
#=============================================================================================================================================================
#----------------------------------------------------- code timing -----------------------------------------------------

#number of simulation runs
nSims = 50
minSteps = 500

#number of discrete sampling points
cDays = 60
S = 100
X = 97
r = 0.1
q = 0.2
volatility = 0.25
option_style = "vanilla"
option_style = "arithmetic"

system.time(mapply(OptionMC, rep(option_style,100), S, X, r, q, cDays, volatility, nSims, minSteps, USE.NAMES=FALSE))

rValue = c("sigma", "call_value", "call_delta", "put_value", "put_delta")
system.time(apply(matrix(rep(option_style,100)), 1, OptionMC, S, X, r, q, cDays, volatility, nSims, minSteps, rValue))

rValue = c("sigma", "call_value", "put_value")
system.time(apply(matrix(rep(option_style,100)), 1, OptionMC, S, X, r, q, cDays, volatility, nSims, minSteps, rValue))

rValue = c("sigma", "call_value")
system.time(apply(matrix(rep(option_style,100)), 1, OptionMC, S, X, r, q, cDays, volatility, nSims, minSteps, rValue))



OptionMCv(option_style, S, X, r, q, cDays, volatility, nSims, minSteps, avgRuns, c("sigma","call_value","call_delta","put_value","put_delta"))

system.time(OptionMCv(option_style, S, X, r, q, cDays, volatility, nSims, minSteps, avgRuns, c("sigma")))

system.time(OptionMCv(option_style, S, X, r, q, cDays, volatility, nSims, minSteps, avgRuns, c("sigma","call_value","call_delta","put_value","put_delta")))

#		
#		optionMC1 = mapply(OptionMC, rep(option_style,100), valuation_data[1,]$settlement, strike[1], risk_free_interest, 0, days_to_maturity, pricing_volatility_adj_vec[1], nSims, minSteps, USE.NAMES=FALSE)
#		optionMC2 = mapply(OptionMC, rep(option_style,100), valuation_data[1,]$settlement, strike[2], risk_free_interest, 0, days_to_maturity, pricing_volatility_adj_vec[2], nSims, minSteps, USE.NAMES=FALSE)
#		
#		optionMC1_mean = mean(as.vector(unlist(optionMC1[paste(option_type[k],"value",sep="_"),])))
#		optionMC2_mean = mean(as.vector(unlist(optionMC2[paste(option_type[k],"value",sep="_"),])))
#		
#		pricing_option1 = list(value=optionMC1_mean)
#		pricing_option2 = list(value=optionMC2_mean)

#	



#=============================================================================================================================================================
# TODO # option_mc.R verification code
#=============================================================================================================================================================
#----------------------------------------------------- simulation volatility-----------------------------------------------------
#number of simulation runs
nSims = 1000
nSteps = 100
S = 100
r = 0.05
q = 0
volatility = 0.25

#drift term
mu = r - q
#time increment 
dt = 1 / 365 / nSteps

#when "nSteps=1", volatility of simulated price path converges to volatility as "cDays" increases, increase "nSims" does not improve convergence
#increase "nSteps" can improve convergence, even with a smaller "nSims" value
#may need to have at least "cDays*nSims*nSteps=100000" to converge
vol_vec = vector()
for (cDays in 30:30) {		
#	standard normal distribution random number
	z = rnorm(cDays*nSims*nSteps, mean=0, sd=1)		
#	generate log-normal return matrix
	return_matrix = matrix(exp((mu - 0.5 * volatility ^ 2) * dt + volatility * sqrt(dt) * z), ncol=nSims)	
#	return value: price path matrix 	
	path = rbind(matrix(rep(S,nSims),ncol=nSims), S*apply(return_matrix,2,cumprod))	
#	calculate the volatility of path
	vol = mean(apply(path, 2, function(x){sqrt(365*nSteps*mean(diff(log(x),n=1)^2))}))	
	vol_vec = c(vol_vec, vol)
}

plot(vol_vec, type="l", col="blue")
abline(h=volatility, col="black", pch=22, lty=2)

#----------------------------------------------------- mc method v.s. analytic method -----------------------------------------------------
#OptionMC(style, S, X, r, q, cDays, volatility, nSims, minSteps=500) 

nSims = 10000
cDays = 20
S = 100
X = 103
r = 0.05
q = 0.
volatility = 0.25

#vanilla
optionMC = OptionMC("vanilla", S, X, r, q, cDays, volatility, nSims)	
paste(rValue, round(unlist(optionMC[rValue]),4),sep="=",collapse=" | ")
optionMC$call_delta - optionMC$put_delta
EuropeanOption("call", tail(S,n=1), X, 0, r, cDays/365, volatility)
EuropeanOption("put", tail(S,n=1), X, 0, r, cDays/365, volatility)

#geometric, previous path unknown
nn = OptionMC("geometric", S, X, r, q, cDays, volatility, nSims)
paste(rValue, round(unlist(nn[rValue]),4),sep="=",collapse=" | ")
AsianOption(averageType="geometric", type="call", underlying=S, strike=X, dividendYield=q, riskFreeRate=r, maturity=cDays/365, volatility=volatility)
AsianOption(averageType="geometric", type="put", underlying=S, strike=X, dividendYield=q, riskFreeRate=r, maturity=cDays/365, volatility=volatility)

#----------------------------------------------------- vanilla, geometric, arithmetic -----------------------------------------------------
nSims = 1000
cDays = 10
X = 100
r = 0.
q = 0.
volatility = 0.25

S = 100
mm = OptionMC("vanilla", S, X, r, q, cDays, volatility, nSims)
paste(rValue, round(unlist(mm[rValue]),4),sep="=",collapse=" | ")

S = rep(100, 50)
nn = OptionMC("geometric", S, X, r, q, cDays, volatility, nSims)
paste(rValue, round(unlist(nn[rValue]),4),sep="=",collapse=" | ")
aa = OptionMC("arithmetic", S, X, r, q, cDays, volatility, nSims)
paste(rValue, round(unlist(aa[rValue]),4),sep="=",collapse=" | ")

S = 100
nn = OptionMC("geometric", S, X, r, q, cDays, volatility, nSims)
AsianOption(averageType="geometric", type="call", underlying=S, strike=X, dividendYield=q, riskFreeRate=r, maturity=cDays/365, volatility=volatility)

#============================================================================================================================================================
# TODO # OptionMC, analytical v.s. numerical, arithmetic v.s. geometric
#============================================================================================================================================================
#--------------------------------------------------- vanilla ---------------------------------------------------
#number of simulation runs
nSims = 100
minSteps = 500
avgRuns = 100

#number of discrete sampling points
cDays = 60
S = 3000
X = 3000
r = 0.
q = 0.
volatility = 0.25
option_style = "vanilla"
#option_style = "geometric"

#analytical method, used as a reference
if (option_style == "vanilla") {
#	vanilla options
	option_call = EuropeanOption("call", S, X, q, r, cDays/365, volatility)		
	option_put  = EuropeanOption("put",  S, X, q, r, cDays/365, volatility)
} else if (option_style == "geometric") {
	option_call = AsianOption(averageType="geometric", type="call", underlying=S, strike=X, dividendYield=q, riskFreeRate=r, maturity=cDays/365, volatility=volatility)	
	option_put  = AsianOption(averageType="geometric", type="put",  underlying=S, strike=X, dividendYield=q, riskFreeRate=r, maturity=cDays/365, volatility=volatility)	
} 

option = list(call=option_call, put=option_put)

#numerical method, "optionMC_vec" function
optionMC_vec = OptionMCv(option_style, S, X, r, q, cDays, volatility, nSims, minSteps, avgRuns)

par(mfrow=c(3,3))
for (i in c("call", "put")) {
	for (j in c("value", "delta", "vega")) {
#		#numerical method, "optionMC_vec" function
		g_vec = optionMC_vec[["data"]][,paste(i,j,sep="_")]		
#		gf is a measure of the simulation results, smaller the value better the results
		gf = 10000 * (mean(g_vec) / unlist(option[[i]][j]) - 1)		
		plot(g_vec, col="blue", xlab=paste("analytic",j,"=",round(unlist(option[[i]][j]),4),sep=" "), ylab=paste(option_style,i,"option",sep=" "))			
		title(paste("MC", j, "=", round(mean(g_vec),4), "(mean)", round(sd(g_vec),4), "(sd)\ngf =", round(gf,4), "(mean)", sep=" "))	
		abline(h=option[[i]][j], col="black", pch=22, lty=2)
		abline(h=mean(g_vec), col="blue", pch=22, lty=2)				
	}
}

#numerical method, "optionMC_vec" function
sigma_vec = optionMC_vec[["data"]][, "sigma"]
plot(sigma_vec, col="blue", xlab=paste("volatility =", volatility, sep=" "))
title(paste("MC sigma =", round(mean(sigma_vec),4), "(mean)", round(sd(sigma_vec),4), "(sd)", sep=" "))	
abline(h=mean(sigma_vec), col="blue", pch=22, lty=2)
abline(h=volatility, col="black", pch=22, lty=2)
legend("bottomleft", legend=c(paste("nSims=",nSims,sep=""),paste("cDays=",cDays,sep="")))	

#--------------------------------------------------- arithmetic v.s. geometric ---------------------------------------------------
#analytical method
option_call = AsianOption(averageType="geometric", type="call", underlying=S, strike=X, dividendYield=q, riskFreeRate=r, maturity=cDays/365, volatility=volatility)	
option_put  = AsianOption(averageType="geometric", type="put",  underlying=S, strike=X, dividendYield=q, riskFreeRate=r, maturity=cDays/365, volatility=volatility)	
option_geometric = list(call=option_call, put=option_put)

##numerical method
#optionMC_geometric = mapply(OptionMC, rep("geometric",avgRuns), S, X, r, q, cDays, volatility, nSims, USE.NAMES=FALSE)
#optionMC_arithmetic = mapply(OptionMC, rep("arithmetic",avgRuns), S, X, r, q, cDays, volatility, nSims, USE.NAMES=FALSE)

#numerical method, "optionMC_vec" function
optionMC_vec_geometric  = OptionMCv("geometric",  S, X, r, q, cDays, volatility, nSims, minSteps, avgRuns)
optionMC_vec_arithmetic = OptionMCv("arithmetic", S, X, r, q, cDays, volatility, nSims, minSteps, avgRuns)

par(mfrow=c(3,3))
for (i in c("call", "put")) {
	for (j in c("value", "delta", "vega")) {
##		numerical method		
#		geometric_vec = as.vector(unlist(optionMC_geometric[paste(i,j,sep="_"),]))
#		arithmetic_vec = as.vector(unlist(optionMC_arithmetic[paste(i,j,sep="_"),]))
		
##		numerical method, "optionMC_vec" function
		geometric_vec = optionMC_vec_geometric[["data"]][,paste(i,j,sep="_")]
		arithmetic_vec = optionMC_vec_arithmetic[["data"]][,paste(i,j,sep="_")]
		
#		plot both "geometric" and "arithmetic" results
		ylim = range(c(geometric_vec,arithmetic_vec))
		plot(geometric_vec, col="blue", xlab=paste("analytic geometric",j,"=",round(unlist(option_geometric[[i]][j]),4),sep=" "), ylab=paste(i,"option",sep=" "))
		points(arithmetic_vec, col="red")
		title(paste("MC", "geometric =", round(mean(geometric_vec),4), "(mean)", round(sd(geometric_vec),4), "(sd)\narithmetic =", round(mean(arithmetic_vec),4), "(mean)", round(sd(arithmetic_vec),4), "(sd)", sep=" "))
		abline(h=option_geometric[[i]][j], col="black", pch=22, lty=2)
		abline(h=mean(geometric_vec), col="blue", pch=22, lty=2)
		abline(h=mean(arithmetic_vec), col="red", pch=22, lty=2)
	}
}
##numerical method	
#sigma_vec_geometric = as.vector(unlist(optionMC_geometric["sigma",]))
#sigma_vec_arithmetic = as.vector(unlist(optionMC_arithmetic["sigma",]))

#numerical method, "optionMC_vec" function
sigma_vec_geometric = optionMC_vec_geometric[["data"]][, "sigma"] 
sigma_vec_arithmetic = optionMC_vec_arithmetic[["data"]][, "sigma"] 
		
plot(sigma_vec_geometric, col="blue", xlab=paste("volatility =", volatility, sep=" "))
points(sigma_vec_arithmetic, col="red")
title(paste("MC geometric =", round(mean(sigma_vec_geometric),4), "(mean)", round(sd(sigma_vec_geometric),4), "(sd)\narithmetic =", round(mean(sigma_vec_arithmetic),4), "(mean)", round(sd(sigma_vec_arithmetic),4), "(sd)", sep=" "))	
abline(h=mean(sigma_vec_geometric), col="blue", pch=22, lty=2)
abline(h=mean(sigma_vec_arithmetic), col="red", pch=22, lty=2)
abline(h=volatility, col="black", pch=22, lty=2)
legend("bottomleft", legend=c("geometric","arithmetic"), text.col=c("blue","red"))

#--------------------------------------------------- strike price effect ---------------------------------------------------
#for a given underlying price S with different strike price X_vec, 
#to exam the value and delta difference between analytical geometric option v.s. numerically simulated geometric and arithmetic option
#1. analytical v.s. numerically, the absolute value difference increases as the option is more ITM, however, in percentage term, it's the opposite
#2.1. geometric v.s. arithmetic, value of arithmetic call option is higher than geometric option, value of arithmetic put option is lower than geometric option
#2.2. the difference increases as option is more ITM
#2.3. delta of arithmetic option is higher than geometric option, for both call and put option

#strike price vector, +/- 10%
X_vec = S * (1 + seq(-20,20,1) * 0.005)

#analytical method
option_call_vec = mapply(AsianOption, "geometric", "call", S, X_vec, q, r, cDays/365, volatility)
option_put_vec  = mapply(AsianOption, "geometric", "put",  S, X_vec, q, r, cDays/365, volatility)
option_vec = list(
		call=list(value=as.vector(unlist(option_call_vec["value",])), delta=as.vector(unlist(option_call_vec["delta",])), vega=as.vector(unlist(option_call_vec["vega",]))), 
		put =list(value=as.vector(unlist(option_put_vec["value",])),  delta=as.vector(unlist(option_put_vec["delta",])),  vega=as.vector(unlist(option_put_vec["vega",]))))

#numerical method
optionMC_geometric_vec  = mapply(OptionMCv, "geometric",  S, X_vec, r, q, cDays, volatility, nSims, minSteps, avgRuns)
optionMC_arithmetic_vec = mapply(OptionMCv, "arithmetic", S, X_vec, r, q, cDays, volatility, nSims, minSteps, avgRuns)

par(mfrow=c(3,3))
for (i in c("call", "put")) {
	for (j in c("value", "delta", "vega")) {
#		difference between analytical method and numerical method simulated results, for both geometric and arithmetic average price options
		diff_geometric_temp  = as.vector(unlist(optionMC_geometric_vec[paste(i,j,sep="_"),]))  - as.vector(unlist(option_vec[[i]][j]))
		diff_arithmetic_temp = as.vector(unlist(optionMC_arithmetic_vec[paste(i,j,sep="_"),])) - as.vector(unlist(option_vec[[i]][j]))		
		ylim = range(c(diff_geometric_temp,diff_arithmetic_temp))
		plot(X_vec, diff_geometric_temp, col="blue", ylim=ylim, xlab=paste("strike price (underlying=",S,")",sep=""), ylab=paste(i,"option",sep=" "))		
		title(paste(i, j, "diff", sep=" "))		
		points(X_vec, diff_arithmetic_temp, col="red")		
	
#		loess line for both geometric and arithmetic points generated through numerical method
		diff_arithmetic_pred = predict(loess(diff_arithmetic_temp ~ X_vec), se=TRUE)		
		lines(X_vec, diff_arithmetic_pred$fit, col="red")		
		diff_geometric_pred = predict(loess(diff_geometric_temp ~ X_vec), se=TRUE)		
		lines(X_vec, diff_geometric_pred$fit, col="blue")		

#		reference line
		abline(h=0, col="black", pch=22, lty=2)		
	}
}

#============================================================================================================================================================
# TODO # fExoticOptions Examples
#============================================================================================================================================================
#Arguments
#b, the annualized cost-of-carry rate, a numeric value; e.g. 0.1 means 10% pa.
#description, a character string which allows for a brief description.
#r, a numeric value, the annualized rate of interest; e.g. 0.25 means 25% pa.
#S, SA, the asset price, a numeric value.
#sigma, a numeric value, the annualized volatility of the underlying security; e.g. 0.3 means 30% volatility pa.
#tau, [TurnWakeAsianApprox*] - is the time to the beginning of the average period.
#time, Time, a numeric value, the time to maturity measured in years; e.g. 0.5 means 6 months.
#title, a character string which allows for a project title.
#TypeFlag, a character string either "c" for a call option or a "p" for a put option.
#X, the exercise price, a numeric value.

## Examples from Chapter 2.12 in E.G. Haug's Option Guide (1997)

## Geometric Average Rate Option:
AsianOption(averageType="geometric", type="call", underlying=100, strike=100, dividendYield=0.0, riskFreeRate=0.02, maturity=0.25, volatility=0.5)
GeometricAverageRateOption(TypeFlag="c", S=100, X=100, Time=0.25, r=0.02, b=0.0, sigma=0.5)

## Turnbull Wakeman Approximation: return "Option Price: NaN" when b=0
TurnbullWakemanAsianApproxOption(TypeFlag="c", S=100, SA=100, X=100, Time=0.25, time=0.25, tau=0.0, r=0.0, b=0.02, sigma=0.25)

## Levy Asian Approximation: return "Option Price: NaN" when b=0
LevyAsianApproxOption(TypeFlag="c", S=100, SA=100, X=100, Time=0.25, time=0.25, r=0.0, b=0.02, sigma=0.25)

#--------------------------------------------------- https://github.com/systematicinvestor/SIT/blob/master/R/random.r# ---------------------------------------------------
###############################################################################
# Random Points in an n-Dimensional Hypersphere
# by Roger Stafford
# 24 Dec 2005 (Updated 28 Dec 2005)
#
# Randomly and uniformly distributes points throughout a hypersphere. 
#
# http://www.mathworks.com/matlabcentral/fileexchange/9443-random-points-in-an-n-dimensional-hypersphere
###############################################################################
# This function returns an m by n array, X, in which 
# each of the m rows has the n Cartesian coordinates 
# of a random point uniformly-distributed over the 
# interior of an n-dimensional hypersphere with 
# radius r and center at the origin.  The function 
# 'randn' is initially used to generate m sets of n 
# random variables with independent multivariate 
# normal distribution, with mean 0 and variance 1.
# Then the incomplete gamma function, 'gammainc', 
# is used to map these points radially to fit in the 
# hypersphere of finite radius r with a uniform % spatial distribution.
# Roger Stafford - 12/23/05
#' @export 
###############################################################################
randsphere <- function
		(
		m,	# number of samples to draw
		n,	# dimension of distribution
		r	# radius of hypersphere with center at the origin
)
{
	x = matrix(rnorm( m * n ), nrow = m, ncol = n);
	s2 = apply(x^2, 1, sum)
	
	#return( x * repmat(r*(pgamma(s2/2,n/2)^(1/n))/sqrt(s2),1,n) )
	return( x * repmat(r*(runif(m)^(1/n))/sqrt(s2),1,n) )
}

###############################################################################
# Test for randsphere function
###############################################################################
randsphere.test <- function()
{
	load.packages('car,scatterplot3d')
	
	png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	# 2 dimensions
	x = randsphere(1000, 2, 1)
	y = x[, 2]
	x = x[, 1]
	
	par(mar = c(5,4,1,1))
	plot(x,y, pch = 20)
	ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)
	
	dev.off()
	png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	# 3 dimensions
	# plot 4 plots : 3d + 3 projections xy,xz,yz
	layout(matrix(1:4,nrow=2))
	
	x = randsphere(10000, 3, 1)
	z = x[, 3]
	y = x[, 2]
	x = x[, 1]
	
	scatterplot3d(x, y, z, highlight.3d = TRUE, pch = 20)
	
	par(mar = c(5,4,1,1))		
	plot(x, y, pch = 20)			
	ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)
	plot(x, z, pch = 20)			
	ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)
	plot(y, z, pch = 20)
	ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)
	
	dev.off()
}


###############################################################################
# Random Vectors with Fixed Sum
# by Roger Stafford
# 19 Jan 2006 (Updated 24 Jan 2006)
#
# Randomly and uniformly generates vectors with a specified sum and values in a specified interval.
#
# http://www.mathworks.com/matlabcentral/fileexchange/9700-random-vectors-with-fixed-sum
###############################################################################
# [x,v] = randfixedsum(n,m,s,a,b)
#
#   This generates an n by m array x, each of whose m columns
# contains n random values lying in the interval [a,b], but
# subject to the condition that their sum be equal to s.  The
# scalar value s must accordingly satisfy n*a <= s <= n*b.  The
# distribution of values is uniform in the sense that it has the
# conditional probability distribution of a uniform distribution
# over the whole n-cube, given that the sum of the x's is s.
#
#   The scalar v, if requested, returns with the total
# n-1 dimensional volume (content) of the subset satisfying
# this condition.  Consequently if v, considered as a function
# of s and divided by sqrt(n), is integrated with respect to s
# from s = a to s = b, the result would necessarily be the
# n-dimensional volume of the whole cube, namely (b-a)^n.
#
#   This algorithm does no "rejecting" on the sets of x's it
# obtains.  It is designed to generate only those that satisfy all
# the above conditions and to do so with a uniform distribution.
# It accomplishes this by decomposing the space of all possible x
# sets (columns) into n-1 dimensional simplexes.  (Line segments,
# triangles, and tetrahedra, are one-, two-, and three-dimensional
# examples of simplexes, respectively.)  It makes use of three
# different sets of 'rand' variables, one to locate values
# uniformly within each type of simplex, another to randomly
# select representatives of each different type of simplex in
# proportion to their volume, and a third to perform random
# permutations to provide an even distribution of simplex choices
# among like types.  For example, with n equal to 3 and s set at,
# say, 40% of the way from a towards b, there will be 2 different
# types of simplex, in this case triangles, each with its own
# area, and 6 different versions of each from permutations, for
# a total of 12 triangles, and these all fit together to form a
# particular planar non-regular hexagon in 3 dimensions, with v
# returned set equal to the hexagon's area.
#
# Roger Stafford - Jan. 19, 2006
#' @export 
###############################################################################
randfixedsum <- function
		(
		m,	# number of samples to draw
		n,	# dimension of distribution
		s,	# sum of each sample equal to s
		a,	# lower bound for each variable
		b	# upper bound for each variable
)
# The scalar value s must accordingly satisfy n*a <= s <= n*b.
{
	# Check the arguments.
	if( (s<n*a) | (s>n*b) | (a>=b) )
		stop('Inequalities n*a <= s <= n*b and a < b must hold.\n')
	
	# Rescale to a unit cube: 0 <= x(i) <= 1
	s = (s - n * a) / (b - a)
	
	
	# Construct the transition probability table, t.
	# t(i,j) will be utilized only in the region where j <= i + 1.
	k = max( min( floor(s), n - 1), 0)	# Must have 0 <= k <= n-1
	s = max( min( s, k + 1), k)			# Must have k <= s <= k+1
	
	s1 = s - (k : (k - n + 1))			# s1 & s2 will never be negative
	s2 = ((k + n) : (k+1)) - s
	
	w = matrix(0, n, (n + 1))
	realmax = 10^300
	w[1, 2] = realmax					# Scale for full 'double' range
	
	t = matrix(0, (n-1), n)
	tiny = 2^(-1074)					# The smallest positive matlab 'double' no.
	
	for( i in 2:n ) {
		tmp1 = w[(i - 1), 2 : (i + 1)] * s1[1 : i] / i
		tmp2 = w[(i - 1), 1 : i] * s2[(n - i + 1) : n] / i
		
		w[i, 2 : (i + 1)] = tmp1 + tmp2
		tmp3 = w[i, 2 : (i+1)] + tiny			# In case tmp1 & tmp2 are both 0,
		tmp4 = (s2[(n - i + 1) : n] > s1[1:i])	# then t is 0 on left & 1 on right
		t[(i - 1), 1 : i] = (tmp2 / tmp3) * tmp4 + (1 - tmp1 / tmp3) * (!tmp4)
	}
	
	
	# Derive the polytope volume v from the appropriate
	# element in the bottom row of w.
	v = n^(3/2) * (w[n, (k + 2)] / realmax) * (b - a)^(n - 1)
	
	
	# Now compute the matrix x.
	x = matrix(0, n, m)
	rt = matrix( runif((n-1) * m), (n-1), m)	# For random selection of simplex type
	rs = matrix( runif((n-1) * m), (n-1), m)	# For random location within a simplex
	
	s = repmat(s, 1, m)
	j = repmat((k + 1), 1, m)	# For indexing in the t table
	
	sm = matrix(0, 1, m)
	pr = matrix(1, 1, m)	# Start with sum zero & product 1
	
	for( i in (n - 1):1) {  # Work backwards in the t table
		e = (rt[(n - i), ] <= t[i, j])		# Use rt to choose a transition
		sx = rs[(n - i), ]^(1/i)			# Use rs to compute next simplex coord.
		sm = sm + (1 - sx) * pr * s / (i+1)	# Update sum
		pr = sx * pr						# Update product
		x[(n - i), ] = sm + pr * e			# Calculate x using simplex coords.
		s = s - e
		j = j - e							# Transition adjustment
	}
	
	x[n, ] = sm + pr * s					# Compute the last x
	
	
	# Randomly permute the order in the columns of x and rescale.
	rp = matrix( runif(n * m), n, m)		# Use rp to carry out a matrix 'randperm'
	p = apply(rp, 2, order)
	x = (b - a) * x[p + repmat(t(seq(0, n * (m - 1), by = n)), n, 1)] + a	# Permute & rescale x
	
	x = matrix(x, n, m)
	#apply(x,2,sum)	
	
	return(t(x))
}

# Alternative Idea
# http://programming-r-pro-bro.blogspot.com/2011/11/modern-portfolio-optimization-theory.html
# diff(c(0,sort(runif(n-1)), 1))
# diff(c(0,sort(runif(n-1)), 1)) * 0.5
# diff(c(0,sort(runif(n-1,0,0.8)), 0.8))
#
#PORTFOLIO OPTIMIZATION FOR VAR, CVAR, OMEGA AND UTILITY WITH GENERAL RETURN DISTRIBUTIONS:
#A MONTE CARLO APPROACH FOR LONG-ONLY AND BOUNDED SHORT PORTFOLIOS WITH OPTIONAL ROBUSTNESS
#AND A SIMPLIFIED APPROACH TO COVARIANCE MATCHING
#by WILLIAM T. SHAW
#
#http://luc.devroye.org/rnbookindex.html
# Non-Uniform Random Variate Generation by Luc Devroye (1986), chapter 5


###############################################################################
# Test for randfixedsum function
###############################################################################
randfixedsum.test <- function()
{
	load.packages('car,scatterplot3d')
	
	png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	# 2 dimensions
	x = randfixedsum(100, 2, 1, 0.2, 0.8)
	y = x[, 2]
	x = x[, 1]
	
	par(mar = c(5,4,1,1))
	plot(x,y, pch = 20)
	
	dev.off()
	png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	# 3 dimensions
	# http://www.statmethods.net/graphs/scatterplot.html
	# plot 4 plots : 3d + 3 projections xy,xz,yz
	layout(matrix(1:4,nrow=2))
	
	x = randfixedsum(1000, 3, 1, 0.2, 0.8)
	z = x[, 3]
	y = x[, 2]
	x = x[, 1]
	
	scatterplot3d(x, y, z, highlight.3d = TRUE, pch = 20, angle=190)
	
	par(mar = c(5,4,1,1))		
	plot(x, y, pch = 20)			
	plot(x, z, pch = 20)			
	plot(y, z, pch = 20)
	
	
	dev.off()
}


###############################################################################
# Note rowSums and colSums are alot faster than apply. For example:
# prices = asset.paths(S, c(r,r), sigma = cov.matrix, N, periods = seq(0, 1/12, 1/360))
#
# temp = prices
# dim(temp) = c(2,31*N)
#
# tic(12)
# a = apply(temp,2,sum)
# toc(12)
#
# tic(12)
# b=colSums(temp)
# toc(12)
#
# range(a-b)
###############################################################################
# s0 * matrix(exp(nu * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
#
# s0 * apply(matrix(exp(nu * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims), 2, cumprod)
# s0 * exp(apply(matrix(nu * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims), nc=nsims), 2, cumsum))
###############################################################################
# Simulating Multiple Asset Paths
# http://www.goddardconsulting.ca/matlab-monte-carlo-assetpaths.html
# http://en.wikipedia.org/wiki/Geometric_Brownian_motion
#
# This function returns the simulation matrix (time, simulation, asset)
# Instead of using mvrnorm from MASS library, we can use rmvnorm function from mvtnorm package
#' @export 
###############################################################################
asset.paths <- function(s0, mu, sigma, 
		nsims = 10000, 
		periods = c(0, 1)	# time periods at which to simulate prices
) 
{
	s0 = as.vector(s0)
	nsteps = len(periods)
	dt = c(periods[1], diff(periods))
	
	if( len(s0) == 1 ) {
		drift = mu - 0.5 * sigma^2
		if( nsteps == 1 ) {
			s0 * exp(drift * dt + sigma * sqrt(dt) * rnorm(nsims))
		} else {
			temp = matrix(exp(drift * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
			for(i in 2:nsteps) temp[i,] = temp[i,] * temp[(i-1),]
			s0 * temp
		}
	} else {
		require(MASS)
		drift = mu - 0.5 * diag(sigma)
		n = len(mu)
		
		if( nsteps == 1 ) {
			s0 * exp(drift * dt + sqrt(dt) * t(mvrnorm(nsims, rep(0, n), sigma)))
		} else {
			temp = array(exp(as.vector(drift %*% t(dt)) + t(sqrt(dt) * mvrnorm(nsteps * nsims, rep(0, n), sigma))), c(n, nsteps, nsims))
			for(i in 2:nsteps) temp[,i,] = temp[,i,] * temp[,(i-1),]
			s0 * temp
		}
	}
}	


###############################################################################
# Test asset.paths function
###############################################################################
asset.paths.test <- function()
{
	#*****************************************************************
	# Plot some price paths
	#******************************************************************  
	S = c(100,105)
	X = 98
	Time = 0.5
	r = 0.05
	sigma = c(0.11,0.16)
	rho = 0.63
	N = 10000
	
	# Single Asset for 10 years
	periods = 0:10
	prices = asset.paths(S[1], r, sigma[1], N, periods = periods)
	
	# plot
	png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
	matplot(prices[,1:100], type='l', xlab='Years', ylab='Prices',
			main='Selected Price Paths')
	dev.off()					
	
	
	# Multiple Assets for 10 years
	periods = 0:10
	cov.matrix = sigma%*%t(sigma) * matrix(c(1,rho,rho,1),2,2)
	prices = asset.paths(S, c(r,r), cov.matrix, N, periods = periods)
	
	# plot
	png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    	
	layout(1:2)
	matplot(prices[1,,1:100], type='l', xlab='Years', ylab='Prices',
			main='Selected Price Paths for Asset 1')
	matplot(prices[2,,1:100], type='l', xlab='Years', ylab='Prices',
			main='Selected Price Paths for Asset 2')
	dev.off()					
	
	# check correlation
	cor(as.vector(prices[1,,] / mlag(prices[1,,])),
			as.vector(prices[2,,] / mlag(prices[2,,])),
			use='complete.obs', method='pearson')
	
	
	#*****************************************************************
	# Price European Call Option
	#******************************************************************  
	load.packages('fOptions')
	
	# BlackScholes
	GBSOption(TypeFlag = "c", S = S[1], X = X, Time = Time, r = r, b = r, sigma = sigma[1])
	
	# Monte Carlo simulation	
	N = 1000000	
	prices = asset.paths(S[1], r, sigma[1], N, periods = Time)
	future.payoff = pmax(0, prices - X)
	discounted.payoff = future.payoff * exp(-r * Time)
	# option price
	mean(discounted.payoff) 
	
	#*****************************************************************
	# Price Asian Call Option
	#******************************************************************  
	load.packages('fExoticOptions')
	
	Time = 1/12
	
	# Approximation
	TurnbullWakemanAsianApproxOption(TypeFlag = "c", S = S[1], SA = S[1], 
			X = X, Time = Time, time = Time, tau = 0 , r = r, b = r, sigma = sigma[1])
	
	# Monte Carlo simulation		
	N = 100000
	periods = seq(0,Time,1/360)
	n = len(periods)
	prices = asset.paths(S[1], r, sigma[1], N, periods = periods)
	future.payoff = pmax(0, colSums(prices)/n - X)
	discounted.payoff = future.payoff * exp(-r * Time)
	# option price
	mean(discounted.payoff) 
	
	#*****************************************************************
	# Price Basket Option
	#******************************************************************  
	
	Time = 0.5
	
	# Approximation
	TwoRiskyAssetsOption(TypeFlag = "cmax", S1 = S[1], S2 = S[2],
			X = X, Time = Time, r = r, b1 = r, b2 = r,
			sigma1 = sigma[1], sigma2 = sigma[2], rho = rho)
	
	# Monte Carlo simulation		
	N = 100000
	cov.matrix = sigma%*%t(sigma) * matrix(c(1,rho,rho,1),2,2)
	prices = asset.paths(S, c(r,r), sigma = cov.matrix, N, periods = Time)
	future.payoff = pmax(0, apply(prices,2,max) - X)
	discounted.payoff = future.payoff * exp(-r * Time)
	# option price
	mean(discounted.payoff) 
	
	#*****************************************************************
	# Price Asian Basket Option
	#******************************************************************  
	
	Time = 1/12
	
	# Monte Carlo simulation		
	N = 10000
	periods = seq(0,Time,1/360)
	n = len(periods)
	
	prices = asset.paths(S, c(r,r), sigma = cov.matrix, N, periods = periods)
	future.payoff = pmax(0, colSums(apply(prices,c(2,3),max))/n - X)
	discounted.payoff = future.payoff * exp(-r * Time)
	# option price
	mean(discounted.payoff) 
	
}

###############################################################################
# Generate All Possible Combinations
# there are 2^n - 1 distinct permutations
# Please note: first row contains all zeros
#' @export 
###############################################################################
all.permutations <- function(n = 1) {
	m = matrix(F,2^n,n)
	m[2,1] = T
	if (n == 1) return(m)
	
	istart = 2
	for(i in 2:n) {
		index = (istart+1):(2*istart)
		m[index, ] = m[1:istart,]
		m[index, i] = T
		istart = istart * 2	
	}
	return(m)
}







