############################################################################################################################################################
# TODO # monte carlo option pricing model
############################################################################################################################################################

#single run of option pricing using Monte Carlo method
OptionMC = function(style, S, X, r, q, cDays, volatility, nSims, minSteps=500) {
#	cDays is the number of calendar days used when discount to present value
#	nSteps is the number of steps when generating simulation path, need to increase this number so that "sigma" -> "volatility"
#	"cDays * nSteps" is the total random numbers generated per path
#	http://stackoverflow.com/questions/25946852/r-monte-carlo-simulation-price-path-converging-volatility-issue	
	nSteps = ifelse(cDays>=1, max(ceiling(minSteps/cDays),1), 0)
	
#	drift term
	mu = r - q
#	underlying price, S can be a vector with previous prices
	S0 = tail(S, n=1)
#	every calendar day increment 
	dt = 1 / 365 / nSteps
#	time to maturity express in fraction of years
	tMaturity = cDays / 365	
#	standard normal distribution random number
	z = rnorm(cDays*nSims*nSteps, mean=0, sd=1)	
	
#	----------------------------------------------------- value -----------------------------------------------------
#	generate log-normal return matrix
	return_matrix = matrix(exp((mu - 0.5 * volatility ^ 2) * dt + volatility * sqrt(dt) * z), ncol=nSims)
#	return value: price path matrix 	
	path = rbind(matrix(rep(S,nSims),ncol=nSims), S0*apply(return_matrix,2,cumprod))
#	different option style
#	plain vanilla option, use the final price as settlement price	
#	arithmetic mean of the entire price path, fixed strike price	
#	geometric mean of the entire price path, fixed strike price	
	if (style == "vanilla") {ref = tail(path, n=1)} 
	else if (style == "arithmetic") {ref = apply(path, 2, mean)} 
	else if (style == "geometric") {ref = apply(path, 2, function(x){exp(mean(log(x)))})} 
	else {}		
#	option's value, average of difference between terminal value and strike price, then discount back to present value
#	value = exp(- r * tMaturity) * sum(pmax(ifelse(type=="call",1,-1) * (ref - X), 0)) / nSims		
	call_value = exp(- r * tMaturity) * sum(pmax(ref - X, 0)) / nSims
	put_value  = exp(- r * tMaturity) * sum(pmax(X - ref, 0)) / nSims
	
#	----------------------------------------------------- delta -----------------------------------------------------	
#	calculate option delta, generate a new price path, with the same brownian motion term "z"
#	S0 is known, when simulate path_delta, add small increment from S0
	path_delta = rbind(matrix(rep(S,nSims),ncol=nSims), S0*1.0001*apply(return_matrix,2,cumprod))	
	path_delta[length(S),] = path_delta[length(S),] * 1.0001			
#	different option style
	if (style == "vanilla") {ref_delta = tail(path_delta, n=1)} 
	else if (style == "arithmetic") {ref_delta = apply(path_delta, 2, mean)} 
	else if (style == "geometric") {ref_delta = apply(path_delta, 2, function(x){exp(mean(log(x)))})} 
	else {}		
#	option's value with "path_delta", i.e., values calculated for calculating delta
#	delta is defined as the change of option value w.r.t. change of underlying price (S0)
#	delta = ((exp(- r * tMaturity) * sum(pmax(ifelse(type=="call",1,-1) * (ref_delta - X), 0)) / nSims) - value) / (S0 * 0.0001)
	call_delta = ((exp(- r * tMaturity) * sum(pmax(ref_delta - X, 0)) / nSims) - call_value) / (S0 * 0.0001)
	put_delta  = ((exp(- r * tMaturity) * sum(pmax(X - ref_delta, 0)) / nSims) - put_value)  / (S0 * 0.0001)
	
#	----------------------------------------------------- vega -----------------------------------------------------	
#	a small change of "volatility" will change the return matrix, i.e., "return_matrix_vega"
	return_matrix_vega = matrix(exp((mu - 0.5 * (volatility*1.0001) ^ 2) * dt + (volatility*1.0001) * sqrt(dt) * z), ncol=nSims)
	path_vega = rbind(matrix(rep(S,nSims),ncol=nSims), S0*apply(return_matrix_vega,2,cumprod))
#	different option style
	if (style == "vanilla") {ref_vega = tail(path_vega, n=1)} 
	else if (style == "arithmetic") {ref_vega = apply(path_vega, 2, mean)} 
	else if (style == "geometric") {ref_vega = apply(path_vega, 2, function(x){exp(mean(log(x)))})} 
	else {}		
#	vega is defined as the change of option value w.r.t. change of volatility
	call_vega = ((exp(- r * tMaturity) * sum(pmax(ref_vega - X, 0)) / nSims) - call_value) / (volatility * 0.0001)
	put_vega  = ((exp(- r * tMaturity) * sum(pmax(X - ref_vega, 0)) / nSims) - put_value)  / (volatility * 0.0001)
	
#	----------------------------------------------------- sigma -----------------------------------------------------	
#	volatility of simulated price path, for "vanilla" options, "sigma" converges to "volatility", as cDays and nSteps get bigger
#	for "arithmetic" and "geometric" options, "sigma" maybe different from "volatility" since "S" may contain existing price path
	sigma = mean(apply(path, 2, function(x){sqrt(365*nSteps*mean(diff(log(x),n=1)^2))}))
	
#	return value:
	rValue = c("path","sigma","call_value","call_delta","put_value","put_delta","call_vega","put_vega")
	eval(parse(text=paste("list(", paste(rValue, "=", rValue, sep="", collapse=","), ")", sep="")))
}

#vector form of "OptionMC" function, "avgRuns" is the number of runs simulated, return the average results of rValue
#"avgRuns" must be greater than or equal to 2
OptionMCv = function(style, S, X, r, q, cDays, volatility, nSims, minSteps=500, avgRuns=100) {
#	"optionMC" is a list which contains "avgRuns" number of runs of simulated "rValue", "path" is not included in "rValue"
	optionMC = apply(matrix(rep(style,max(avgRuns,2))), 1, OptionMC, S, X, r, q, cDays, volatility, nSims, minSteps)
#	extract "rValue" from the "optionMC" list
	rValue = c("sigma","call_value","call_delta","put_value","put_delta","call_vega","put_vega")
	mm = data.frame(apply(matrix(rValue), 1, function(m){as.vector(sapply(optionMC, function(x){unlist(x[m])}, simplify=TRUE))}))
#	use the same name as "rValue" for the vector form
	colnames(mm) = rValue	
#	return value: mean of "rValue" element, and its orignal "data"
	append(as.list(data.frame(t(apply(mm, 2, mean)))), list(data=mm))
}

#** deprecated **
#specify the number of exiting points ("cDaysIndex") used in simulation, can use "apply()" wrapper around "OptionMC" function
OptionMC2 = function(cDaysIndex, style, S_vec, X, r, q, volatility, nSims, minSteps=500) {
#	cDaysIndex shall be less or equal to length(S)
	if (cDaysIndex > length(S)) stop(paste("cDaysIndex=", cDaysIndex, " is greater than length(S)=", length(S), sep=""))
#	S_vec is a know price path, use "cDaysIndex" number of know price points for numerical calculation
	S = head(S_vec, n=cDaysIndex)
#	number of trading days left, when S=S_vec (cDays=0), it is the last day of the price path
	cDays = length(S_vec) - cDaysIndex
#	monte carlo simulation to calculate the value of option
	option = OptionMC(style, S, X, r, q, cDays, volatility, nSims, minSteps)	
#	return value:
	rValue=c("path","sigma","call_value","call_delta","put_value","put_delta","call_vega","put_vega")
	eval(parse(text=paste("list(", paste(rValue, "=option$", rValue, sep="", collapse=","), ")", sep="")))
}


