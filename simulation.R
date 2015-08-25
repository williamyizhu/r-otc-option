############################################################################################################################################################
# TODO # Add comment
############################################################################################################################################################
#1. theoretical volatility is the key, need to have a better model to understand theoretical volatility, e.g., GARCH model, volatility seasonal effects, fundmental market behaviour, etc.
#2. intra-day, daily, multiple days re-balancing, i.e., re-balancing frequency, if it is necessary to simulate all that???
#3. over / under hedging has the equivalent effect as taking directional trade
#4. human discretional intra-day hedgin decision has little effect on improving hedging results, i.e., reducing hedging pnl variance
#5. there is no way to hedge the Gamma in China, because there is no exchange-traded option market yet
#6. there is no need to run simulation on GBM generated path, since volatility and mean is known in GBM, and theoretical volatility used for pricing the option is pre-determined and higher than volatility used for GBM. 
#7. simulation on historical data, and get the distribution for both call and put, use pre-defined rules, and minimized the degree of freedom, e.g., only use theoretical volatility as a function of previous realized volatility
#8. there is no options market in China, therefore only sell short-dated options

############################################################################################################################################################
# TODO # prepare "valuation_data" for simulation
############################################################################################################################################################
##GLOBAL VARIABLE
#total_pnl_before_commission_dist
#is_print = FALSE
#is_plot = FALSE

#"contract_date" must be a trading day (included in "data"), "settlement_date" may or may not be a trading day 
#"asian option", the final settlement price is the arithmetic or geometric average between "contract_date" and "settlement_date", delta shall be close to zero on "settlement_date"
#	|<-------------number_of_volatility_days------------->|<--------------number_of_volatility_days------------->|
#	|													  |			   |<--------number_of_trading_days--------->|
#	|													  |			   |<-----------tDays---------->|			 |
#	|													  |<----- arithmetic or geometric mean ---->|< delta->0 >|
#	|													  |			   |							|		     |
#													  contract	  contract+1					settlement	   final
#														 S0			  S0+1										

#------------------------------------------- select the data between valuation period -------------------------------------------
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
valuation_period = (data$date>=contract_date) & (data$date<=settlement_date)

#first trading day after settlement day, close all hedging position on this day, e.g., trade on open price
index = which(valuation_period==TRUE)
if (tail(index, n=1) == dim(data)[1]) {
#	if the last date in the "data" is on the same day as or earlier than the settlement_date, then hedging is still in process
#	e.g., if settlement_date is 2014-07-04, and same as last date in the "data", then the position will be cleared on 2014-07-05
#	i.e., we need to wait until the end of 2014-07-05, to know the final hedging result, final_date = 2014-07-05
	print(paste("contract date =", contract_date, "|", "settlement date =", settlement_date, "|", "position has NOT expired yet, hedging in process"))
	is_position_expired = FALSE	
#	***may cause delay in processing***, use library(WindR) function to obtain trading days between "contract_date" and "settlement_date", "+ 1" for "final_date"
	number_of_volatility_days = as.numeric(w.tdayscount(contract_date, settlement_date, TradingCalendar=exchange)$Data[2]) + 1
} else {
	print(paste("contract date =", contract_date, "|", "settlement date =", settlement_date, "|", "position has expired"))
	is_position_expired = TRUE
#	save all valuation data into a new dataset, final_date is one trading day after settlement_date, i.e., close all hedging position on final_date
	valuation_period[index[length(index)]+1] = TRUE
#	for expired position, when calculating realized volatility, need to include the "final_date", since remaining hedging position is traded out on that day
	number_of_volatility_days = sum(valuation_period)
}
valuation_data = data[valuation_period,]

#------------------------------------------- variables used in later program -------------------------------------------
N = dim(valuation_data)[1]
#the contract date is not included in the agreement period, if contract day is on Friday, then agreement period includs weekends
agreement_period = settlement_date - contract_date
#there is no trading activities on the contract date (therefore - 1) or on weekends
number_of_trading_days = number_of_volatility_days - 1

############################################################################################################################################################
# TODO # volatility model for both "pricing" and "hedging"
############################################################################################################################################################
#------------------------------------------- realized volatility vector -------------------------------------------
#prepare the data for calculation of realized volatility, use x days trading data before valuation_data, where x = length(index)
previous_volatility_period = rep(FALSE, dim(data)[1])
previous_volatility_index = index[1] - rep((number_of_volatility_days-1):1)
#if there is enough data points for realized volatility calculation, otherwise, use valuation_data (this does not make sense in real situation)
if (head(previous_volatility_index, n=1) > 0) {	
#	"realized_volatility_data" has 1 less entry than "valuation_data"
	previous_volatility_period[previous_volatility_index] = TRUE
	previous_volatility_data = data[previous_volatility_period, ] 
} else {
	readline(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> realized_volatility_data is not ready, use valuation_data <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")	
	previous_volatility_data = valuation_data
}

#calculate realized volatility, log return = ln[P(t)/p(t-1)], the calculation includes both contract date and final date
#length(log_return) = length(previous_volatility_data$settlement) + length(valuation_data$settlement) - 1
log_return = diff(log(c(previous_volatility_data$settlement, valuation_data$settlement)), lag=1)

#number of log_return points used for realized_volatility calculation is 1 less than valuation_data$settlement point
#(log_return window) + length(valuation_data$settlement) = length(log_return) + 1
#realized_volatility_vec = sqrt(252 * apply(embed(log_return^2, (length(valuation_data$settlement)-1)), 1, mean))
#x = 1:10
#element = 3
#embed(x, length(x) - element + 1)
realized_volatility_vec = sqrt(252 * apply(embed(log_return^2, (length(log_return)-dim(valuation_data)[1]+1)), 1, mean))

#first element of "realized_volatility_vec" is used for pricing_volatility, on the pricing day, "previous_realized_volatility = head(realized_volatility_vec, n=1)" is the best estimation
if (is_realized_volatility_df) {
	pricing_volatility = pricing_volatility_hist_weight * (hist_realized_volatility_mean + pricing_volatility_hist_sd_z * hist_realized_volatility_sd) + (1 - pricing_volatility_hist_weight) * head(realized_volatility_vec, n=1)
} else {
	pricing_volatility = head(realized_volatility_vec, n=1)
}

#last element is the same as realized volatility during the protection period
realized_volatility = tail(realized_volatility_vec, n=1)

#------------------------------------------- Shapiro-Wilk test for Normality -------------------------------------------
#This p-value tells you what the chances are that the sample comes from a normal distribution. 
#The lower this value, the smaller the chance. Statisticians typically use a value of 0.05 as a cutoff, 
#so when the p-value is lower than 0.05, you can conclude that the sample deviates from normality.
#http://en.wikipedia.org/wiki/Normality_test
if (length(log_return) >= 3) {
#	shapiro.test(log_return), sample size must be between 3 and 5000
	normality_test = shapiro.test(log_return)
}

#------------------------------- volatility model -------------------------------
#volatility used for pricing and hedging are not the same, pricing volatility shall be higher than hedging volatility
pricing_volatility_spread = c(0.0, -0.0)
pricing_volatility_adj = pricing_volatility_ratio * pricing_volatility
pricing_volatility_adj_vec = pricing_volatility_adj + pricing_volatility_spread
		
#hedging volatility mode
if (hedging_volatility_mode == "fixed") {
	hedging_volatility = hedging_volatility_ratio * matrix(rep(pricing_volatility, 2), nrow=2, byrow=TRUE)	
} else if (hedging_volatility_mode == "dynamic") {
	hedging_volatility = hedging_volatility_ratio * matrix(rep(realized_volatility_vec, 2), nrow=2, byrow=TRUE)
}

############################################################################################################################################################
# TODO # main siumulation loop for both call and put
############################################################################################################################################################
#for both call and put spread, the theoretical value of the 1st option needs to be higher 
option_type = c("call", "put")
par(mfcol=c(2,2))
#used for calculate multiple runs, and generate distributions graph about "pricing_spread_value" and "total_pnl_before_commission"
pricing_spread_value_call_put = vector()
total_pnl_before_commission_call_put = data.frame(valuation_data$date, valuation_data$settlement)

for (k in 1:length(option_type)) {
#------------------------------- calculate theoretical values -------------------------------
#	create strike price vector based on option type
	strike = round((1 + ifelse(option_type[k] == "call", 1, -1) * strike_percentage) * valuation_data[1,]$settlement, strike_rounding_digit)

#	days to maturity at the end of that day, i.e., days_to_maturity = 0 for the settlement day
	days_to_maturity = as.numeric(settlement_date - valuation_data$date)
#	calculate European option theoretical value and greeks			
#	TODO		
	if (option_style == "european") {
#		EuropeanOption(type, underlying, strike, dividendYield, riskFreeRate, maturity, volatility)	
		option1 = mapply(EuropeanOption, option_type[k], valuation_data$settlement, strike[1], 0, risk_free_interest, days_to_maturity/365, hedging_volatility[1,], USE.NAMES=FALSE)
		option2 = mapply(EuropeanOption, option_type[k], valuation_data$settlement, strike[2], 0, risk_free_interest, days_to_maturity/365, hedging_volatility[2,], USE.NAMES=FALSE)	
		
		
		
	} else if (option_style == "arithmetic") {					
#		AsianOption(averageType="geometric", type="call", underlying=100, strike=100, dividendYield=0.0, riskFreeRate=0.0, maturity=0.25, volatility=0.5)
		option1 = mapply(AsianOption, "geometric", option_type[k], valuation_data$settlement, strike[1], 0, risk_free_interest, days_to_maturity/365, hedging_volatility[1,], USE.NAMES=FALSE)
		option2 = mapply(AsianOption, "geometric", option_type[k], valuation_data$settlement, strike[2], 0, risk_free_interest, days_to_maturity/365, hedging_volatility[2,], USE.NAMES=FALSE)	 
		
##		sf@@
#		for (gg in 1:45) {
#			print(gg)
#			option1 = OptionMC("arithmetic", option_type[k], valuation_data$settlement, strike[1], risk_free_interest, 0, 45, hedging_volatility[1,], nSims)
#		}
#		
#		for (gg in 1:45) {
#			print(gg)
#			option1 = AsianOption("geometric", option_type[k], valuation_data$settlement[gg], strike[1], 0, risk_free_interest, days_to_maturity/365, hedging_volatility[1,])
#		}
		
		
		
	} else if (option_style == "geometric") {
#		AsianOption(averageType="geometric", type="call", underlying=100, strike=100, dividendYield=0.0, riskFreeRate=0.0, maturity=0.25, volatility=0.5)
		option1 = mapply(AsianOption, "geometric", option_type[k], valuation_data$settlement, strike[1], 0, risk_free_interest, days_to_maturity/365, hedging_volatility[1,], USE.NAMES=FALSE)
		option2 = mapply(AsianOption, "geometric", option_type[k], valuation_data$settlement, strike[2], 0, risk_free_interest, days_to_maturity/365, hedging_volatility[2,], USE.NAMES=FALSE)		
	} else {
		stop(paste("unrecognized option_style = ", option_style, sep=""))
	}
	
#	save the results to valuation_data dataset	
	valuation_data$value1 = as.vector(unlist(option1["value",]))
	valuation_data$delta1 = as.vector(unlist(option1["delta",]))
	valuation_data$value2 = as.vector(unlist(option2["value",]))
	valuation_data$delta2 = as.vector(unlist(option2["delta",]))	
	valuation_data$spread_value = valuation_data$value1 - valuation_data$value2
	valuation_data$spread_delta = valuation_data$delta1 - valuation_data$delta2	
	valuation_data$days_to_maturity = days_to_maturity	
#	the option spread position delta, calculated at the end of the day, i.e., based on settlement prices, negative means we will be on the sell side
	valuation_data$position_delta = - valuation_data$spread_delta * position_size
	
#------------------------------- hedging position and delta -------------------------------
#	required hedging position delta, to hedge the detla calculated based on previous trading's settlement price 
#	i.e., hedge (T-1) delta calculated based on (T-1) settlement price	
	if (N == 1) {
		valuation_data$hedging_delta = 0
	} else {
		valuation_data$hedging_delta = c(0, -valuation_data[seq(1,N-1,1),]$position_delta)
	}	
	
#	when position size is greater than 1, we round the hedging position, i.e., number futures traded to hedge the delta 
	if (abs(position_size) != 1) {		
		valuation_data$hedging_delta = round(valuation_data$hedging_delta)
	}
	
	if (is_position_expired) {
#		the otc position expires on the previous trading day, therefore, the hedging_delta value of the last entry of valuation_data is 0
		valuation_data[N,]$hedging_delta = 0		
	} else {
	}	
	
#	actual trading required in that day, i.e., buy or sell future contracts to achieve hedging position, "c(0, diff(c(0),n=1))" return "[1] 0"
	valuation_data$hedging_delta_change = c(0, diff(valuation_data$hedging_delta, n=1))
		
############################################################################################################################################################
# TODO # position initial pricing based on pricing volatility, i.e., "pricing_volatility_adj_vec"
############################################################################################################################################################	
#	total premium collected from the buyer, which is determined on the contract day, use different volatility, i.e., pricing volatility v.s. hedging volatility
#	use valuation_data[1,]$settlement (i.e., contract_date) for pricing the option, note: there is no hedging on contract_date
	days_to_maturity = as.numeric(settlement_date - valuation_data[1,]$date)		
	if (option_style == "european") {
		pricing_option1 = EuropeanOption(option_type[k], valuation_data[1,]$settlement, strike[1], 0, risk_free_interest, days_to_maturity/365, pricing_volatility_adj_vec[1])
		pricing_option2 = EuropeanOption(option_type[k], valuation_data[1,]$settlement, strike[2], 0, risk_free_interest, days_to_maturity/365, pricing_volatility_adj_vec[2])
	} else if ((option_style == "arithmetic") | (option_style == "geometric")) {
		if (calculation_mode == "analytical") {
#			analytical method for "geometric" average option, use this to approximate the price of an "arithmetic" average option
			pricing_option1 = AsianOption("geometric", option_type[k], valuation_data[1,]$settlement, strike[1], 0, risk_free_interest, days_to_maturity/365, pricing_volatility_adj_vec[1])
			pricing_option2 = AsianOption("geometric", option_type[k], valuation_data[1,]$settlement, strike[2], 0, risk_free_interest, days_to_maturity/365, pricing_volatility_adj_vec[2])			
		} else if (calculation_mode == "numerical") {
#			numerical method
#			OptionMC(style, S, X, r, q, cDays, volatility, nSims, minSteps=500, rValue=c("path","sigma","call_value","call_delta","put_value","put_delta")) 	
#			OptionMC_vec(style, S, X, r, q, cDays, volatility, nSims, minSteps=500, avgRuns=100, rValue=c("sigma","call_value","call_delta","put_value","put_delta"))
			optionMC_vec1 = OptionMC_vec(option_style, valuation_data[1,]$settlement, strike[1], risk_free_interest, 0, days_to_maturity, pricing_volatility_adj_vec[1], nSims, minSteps, avgRuns, paste(option_type[k],"value",sep="_"))
#			optionMC_vec2 = OptionMC_vec(option_style, valuation_data[1,]$settlement, strike[2], risk_free_interest, 0, days_to_maturity, pricing_volatility_adj_vec[2], nSims, minSteps, avgRuns, paste(option_type[k],"value",sep="_"))
			pricing_option1 = list(value=mean(optionMC_vec1[,paste(option_type[k],"value",sep="_")]))
#			pricing_option2 = list(value=mean(optionMC_vec2[,paste(option_type[k],"value",sep="_")]))
			pricing_option2 = list(value=0)			
		}
	} else {
		stop(paste("unrecognized option_style = ", option_style, sep=""))
	}
#	option spread value, "pricing_option2$value" maybe close to zero
	pricing_spread_value = pricing_option1$value - pricing_option2$value
	
#	spread value for both call and put on the pricing day, i.e., contract_date
	pricing_spread_value_call_put = cbind(pricing_spread_value_call_put, pricing_spread_value)
	total_premium = position_size * multiplier * pricing_spread_value
	
############################################################################################################################################################
# TODO # hedging process and option payout at expiration
############################################################################################################################################################		
#	calculate the PnL from hedging position, long positioin is cash outflow (positive), short position is cash inflow (negative)
#	hedging position can be adjusted on 1 price, e.g., open or close, or throughout the day
#	average_hedging_price = valuation_data$open
	average_hedging_price = (valuation_data$open + valuation_data$high + valuation_data$low + valuation_data$close) / 4
	
#	close all the hedging position on "open" on the following day of the settlement date, i.e., no need to close the hedging position throughout the day
#	average_hedging_price[length(average_hedging_price)] = tail(valuation_data$open, n=1)
#	cash flow needs to time the multiplier, in order to get the actual RMB value
	hedging_cash_flow = valuation_data$hedging_delta_change * multiplier * average_hedging_price
	
#	hedging_delta is the net long or short position, we can get the mark-to-market value from here	
#	hedging_cash_flow[i] (+) is a long position; hedging_delta (+) is a long position; 
#	hedging_cash_flow[i] (-) is a short position; hedging_delta (-) is a short position; 	
#	hedging pnl is valued at daily settlement price	
	hedging_pnl_vector = valuation_data$hedging_delta * multiplier * valuation_data$settlement - cumsum(hedging_cash_flow)		

#	calculate the payout (if there is any) to option spread buyer, payout is a non-negative value
#	for call, payout is great than 0 (if there is any), max payout for call spread is (strike[2] - strike[1])
#	for put, payout is great than 0 (if there is any), max payout for put spread is (strike[1] - strike[2])
	if (option_style == "european") {
		St_vec = valuation_data$settlement
	} else if (option_style == "arithmetic") {
		St_vec = cumsum(valuation_data$settlement) / seq(1:length(valuation_data$settlement))
	} else if (option_style == "geometric") {
#		geometric mean of x is defined as "exp(mean(log(x)))"
		St_vec = exp(cumsum(log(valuation_data$settlement)) / seq(1:length(valuation_data$settlement)))		
	} else {
		stop(paste("unrecognized option_style = ", option_style, sep=""))
	}
	
#	"St_vec" is different depending on option_style
	if (option_type[k] == "call") {
		payout_vector = position_size * multiplier * pmin(pmax(St_vec-strike[1], 0), strike[2]-strike[1])
	} else {
		payout_vector = position_size * multiplier * pmin(pmax(strike[1]-St_vec, 0), strike[1]-strike[2])
	}
	
#	payout on the contract day is 0, i.e., customer can not early exercise on that day, even for ITM position
	payout_vector[1] = 0
#	if position has expired, then payout on the final date is the same as the payout on the settlement date	
	if (is_position_expired) {
		payout_vector[N] = payout_vector[N-1]	
	} else {		
	}
	
#	premium collected minus the hedging PnL and payout (if there is any) is the total PnL
#	total PnL before commission in a vector form
	total_pnl_before_commission_vector = total_premium + hedging_pnl_vector - payout_vector
	
#	save to the valuation_data dataset
	valuation_data$hedging_pnl = hedging_pnl_vector
	valuation_data$payout = payout_vector
	valuation_data$total_pnl_before_commission = total_pnl_before_commission_vector	
	
#	some statistics about the hedging results
	total_pnl_before_commission_call_put = cbind(total_pnl_before_commission_call_put, total_pnl_before_commission_vector)	
#	negative_pnl_before_commission = length(which(total_pnl_before_commission_vector <= 0))

############################################################################################################################################################
# TODO # for both call and put, 1) print result; 2) plot "Daily Settlement Price" v.s. "Total PnL Before Commission"
############################################################################################################################################################
#	total hedging position delta, this value needs to be 0, in order for hedging_pnl to be valid
#	total_hedging_delta = sum(valuation_data$hedging_delta_change)
#	commission related issue
	total_contract_traded = sum(abs(valuation_data$hedging_delta_change)) 
	total_commission = total_contract_traded * commission_per_contract
	
	if (is_print) {
		print(paste("####################################################", "option style =", option_style, "|", "option type =", option_type[k], "####################################################"))
#		the first entry values are calculated based on hedging_volatility, not pricing_volatility		
		if (is_position_expired) {
			print(valuation_data)
		} else {
#			add a line for the "next_trading_day", i.e., how to re-balance the position on the "next_trading_day"
			current_trading_day = tail(valuation_data, n=1)		
			next_trading_day = current_trading_day
			next_trading_day[,] = "__"
			next_trading_day$contract = current_trading_day$contract
			next_trading_day$date = as.Date(w.tdaysoffset(1, current_trading_day$date, TradingCalendar=exchange)$Data[1,1])
			next_trading_day$days_to_maturity = as.numeric(settlement_date - next_trading_day$date)
			next_trading_day$hedging_delta = - round(current_trading_day$position_delta)
			next_trading_day$hedging_delta_change = next_trading_day$hedging_delta - current_trading_day$hedging_delta		
#			print next_trading_day information with "valuation_data"
			valuation_data_current_trading_day = rbind(valuation_data, next_trading_day)
			print(valuation_data_current_trading_day)				
		}		
		print(paste("--------------------------------------------------------------------------------------------------"))
		print(paste("X1 =", strike[1], "|", "X2 =", strike[2], "|", "pricing spread value =", pricing_spread_value))
		print(paste("pricing volatility", "|", "v1 =", pricing_volatility_adj_vec[1], "|", "v2 =", pricing_volatility_adj_vec[2]))
#		may use dynamic hedging_volatility		
#		print(paste("hedging volatility", "|", "v1 =", volatility_hedging[1], "|", "v2 =", volatility_hedging[2]))
		print(paste("--------------------------------------------------------------------------------------------------"))
		print(paste("total premium =", round(total_premium,2), "|", "hedging pnl =", valuation_data[N,]$hedging_pnl, "|", "payout =", valuation_data[N,]$payout))
		print(paste("total pnl before commission =", round(valuation_data[N,]$total_pnl_before_commission,2), "[", round(min(total_pnl_before_commission_vector),2), ",", round(mean(total_pnl_before_commission_vector),2), ",", round(max(total_pnl_before_commission_vector),2), "]"))
#		print(paste("total hedging delta =", total_hedging_delta))
		print(paste("total contract traded =", total_contract_traded, "|", "commission per contract =", commission_per_contract, "|", "total commission =", total_commission))
		print(paste("--------------------------------------------------------------------------------------------------"))
	}
	
#	plot "Daily Settlement Price" v.s. "Total PnL Before Commission"
	if (is_plot) {
		plot(valuation_data$date, valuation_data$settlement, xlab="", ylab="", type="b", col="black")
		mtext("Daily Settlement Price", side=2, line=2.5, col="black")
		par(new=TRUE)
		plot(valuation_data$date, valuation_data$total_pnl_before_commission, xlab="", ylab="", axes=FALSE, type="b", col="red")
		mtext("Total PnL Before Commission", side=4, line=4.5, col="red")
		axis(4, col="red", col.axis="red", las=1)
		title(paste("Daily Settlement Price v.s. Total PnL Before Commission (", option_type[k], ")", sep=""))
		par(new=FALSE)		
	}	
}

############################################################################################################################################################
# TODO # 1) print statistics; 2) plot "Daily Settlement Price" v.s. "realized volatility"
############################################################################################################################################################
if (is_print) {
	print(paste("####################################################", "statistics", "####################################################"))
	print(paste("agreement period =", agreement_period, "|", "number of trading days =", number_of_trading_days, "|", "symbol =", contract_symbol))
#	Shapiro-Wilk test for Log-Normal return
	print(paste("Shapiro-Wilk test for Log-Normal return", "|", "W =", normality_test$statistic, "|", "p-value =", normality_test$p.value))
#	pricing and realized volatility
	print(paste("pricing volatility = ", pricing_volatility, " | realized volatility [", head(valuation_data,n=1)$date, " , ", tail(valuation_data,n=1)$date, "] = ", realized_volatility, " (current) ", sep=""))
	print(paste("max =", max(realized_volatility_vec), "|", "min =", min(realized_volatility_vec), "|", "mean =", mean(realized_volatility_vec)))
	print(quantile(realized_volatility_vec))
	print(paste("--------------------------------------------------------------------------------------------------"))
	print(paste("contract date =", contract_date, "(", weekdays(contract_date), ")", "|", "settlement price =", valuation_data[1,]$settlement))	
	print(paste("settlement date =", settlement_date, "(", weekdays(settlement_date), ")", "|", "settlement price =", ifelse(is_position_expired, valuation_data[N-1,]$settlement, "N/A")))	
	print(paste("position size =", position_size, "|", "multiplier =", multiplier, "|", "notional size =", position_size * multiplier, "(tons)"))	
	print(paste("risk free interest = ", risk_free_interest, " | strike rounding digit = ", strike_rounding_digit, sep=""))	
	print(paste("--------------------------------------------------------------------------------------------------"))
}

#plot "Daily Settlement Price" v.s. "realized volatility"
if (is_plot) {
	plot(valuation_data$date, valuation_data$settlement, xlab="", ylab="", type="b", col="black")
	mtext("Daily Settlement Price", side=2, line=2.5, col="black")
	par(new=TRUE)
	plot(realized_volatility_vec, xlab="", ylab="", axes=FALSE, type="b", col="red")
	mtext("Realized Volatility", side=4, line=4.5, col="red")
	axis(4, col="red", col.axis="red", las=1)
	title(paste("Daily Settlement Price v.s. Realized Volatility (", dim(valuation_data)[1]-1, ")", sep=""))
	par(new=FALSE)
}

############################################################################################################################################################
# TODO # save result to run_result
############################################################################################################################################################
#run_result is a global variable, created in "simulation_batch.R"
mm = data.frame(period_level, contract_date, settlement_date)
#PnL, if is_position_expired == TRUE, final_date is the date clear all the position, i.e., the first trading day after settlement_date
colnames(total_pnl_before_commission_call_put) = c("final_date", "final_date_settlement", paste("pnl_", option_type, sep=""))
nn = tail(total_pnl_before_commission_call_put, n=1)
#pricing_spread_value, calculated based on contract_date settlement price
pricing_spread_value_call_put = cbind(pricing_spread_value_call_put, valuation_data[1,]$settlement)
colnames(pricing_spread_value_call_put) = c(paste("pricing_spread_value_", option_type, sep=""), "contract_date_settlement")
#save the value to the global variable, defined in "simulation_batch.R"
run_result = rbind(run_result, cbind(mm, nn, pricing_volatility, pricing_volatility_adj, realized_volatility, number_of_volatility_days, is_position_expired, pricing_spread_value_call_put))


#w.tdayscount(contract_date, settlement_date, TradingCalendar=exchange)

#itm_ness1 = (strike[1] / valuation_data[i,]$settlement) - 1
#itm_ness2 = (strike[2] / valuation_data[i,]$settlement) - 1
#
#volatility_slope = 0
#
#volatility[1] = atm_volatility + abs(itm_ness1) * volatility_slope
#volatility[2] = atm_volatility + abs(itm_ness2) * volatility_slope
#
#print(paste(volatility[1], volatility[2]))

##	valuation_data$spread_delta = (valuation_data$delta1 - valuation_data$delta2) / seq(1:dim(valuation_data)[1])
#	
#	aa = mapply(EuropeanOption, option_type[k], valuation_data$settlement, strike[1], 0, risk_free_interest, days_to_maturity/365, hedging_volatility[1,], USE.NAMES=FALSE)
#	bb = mapply(EuropeanOption, option_type[k], valuation_data$settlement, strike[2], 0, risk_free_interest, days_to_maturity/365, hedging_volatility[2,], USE.NAMES=FALSE)	
#	
#	cc = as.vector(unlist(aa["delta",]))
#	dd = as.vector(unlist(bb["delta",]))
#	
###	valuation_data$spread_delta = (cc - dd) / seq(1:dim(valuation_data)[1])
##	
#	valuation_data$spread_delta = (cc - dd)