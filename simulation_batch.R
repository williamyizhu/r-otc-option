############################################################################################################################################################
# TODO # Add comment
############################################################################################################################################################

############################################################################################################################################################
# TODO # clean running evnironment and load packages
############################################################################################################################################################
#remove all variables except "run_result_summary"
#rm(list=ls(all=TRUE)) 
if (any(ls(all=TRUE)=="run_result_summary")) {
#	The %in% operator tells which elements are among the ones to remove
	rm(list=ls(all=TRUE)[! ls(all=TRUE) %in% c("run_result_summary")])
} else {
	rm(list=ls(all=TRUE)) 
	run_result_summary = list()	
}

#print options
options(width=438L)
library("RQuantLib")
source(paste(getwd(), "/option_mc.R", sep=""), echo=FALSE, encoding="GBK")

############################################################################################################################################################
# TODO # create data set under Wind (www.wind.com.cn)
############################################################################################################################################################
exchange = "DCE"
underlying = "M"
#[year + delivery month]
contract_year_month = "1409"
contract_symbol = toupper(paste(exchange, underlying, contract_year_month, sep="."))
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

############################################################################################################################################################
# TODO # simulation parameters, GLOBAL VARIABLE otc product specifics
############################################################################################################################################################
#actual trading size, e.g., DCE soybean meal, position_size = 500 is equivalent to 5,000 tons
position_size = 100
multiplier = 10
commission_per_contract = 5
#strike price with respect to the contract_date settlement price, only sell OTM options
strike_percentage = c(0.03, 0.18)
strike_rounding_digit = 0
#higher risk-free interest rate, increases the value of call, decreases the value of put
risk_free_interest = 0.0

#"simulation.R", pricing_volatility = pricing_volatility_hist_pct * (hist_realized_volatility_mean + pricing_volatility_hist_sd_z * hist_realized_volatility_sd) + (1 - pricing_volatility_hist_pct) * head(realized_volatility_vec, n=1)
pricing_volatility_hist_weight = 0.5
pricing_volatility_hist_sd_z = 1.96
#"simulation.R", pricing_volatility_adj = pricing_volatility_ratio * pricing_volatility
pricing_volatility_ratio = 1.8

#"asian" option is cheaper than "european" option, check the boundary conditions
#NOTE: "asian" option is path-dependant, run-time pricing of asian option may not be accurate, since it does not take exiting fixing into consideration 
option_style = "european"
#option_style = "arithmetic"
#option_style = "geometric"
#if "calculation_mode = "analytical", then use "geometric" average option (analytical method) to approximate the price of "arithmetic" average option (numerical method)
calculation_mode = "analytical"
#calculation_mode = "numerical"
nSims = 50
minSteps = 500
avgRuns = 100
#choice between same "hedging_volatility" for entire simulation, or dynamic "hedging_volatility" which based on most recent data (Baysian)
hedging_volatility_mode = "fixed"
#hedging_volatility_mode = "dynamic"
#"hedging_volatility_ratio < 1" has the same effect of over-hedging, i.e., tune more towards expiration
hedging_volatility_ratio = 1.2

############################################################################################################################################################
# TODO # create "contract_settlement_date", which contains the period for simulation runs
############################################################################################################################################################
#number of days covered under agreement
days = 60
#use the most actively traded month for pricing and hedging
#number of years used for simulation
sim_year_number = 5
#number of month before the delivery month, i.e., the last day of sim period of each year
sim_back_month = 2
#number of days before the last day of sim period of each year
sim_back_days = 120 + days
#sim_back_days = 60 + days

#number of runs to generate the distribution for both call and put PnL
runs = sim_back_days - days + 1
runs = 1

#the last day of each month
month_end = seq(as.Date(paste(year_end-sim_year_number+1, "-02-01", sep="")), length=12*sim_year_number, by="1 month") - 1
index = rep(FALSE, 12)

#last month of a year use "delivery_month" contract as pricing and hedging contract, which maybe in the previous year
settlement_month = as.numeric(delivery_month) - sim_back_month + 12 * ifelse(as.numeric(delivery_month)-sim_back_month<=0, 1, 0)
index[settlement_month] = TRUE
month_index = rep(index, sim_year_number)
settlement_date_end = as.Date(month_end[month_index])

#"contract_date_start" is calculated as number of days backed from "settlement_date_end" 
contract_date_start = settlement_date_end - sim_back_days

#only include the "contract_date_start" is before the current Sys.Date(), i.e., at least there is some data points for analysis
#"contract_date_start and" "settlement_date_end" shall be before the "contract_year_month" expires, i.e., can not base on an already expired contract
ii = (contract_date_start < Sys.Date()) & (contract_date_start < as.Date(paste("20", substr(contract_year_month,1,2), "-", substr(contract_year_month,3,4), "-01", sep="")))
contract_date_start = contract_date_start[ii]
settlement_date_end = settlement_date_end[ii]

print("---------------------------------------------------------------------------")
print(paste("contract_date_start:", paste(contract_date_start, collapse=" | ")))
print(paste("settlement_date_end:", paste(settlement_date_end, collapse=" | ")))
print("---------------------------------------------------------------------------")

#contract_date_start = as.Date(c("2012-03-22"))
#settlement_date_end = as.Date(c("2014-08-31"))	

contract_date_start = as.Date(c("2014-06-01"))
settlement_date_end = as.Date(c("2014-08-31"))	

#contract_date_start = as.Date(c("2014-09-05"))
#settlement_date_end = as.Date(c("2014-10-31"))	

#contract_date_start = as.Date(c("2014-08-01", "2014-08-04", "2014-08-05", "2014-08-06", "2014-08-07", "2014-08-08", "2014-08-11", "2014-08-12", "2014-08-13", "2014-08-14", "2014-08-15"))
#settlement_date_end = as.Date(c("2014-10-31", "2014-10-31", "2014-10-31", "2014-10-31", "2014-10-31", "2014-10-31", "2014-10-31", "2014-10-31", "2014-10-31", "2014-10-31", "2014-10-31"))	

#create a contract settlement period level vector, to explore volatility change during different period
period_level_vec = paste("[", contract_date_start, ",", settlement_date_end, "]", sep="")

#generate a data frame for contract_date and settlement_date 
contract_settlement_date = data.frame()
for (i in 1:length(contract_date_start)) {
#	start_end_days = as.numeric(settlement_date_end[i] - contract_date_start[i])
#	from "contract_date_start" to "settlement_date_end", including "contract_date_start"
	for (j in 0:(runs-1)) {
#		check if the valuation window is before the "settlement_date_end"
#		contract_date_start[i] must be before or at last element of data, i.e., at least there is 1 data point
		if ((contract_date_start[i]+j <= tail(data$date,n=1)) & (contract_date_start[i]+j+days <= settlement_date_end[i])) {
			temp = data.frame(period_level_vec[i], contract_date_start[i] + j, contract_date_start[i] + j + days)
			contract_settlement_date = rbind(contract_settlement_date, temp)
		}		
	}	
}
#return an error if dim(contract_settlement_date)[1] = 0
colnames(contract_settlement_date) = c("period_level", "contract_date", "settlement_date")

############################################################################################################################################################
# TODO # main simulation loop
############################################################################################################################################################
#data frame for store total PnL before commission for each run
run_result = data.frame()

#print and plot parameter, if there is only 1 run, then print and plot
if (dim(contract_settlement_date)[1] == 1) {
	is_print = TRUE
	is_plot = TRUE
} else {
	is_print = FALSE
	is_plot = FALSE
}

#read saved file for historical realized volatility stats, e.g., mean, sd, etc.
fpath = paste(getwd(), "/pricing/", paste("realized_volatility", paste(exchange, underlying, days, sep="."), sep="_"), ".txt", sep="")
if (file.exists(fpath)) {
	realized_volatility_df = read.csv(fpath, header=TRUE, sep=",")
	is_realized_volatility_df = TRUE
} else {	
#	stop(paste("\"", fpath, "\" does not exist!"))
	readline(prompt=paste("\"", fpath, "\" does not exist!", sep=""))
	is_realized_volatility_df = FALSE
}

#call main simulation script ["C:/Users/William Yizhu/Documents/workspace/r-option/simulation.R"]
for (i in 1:dim(contract_settlement_date)[1]) {
#	class label for different runs, to explore if there is any significance among different time period
	period_level = contract_settlement_date[i,]$period_level
#	use settlement price on contract date to calculate the option initial value, which is sold to customer
#	the contract date must have a settlement price, i.e., a trading day
#	use settlement price on settlement date to calculate the payout to customer
#	let "final_date" as the first trading day after settlement day, close all hedging position on this day	
	contract_date = contract_settlement_date[i,]$contract_date
	settlement_date = contract_settlement_date[i,]$settlement_date
	
#	get the historical realized volatility for this settlement month
	if (is_realized_volatility_df) {
		settlement_date_month = as.numeric(unlist(strsplit(as.character(settlement_date), "-"))[2])
		hist_realized_volatility_mean = realized_volatility_df[settlement_date_month,]$mean	
		hist_realized_volatility_sd = realized_volatility_df[settlement_date_month,]$sd			
	} else {		
	}
	
#	call "simulation.R" to run the simulation, i.e., in a batch model
#	source(paste(getwd(), "/simulation.R", sep=""), echo=FALSE, encoding="GBK")	
	result = tryCatch({
				source(paste(getwd(), "/simulation.R", sep=""), echo=FALSE, encoding="GBK")
			}, warning = function(war) {
				print(war)
			}, error = function(err) {
				print(err)				
			}, finally = {				
			})	
}

############################################################################################################################################################
# TODO # modify run_result
############################################################################################################################################################
#classify with contract_date, i.e., year, month, day
aa = strsplit(as.character(run_result$contract_date), "-")
bb = as.matrix(aa)
cc = t(apply(bb, 1, unlist))
contract_date.df = data.frame(cc)
colnames(contract_date.df) = paste("contract_date_", c("year", "month", "day"), sep="")
run_result = cbind(run_result, contract_date.df)

#classify with settlement_date, i.e., year, month, day
aa = strsplit(as.character(run_result$settlement_date), "-")
bb = as.matrix(aa)
cc = t(apply(bb, 1, unlist))
settlement_date.df = data.frame(cc)
colnames(settlement_date.df) = paste("settlement_date_", c("year", "month", "day"), sep="")
run_result = cbind(run_result, settlement_date.df)

#simulation result of expired position, i.e., exclude "on-going" position 
run_result_expired = run_result[run_result$is_position_expired,]
	
############################################################################################################################################################
# TODO # display run_result 
############################################################################################################################################################
if (dim(contract_settlement_date)[1] != 1) {	
	print("------------------------------------------- run result summary -------------------------------------------")
	print(run_result)		
	
#	create multiple charts
	par(mfcol=c(2,4))
	
#	---------------------------------------------------- plot 1 ----------------------------------------------------
#	run_result$is_position_expired == TRUE, histgram of the total_pnl_before_commission	
	bn = min(max(length(run_result_expired$pnl_call), 30), 100)
	hh = hist(run_result_expired$pnl_call, breaks=bn, plot=FALSE)	
	bar_color = rep("white", length(hh$breaks))
	bar_color[hh$breaks < 0] = "red"	
	hist(run_result_expired$pnl_call, breaks=bn, col=bar_color, xlab="Total PnL Before Commission (Call)", main="")
	
#	average PnL for both call, put and combined, only for "expired" runs
	pnl_mean = mean(c(run_result_expired$pnl_call, run_result_expired$pnl_put)) / (position_size * multiplier)
	pnl_call_mean = mean(run_result_expired$pnl_call) / (position_size * multiplier)
	pnl_put_mean = mean(run_result_expired$pnl_put)	/ (position_size * multiplier)	
	pnl_min = min(c(run_result_expired$pnl_call, run_result_expired$pnl_put)) / (position_size * multiplier)
	pnl_call_min = min(run_result_expired$pnl_call) / (position_size * multiplier)
	pnl_put_min = min(run_result_expired$pnl_put)	/ (position_size * multiplier)	
	title(paste("AvgPnL(per)=", round(pnl_mean,2), " (Call=", round(pnl_call_mean,2), " | Put=", round(pnl_put_mean,2), ")\nMinPnL(per)=", round(pnl_min,2), " (Call=", round(pnl_call_min,2), " | Put=", round(pnl_put_min,2), ")", sep=""))	

#	display average PnL for both call and put
	aa = round(tapply((run_result_expired$pnl_call+run_result_expired$pnl_put)/2, run_result_expired$period_level, mean) / (position_size * multiplier), 2)
	bb = round(tapply(run_result_expired$pnl_call, run_result_expired$period_level, mean) / (position_size * multiplier), 2)
	cc = round(tapply(run_result_expired$pnl_put, run_result_expired$period_level, mean) / (position_size * multiplier), 2)	
	gg = paste("Avg=", format(aa,nsmall=2), ", c=", format(bb,nsmall=2), ", p=", format(cc,nsmall=2), sep="")	
	legend("topright", gg, pch=seq(1:length(gg)))
	
#	display min PnL for both call and put	
	aa = round(tapply((run_result_expired$pnl_call+run_result_expired$pnl_put)/2, run_result_expired$period_level, min) / (position_size * multiplier), 2)
	bb = round(tapply(run_result_expired$pnl_call, run_result_expired$period_level, min) / (position_size * multiplier), 2)
	cc = round(tapply(run_result_expired$pnl_put, run_result_expired$period_level, min) / (position_size * multiplier), 2)	
	gg = paste("Min=", format(aa,nsmall=2), ", c=", format(bb,nsmall=2), ", p=", format(cc,nsmall=2), sep="")	
	legend("right", gg, pch=seq(1:length(gg)))		

#	---------------------------------------------------- plot 2 ----------------------------------------------------	
#	same histogram plot for put
	bn = min(max(length(run_result_expired$pnl_put), 30), 100)
	hh = hist(run_result_expired$pnl_put, breaks=bn, plot=FALSE)
	bar_color = rep("white", length(hh$breaks))
	bar_color[hh$breaks < 0] = "red"
	hist(run_result_expired$pnl_put, breaks=bn, col=bar_color, xlab="Total PnL Before Commission (Put)", main="")	
	
#	calculate the fail rate, i.e., the percentage of runs with a negative overall pnl
#	fail_rate = (length(which(run_result_expired$pnl_call < 0)) + length(which(run_result_expired$pnl_put < 0))) / length(c(run_result_expired$pnl_call, run_result_expired$pnl_put))
	fail_rate_call = length(which(run_result_expired$pnl_call < 0)) / length(run_result_expired$pnl_call)	
	fail_rate_put = length(which(run_result_expired$pnl_put < 0)) / length(run_result_expired$pnl_put)
	fail_rate = (fail_rate_call + fail_rate_put) / 2
	
#	PnL ratio for call, put and combined
	pnl_call_positive = sum(run_result_expired$pnl_call[which(run_result_expired$pnl_call >= 0)])
	pnl_call_negative = sum(run_result_expired$pnl_call[which(run_result_expired$pnl_call < 0)])
	pnl_put_positive = sum(run_result_expired$pnl_put[which(run_result_expired$pnl_put >= 0)])
	pnl_put_negative = sum(run_result_expired$pnl_put[which(run_result_expired$pnl_put < 0)])	
	pnl_ratio = abs((pnl_call_positive + pnl_put_positive) / (pnl_call_negative + pnl_put_negative))
	pnl_ratio_call = abs(pnl_call_positive / pnl_call_negative)
	pnl_ratio_put = abs(pnl_put_positive / pnl_put_negative)		
	tt = paste("FailRate=", round(fail_rate*100,2), "% (Call=", round(fail_rate_call*100,2), "%", " | Put=", round(fail_rate_put*100,2), "%", ")\nPnlRatio=", round(pnl_ratio,2), " (Call=", round(pnl_ratio_call,2), " | Put=", round(pnl_ratio_put,2), ")", sep="")
	title(tt)
	
#	PnL ratio for call, put and combined, for different "period_level", e.g., "settlement_date_year" 
	fail_rate_sub_call = round(tapply(run_result_expired$pnl_call, run_result_expired$period_level, function(x){length(which(x<0))/length(x)}) * 100, 2)
	fail_rate_sub_put = round(tapply(run_result_expired$pnl_put, run_result_expired$period_level, function(x){length(which(x<0))/length(x)}) * 100, 2)	
	fail_rate_sub = round((fail_rate_sub_call + fail_rate_sub_put) / 2, 2)	
	gg = paste("FR=", format(fail_rate_sub,nsmall=2), "%, c=", format(fail_rate_sub_call,nsmall=2), "%, p=", format(fail_rate_sub_put,nsmall=2), "%", sep="")
	legend("topright", gg, pch=seq(1:length(gg)))	
	
#	---------------------------------------------------- plot 3 ----------------------------------------------------	
#	density plot for "pricing_volatility", "pricing_volatility_adj", "realized_volatility", only "is_position_expired=TRUE" runs
	pricing_volatility_density = density(run_result_expired$pricing_volatility)
	pricing_volatility_adj_density = density(run_result_expired$pricing_volatility_adj)
	realized_volatility_density = density(run_result_expired$realized_volatility)	
	xlim = c(min(pricing_volatility_density$x, pricing_volatility_adj_density$x, realized_volatility_density$x), max(pricing_volatility_density$x, pricing_volatility_adj_density$x, realized_volatility_density$x))
	ylim = c(0, max(pricing_volatility_density$y, pricing_volatility_adj_density$y, realized_volatility_density$y))	
	plot(pricing_volatility_density$x, pricing_volatility_density$y, xlim=xlim, ylim=ylim, col="blue", type="l", xlab="Volatility", ylab="Density")
	lines(pricing_volatility_adj_density$x, pricing_volatility_adj_density$y, xlim=xlim, ylim=ylim, col="red")
	lines(realized_volatility_density$x, realized_volatility_density$y, xlim=xlim, ylim=ylim, col="black")
	title(paste(underlying, delivery_month, " | X=[", paste(strike_percentage * 100, "%", sep="", collapse=","), "] | Days=", days, " | Runs=", runs, " | n=", dim(run_result_expired)[1], " | RFI=", risk_free_interest, " | mode=", hedging_volatility_mode, sep=""))
	legend("topleft", legend=c("Pricing Volatility","Pricing Volatility Adjusted","Realized Volatility"), text.col=c("blue","red","black"), pch=c(15,15,15), col=c("blue","red","black"))
	legend("topright", legend=unique(run_result_expired$period_level), pch=seq(1:length(period_level_vec)))	

#	---------------------------------------------------- plot 4 ----------------------------------------------------	
#	scatter plot of "Total PnL Before Commission" for both call and put
	yrange = range(run_result_expired$pnl_call, run_result_expired$pnl_put)
	plot(run_result_expired$realized_volatility, run_result_expired$pnl_call, ylim=yrange, yaxt="n", col="red", pch=seq(1:length(period_level_vec))[unclass(run_result_expired$period_level)], xlab="Current Realized Volatility", ylab="Total PnL Before Commission")
	points(run_result_expired$realized_volatility, run_result_expired$pnl_put, col="blue", pch=seq(1:length(period_level_vec))[unclass(run_result_expired$period_level)])	
	abline(h=0, col="black", pch=22, lty=2)
	axis(2, at=pretty(yrange,n=11))
	title(paste("Style=", option_style, "(", nSims, ") || VolatilityRatio (Pricing=", pricing_volatility_ratio, " | Hedging=", hedging_volatility_ratio, ")\nCalculation=", calculation_mode, " || Hist (", is_realized_volatility_df, " | w=", pricing_volatility_hist_weight, " | z=", pricing_volatility_hist_sd_z, ")", sep=""))	
	
#	---------------------------------------------------- plot 5 ----------------------------------------------------
#	scatter plot of "Option Spread Value" for both call and put	
#	risk free interest can have an affect on the price of call and put, increase rfi increases value of call and decreases the value of put
#	find the max and min for both call and put
	yrange = range(run_result$pricing_spread_value_call, run_result$pricing_spread_value_put)
	plot(run_result$realized_volatility, run_result$pricing_spread_value_call, ylim=yrange, yaxt="n", col="red", pch=seq(1:length(period_level_vec))[unclass(run_result$period_level)], xlab="Current Realized Volatility", ylab="Option Spread Value")
	points(run_result$realized_volatility, run_result$pricing_spread_value_put, col="blue", pch=seq(1:length(period_level_vec))[unclass(run_result$period_level)])		
	axis(2, at=pretty(yrange,n=11))
	
#	display pricing_spread_value for both call and put
	aa = round(tapply(run_result$pricing_spread_value_call, run_result$period_level, min), 2)
	bb = round(tapply(run_result$pricing_spread_value_call, run_result$period_level, mean), 2)
	cc = round(tapply(run_result$pricing_spread_value_call, run_result$period_level, max), 2)	
	dd = round(tapply(run_result$pricing_spread_value_put, run_result$period_level, min), 2)
	ee = round(tapply(run_result$pricing_spread_value_put, run_result$period_level, mean), 2)
	ff = round(tapply(run_result$pricing_spread_value_put, run_result$period_level, max), 2)	
	gg = paste("c=[", format(aa,nsmall=2), ", ", format(bb,nsmall=2), ", ", format(cc,nsmall=2), "], p=[", format(dd,nsmall=2), ", ", format(ee,nsmall=2), ", ", format(ff,nsmall=2), "]", sep="")	
	legend("bottomright", gg, pch=seq(1:length(gg)))	 
	legend("topleft", legend=c("Call","Put"), text.col=c("red","blue"), pch=c(15,15), col=c("red","blue"))

#	---------------------------------------------------- plot 6 ----------------------------------------------------
#	scatter plot of "Option Value as a % of Contract Date Settlement Price per Day" for both call and put	
#	risk free interest can have an affect on the price of call and put, increase rfi increases value of call and decreases the value of put
	dd = as.numeric(run_result$settlement_date - run_result$contract_date)
	pricing_spread_value_pct_call = 100 * run_result$pricing_spread_value_call / dd / run_result$contract_date_settlement
	pricing_spread_value_pct_put = 100 * run_result$pricing_spread_value_put / dd / run_result$contract_date_settlement
#	find the max and min for both call and put
	yrange = range(pricing_spread_value_pct_call, pricing_spread_value_pct_put)
	plot(run_result$realized_volatility, pricing_spread_value_pct_call, ylim=yrange, yaxt="n", col="red", pch=seq(1:length(period_level_vec))[unclass(run_result$period_level)], xlab="Current Realized Volatility", ylab="Option Value as a % of Contract Date Settlement Price per Day")
	points(run_result$realized_volatility, pricing_spread_value_pct_put, col="blue", pch=seq(1:length(period_level_vec))[unclass(run_result$period_level)])	
	axis(2, at=pretty(yrange,n=11))
	
#	---------------------------------------------------- plot 7 ----------------------------------------------------	
#	boxplot of realized_volatilty for each year, include runs that has not expired, i.e., data=run_result 
#	for "is_position_expired=FALSE" runs, "realized_volatility" is the realized volatility until current day
#	stats, a matrix, each column contains the extreme of the lower whisker, the lower hinge, the median, the upper hinge and the extreme of the upper whisker for one group/plot
	bp = boxplot(realized_volatility~settlement_date_year, data=run_result, xlab="Current Realized Volatility", ylab="Settlement Date (Year)", notch=FALSE, varwidth=TRUE, horizontal=TRUE, at=rev(1:nlevels(run_result$settlement_date_year)))
	text(rev(bp$stats[1,]), seq(bp$n)+0.4, rev(paste("n=",table(run_result$settlement_date_year),sep="")))
	text(rev(bp$stats[5,]), seq(bp$n)+0.4, rev(round(bp$stats[5,],4)))
#	add range for mean +/- 2*sd
#	mean.t = tapply(run_result_expired$realized_volatility, run_result_expired$period_level, mean)
#	sd.t = tapply(run_result_expired$realized_volatility, run_result_expired$period_level, sd)
#	yi = seq(bp$n) + 0.1
#	points(mean.t, yi, col="red", pch=18)
#	arrows(mean.t-2*sd.t, yi, mean.t+2*sd.t, yi, code=3, col="red", angle=75, length=0.1)	

	print("------------------------------------------- negative pnl (call) -------------------------------------------")
	print(run_result_expired[run_result_expired$pnl_call < 0,])
	print("------------------------------------------- negative pnl (put) -------------------------------------------")
	print(run_result_expired[run_result_expired$pnl_put < 0,])
}

############################################################################################################################################################
# TODO # save run_result plot
############################################################################################################################################################
if (dim(contract_settlement_date)[1] == 1) {
	pfilename = paste(getwd(), "/plot/", "single_result_plot_", 10000 + sum(grepl("single_result_plot", dir("plot"))), ".png", sep="")
} else {
	pfilename = paste(getwd(), "/plot/", "run_result_plot_", 10000 + sum(grepl("run_result_plot", dir("plot"))), ".png", sep="")
}
dev.copy(png, width = 1920, height = 1014, filename=pfilename)
dev.off ()

#save run_result
run_result_summary[[paste(contract_symbol,".",days,sep="")]] = run_result
print(summary(run_result_summary))


##------------------------------------------- realized volatility in the current period -------------------------------------------
##calculate realized volatility, log return = ln[P(t)/p(t-1)], the calculation includes both contract date and final date
#log_return = diff(log(valuation_data$settlement), lag=1)
#realized_volatility = sqrt(252 * mean(log_return^2))

##closer analysis of the realized volatility, if volatility_window = 0 then use the log daily return for calculation
#volatility_window = 5
#realized_volatility_array = vector()
#for (i in 1:length(log_return)) {
##	(volatility_window + 1) is the number of log_return points used to calculate realized volatility
#	end = i + volatility_window
#	if (end <= length(log_return)) {
#		realized_volatility_array[i] = sqrt(252 * mean(log_return[i:end]^2))
#	} else {		
#	}
#}

##	calculate options value and greeks
#	valuation_data$value1 = rep(0, dim(valuation_data)[1])
#	valuation_data$value2 = rep(0, dim(valuation_data)[1])
#	valuation_data$delta1 = rep(0, dim(valuation_data)[1])
#	valuation_data$delta2 = rep(0, dim(valuation_data)[1])
#	valuation_data$spread_value = rep(0, dim(valuation_data)[1])
#	valuation_data$spread_delta = rep(0, dim(valuation_data)[1])
#	valuation_data$days_to_maturity = rep(0, dim(valuation_data)[1])

#	the option spread position delta, calculated at the end of the day, i.e., based on settlement prices
#	valuation_data$position_delta = rep(0, dim(valuation_data)[1])

#	for (i in 1:dim(valuation_data)[1]){	
##	days to maturity at the end of that day, i.e., days_to_maturity = 0 for the settlement day
#		days_to_maturity = as.numeric(settlement_date - valuation_data[i,]$date)
##	calculate European option theoretical value and greeks
##	EuropeanOption(type, underlying, strike, dividendYield, riskFreeRate, maturity, volatility)
#		option1 = EuropeanOption(option_type[k], valuation_data[i,]$settlement, strike[1], 0, risk_free_interest, days_to_maturity/365, volatility_hedging[1])
#		option2 = EuropeanOption(option_type[k], valuation_data[i,]$settlement, strike[2], 0, risk_free_interest, days_to_maturity/365, volatility_hedging[2])
##	save the results to valuation_data dataset
#		valuation_data[i,]$value1 = option1$value
#		valuation_data[i,]$delta1 = option1$delta
#		valuation_data[i,]$value2 = option2$value
#		valuation_data[i,]$delta2 = option2$delta	
#		valuation_data[i,]$spread_value = option1$value - option2$value
#		valuation_data[i,]$spread_delta = option1$delta - option2$delta
#		valuation_data[i,]$days_to_maturity = days_to_maturity	
##	position delta, negative means we will be on the sell side
#		valuation_data[i,]$position_delta = -(option1$delta - option2$delta) * position_size
#	}

#par(new=FALSE)
#
### set up some fake test data
#time <- seq(0,72,12)
#betagal.abs <- c(0.05,0.18,0.25,0.31,0.32,0.34,0.35)
#cell.density <- c(0,1000,2000,3000,4000,5000,6000)
#
### add extra space to right margin of plot within frame
#par(mar=c(5, 4, 4, 6) + 0.1)
#
### Plot first set of data and draw its axis
#plot(time, betagal.abs, pch=16, axes=FALSE, ylim=c(0,1), xlab="", ylab="", 
#		type="b",col="black", main="Mike's test data")
#axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
#mtext("Beta Gal Absorbance",side=2,line=2.5)
#box()
#
### Allow a second plot on the same graph
#if (1) {
#	par(new=TRUE)
#}
### Plot the second plot and put axis scale on right
#plot(time, cell.density, pch=15,  xlab="", ylab="", ylim=c(0,7000), axes=FALSE, type="b", col="red")
### a little farther out (line=4) to make room for labels
#mtext("Cell Density",side=4,col="red",line=4) 
#axis(4, col="red",col.axis="red",las=1)
#
### Draw the time axis
#axis(1,pretty(range(time),10))
#mtext("Time (Hours)",side=1,col="black",line=2.5)  
#
#quantile(x <- rnorm(1001)) # Extremes & Quartiles by default
#quantile(x,  probs = c(0.1, 0.5, 1, 2, 5, 10, 50, NA)/100)
