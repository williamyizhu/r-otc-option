############################################################################################################################################################
# TODO # Add comment
############################################################################################################################################################
#"volatility" is calculated based on "sqrt(252 * mean(log_return^2))", where "log_return = diff(log(settlement), lag=1)"
#therefore, "number_of_volatility_days" is not a determint factor, when it comes to calculation of "volatility"

#rm(list=ls(all=TRUE)) 
#exchange = "DCE"
#underlying = "M"
##underlying = "I"
##[year + delivery month]
#contract_year_month = "1501"
#option_style = c("vanilla", "geometric", "arithmetic")
#option_style = c("vanilla", "geometric")
#option_style = c("arithmetic")
#option_style = c("vanilla")
##higher interest rate increases the value of call, and decreases the value of put
#risk_free_interest = 0.0
##number of calendar days, not trading days
#days_to_maturity = 30
##x1_pct_chg is based on atm, x2_pct_chg is based on x1_pct_chg
#x1_itm_otm_pct = c(3, 7, 0.02)
#x2_itm_otm_pct = c(0, 3, 0.05)
##numerical method parameters
#nSims = 50
#minSteps = 500
#avgRuns = 100

#options(width = 438L)
#library("RQuantLib")
source(paste(getwd(), "/option_mc.R", sep=""), local=TRUE, echo=FALSE, encoding="GBK")
#need to run "Sys.setlocale(locale="chs")" before source() this file

#trading calendar list for iWind
tCalendarList = list(DCE="DCE",SHF="SHFE")

############################################################################################################################################################
# TODO # underlying contract and period
############################################################################################################################################################
contract_symbol = toupper(paste(exchange, underlying, contract_year_month, sep="."))
delivery_month = substr(contract_year_month, 3, 4)
#most recent year expressed as [20XX], and the number of years of historical data
year_end = as.numeric(paste("20", substr(contract_year_month, 1, 2), sep=""))
number_of_years = 10

#variables inside ["C:/Users/William Yizhu/Documents/workspace/r-option/get_dataset.R"] are all local to this file
source(paste(getwd(), "/get_dataset.R", sep=""), local=TRUE, echo=FALSE, encoding="GBK")
print("---------------------------------------------------------------------------")
print("head(data)")
print("---------------------------------------------------------------------------")
print(head(data))
print("---------------------------------------------------------------------------")
print("tail(data)")
print("---------------------------------------------------------------------------")
print(tail(data))

############################################################################################################################################################
# TODO # parameters
############################################################################################################################################################
option_type = c("call", "put")
option_type_chs = c("看涨", "看跌")
option_style_all = c("vanilla", "geometric", "arithmetic")

#convert option_style from english to chinese, use "geometric" results to approximate "arithmetic" results
option_style_index = apply(matrix(option_style), 1, function(a,b){which(b==a)}, option_style_all)
option_style_chs = c("最后交易日结算价", "期限内交易日结算价几何平均值", "期限内交易日结算价算数平均值")
option_style_chs = c("最后交易日结算价", "期限内交易日结算价算数平均值", "期限内交易日结算价算数平均值")

#strike price rounding digit
strike_rounding_digit = 0

############################################################################################################################################################
# TODO # volatility model
############################################################################################################################################################
#use current date as the contract_date, "days_to_maturity" is calendar days, use the number of trading days, not calendar days, "+ 1" for "final_date"
contract_date = tail(data, n=1)$date
settlement_date = contract_date + days_to_maturity
number_of_volatility_days = as.numeric(w.tdayscount(contract_date, settlement_date, TradingCalendar=tCalendarList[exchange])$Data[2]) + 1

##-------------------------------------- calculate historical realized volatility --------------------------------------
#log_return = diff(log(tail(data, n=number_of_volatility_days)$settlement), lag=1)
#previous_realized_volatility = sqrt(252 * mean(log_return^2))
#
##read saved file (e.g., "realized_volatility_DCE.M.30.txt") for historical realized volatility stats, e.g., mean, sd, etc.
#fpath = paste(getwd(), "/pricing/", paste("realized_volatility", paste(exchange, underlying, days_to_maturity, sep="."), sep="_"), ".txt", sep="")
#if (file.exists(fpath)) {
#	realized_volatility_df = read.csv(fpath, header=TRUE, sep=",")
##	if historical realized volatility data exits, get the historical realized volatility for this settlement month	
#	settlement_date_month = as.numeric(unlist(strsplit(as.character(settlement_date), "-"))[2])
#	hist_realized_volatility_mean = realized_volatility_df[settlement_date_month,]$mean	
#	hist_realized_volatility_sd = realized_volatility_df[settlement_date_month,]$sd		
#} else {	
##	stop(paste("\"", fpath, "\" does not exist!"))
##	readline(prompt=paste("\"", fpath, "\" does not exist!", sep=""))	
##	historical realized volatility mean and sd does not exist
#	hist_realized_volatility_mean = NA
#	hist_realized_volatility_sd = NA
#}

############################################################################################################################################################
# TODO # calculate option price matrix and print results to screen
############################################################################################################################################################
#ATM price, use the latest settlement price as the starting price
atm = tail(data, n=1)$settlement

#there is no trading activities on the contract date (therefore - 1) or on weekends
number_of_trading_days = number_of_volatility_days - 1

print("---------------------------------------------------------------------------")
print(paste("contract date =", tail(data,n=1)$date, "|", "symbol =", contract_symbol, "|", "days to maturity =", days_to_maturity))
print(paste("atm =", atm, "|", "number of trading days =", number_of_trading_days, "|", "risk free interest =", risk_free_interest))
#print(paste("volatility | previous = ", round(previous_realized_volatility*100,2), "% | historical = ", round(hist_realized_volatility_mean*100,2), "% (mean) ", round(hist_realized_volatility_sd*100,2), "% (sd)", sep=""))

pricing_matrix_list = list()
for (k in option_type) {
#-------------------------------------- generate price matrix, which contains X --------------------------------------		
#	first option strike	
	if (k == "call") {
		x1 = 1 + seq(x1_itm_otm_pct[2],-x1_itm_otm_pct[1],-1) * x1_itm_otm_pct[3]
	} else if (k == "put") {
		x1 = 1 + seq(x1_itm_otm_pct[1],-x1_itm_otm_pct[2],-1) * x1_itm_otm_pct[3]
	} else {		
	}
#	second option strike
	x2_pct_chg = seq(1:x2_itm_otm_pct[2]) * x2_itm_otm_pct[3]
#	generate "strike percentage matrix"	
	strike_pct_matrix = cbind(matrix(x1), apply(matrix(x2_pct_chg), 1, function(a,b,k){b+ifelse(k=="call",1,-1)*a}, x1, k))
	colnames(strike_pct_matrix) = c("x1(100%)", paste("x2(", ifelse(k=="call","+","-"), round(x2_pct_chg*100,2), "%)", sep=""))	
	rownames(strike_pct_matrix) = paste(round((x1-1)*100), "%", sep="")
#	convert the pct into actual price, strike_matrix is different for call and put
	strike_matrix = round(strike_pct_matrix*atm, strike_rounding_digit)	
#	export "strike_matrix" to a .csv file for other uses
	write.csv(strike_matrix, paste(getwd(), "/pricing/", paste("strike_matrix", exchange, underlying, contract_year_month, k, sep="."), ".csv", sep=""), quote=FALSE, row.names=TRUE)
	
#-------------------------------------- create volatility matrix (bid and ask) based on volatility model --------------------------------------		
#	** theoretical volatility **, apply wing model to strike price vector "X_vec"
	vol_theo = apply(matrix(strike_matrix,ncol=1), 1, volatility_wing_model, 
			days_to_maturity, alpha, f_atm, f_ref, SSR,
			vol_ref, VCR, slope_ref, SCR,
			dn_cf, up_cf, put_curv, call_curv,
			dn_sm, up_sm, dn_slope, up_slope)	
#	extract values from "vol_theo"
	vol_theo_vec = as.vector(sapply(vol_theo, function(x){unlist(x["theo"])}, simplify=TRUE))			
	
#	** bid/ask offset **
	ba_offset = apply(matrix(strike_matrix,ncol=1), 1, volatility_wing_model, 
			days_to_maturity, alpha, f_atm, f_ref, SSR,
			vol_ref_offset, VCR_offset, slope_ref_offset, SCR_offset,
			dn_cf_offset, up_cf_offset, put_curv_offset, call_curv_offset,
			dn_sm_offset, up_sm_offset, dn_slope_offset, up_slope_offset)
#	extract values from "ba_offset", offset must be greater than 0
	ba_offset_vec = pmax(0, as.vector(sapply(ba_offset, function(x){unlist(x["theo"])}, simplify=TRUE)))				
#	same offsets for bid and ask, need to adjust "vol_theo" level
	theo_ask_matrix = matrix(vol_theo_vec+ba_offset_vec, nrow=dim(strike_matrix)[1])
	theo_bid_matrix = matrix(vol_theo_vec-ba_offset_vec, nrow=dim(strike_matrix)[1])
	
#	generate volatility matrix for both bid and ask volatility
	volatility_matrix = as.data.frame(matrix(paste(format(round(theo_bid_matrix*100,2),nsmall=2), "/", format(round(theo_ask_matrix*100,2),nsmall=2), "(", strike_matrix, ")", sep=""), nrow=dim(strike_matrix)[1], dimnames=list(rownames(strike_matrix),colnames(strike_matrix))))

	print("---------------------------------------------------------------------------")
	print(paste("option type =", k, "| volatility pricing %"))
	print("---------------------------------------------------------------------------")
	print(volatility_matrix)
	
#-------------------------------------- calculate option price matrix, let S = atm --------------------------------------	
#	EuropeanOption("call", atm, 2951, 0, risk_free_interest, days_to_maturity/365, pricing_volatility_adj_vec[1])
	for (j in option_style) {
#		calculate theoretical prices, option value of the first and second strike price
		if (j == "vanilla") {			
			option_ask = mapply(EuropeanOption, k, atm, strike_matrix, 0, risk_free_interest, days_to_maturity/365, theo_ask_matrix)
			option_bid = mapply(EuropeanOption, k, atm, strike_matrix, 0, risk_free_interest, days_to_maturity/365, theo_bid_matrix)
		}
		else if (j == "geometric") {
			option_ask = mapply(AsianOption, j, k, atm, strike_matrix, 0, risk_free_interest, days_to_maturity/365, theo_ask_matrix)
			option_bid = mapply(AsianOption, j, k, atm, strike_matrix, 0, risk_free_interest, days_to_maturity/365, theo_bid_matrix)
		} else if (j == "arithmetic") {		
			mm = mapply(OptionMC_vec, j, atm, strike_matrix, risk_free_interest, 0, days_to_maturity, theo_ask_matrix, nSims, minSteps, avgRuns)
			nn = mapply(OptionMC_vec, j, atm, strike_matrix, risk_free_interest, 0, days_to_maturity, theo_bid_matrix, nSims, minSteps, avgRuns)
			option_ask = rbind(mm, value=mm[paste(k,"value",sep="_"),])			
			option_bid = rbind(nn, value=nn[paste(k,"value",sep="_"),])
		} else {			
		}
#		extract "value" from "option_ask" and "option_bid"
		option_ask_matrix = matrix(unlist(option_ask["value",]), nrow=dim(strike_matrix)[1])
		option_bid_matrix = matrix(unlist(option_bid["value",]), nrow=dim(strike_matrix)[1])
#		calculate the spread matrix, option_ask_matrix[,1] and option_bid_matrix[,1] are plain vanilla option prices
		option_spread_ask = cbind(option_ask_matrix[,1], apply(option_bid_matrix[,-1], 2, function(a,b){b-a}, option_ask_matrix[,1]))			
		option_spread_bid = cbind(option_bid_matrix[,1], apply(option_ask_matrix[,-1], 2, function(a,b){b-a}, option_bid_matrix[,1]))	
		
#		generate pricing matrix
		strike_matrix_range = cbind(paste(strike_matrix[,1],"~ ",sep=""), matrix(paste(strike_matrix[,1],"~",strike_matrix[,-1],sep=""),nrow=dim(strike_matrix)[1]))
		pricing_matrix = as.data.frame(matrix(paste(format(round(option_spread_bid,2),nsmall=2), "/", format(round(option_spread_ask,2),nsmall=2), "(", strike_matrix_range, ")", sep=""), nrow=dim(strike_matrix)[1], dimnames=list(rownames(strike_matrix),colnames(strike_matrix))))
#		save result to a list, both "call" and "put", export to a file
		pricing_matrix_list[[k]][[j]] = pricing_matrix
		
		print("---------------------------------------------------------------------------")
		print(paste("option type =", k, "|", "option style =", j))
		print("---------------------------------------------------------------------------")
		print(pricing_matrix)		
	}	
}

############################################################################################################################################################
# TODO # export to a file
############################################################################################################################################################
#file name "pricing_DCE.M.1501_2014-08-26。txt", only 1 pricing file per day
fpath = paste(getwd(), "/pricing/", "pricing_", contract_symbol, "_", as.character(tail(data,n=1)$date), ".txt", sep="")

if (file.exists(fpath)) {
	file.remove(fpath)
} else {	
}

write.table("---------------------------------------------------------------------------", fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
write.table(paste("报价日期 =", tail(data,n=1)$date, "|", "合约 =", contract_symbol, "|", "结算价 =", atm, "|", "期限 =", days_to_maturity, "天"), fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
for (i in 1:length(option_type)) {
	for (j in 1:length(option_style_index)) {
		write.table("---------------------------------------------------------------------------", fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
		write.table(paste("类型 =", option_type_chs[i], "|", "到期结算价 =", option_style_chs[option_style_index[j]]), fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
		write.table("---------------------------------------------------------------------------", fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
		write.table(paste("\t\t", colnames(pricing_matrix_list[[option_type[i]]][[option_style[j]]]), sep="", collapse=""), fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
		write.table(pricing_matrix_list[[option_type[i]]][[option_style[j]]], fpath, append=TRUE, quote=FALSE, sep="\t", row.names=TRUE, col.names=FALSE)		
	}
}

#set the locale back to "English_United States"
#Sys.setlocale(locale="us")

#print(cbind(as.character(data$date),data$volume))
#print(tail(data,n=50))

##volatility used for pricing the option spread
#pricing_volatility_spread = c(0.0, -0.0)
##"simulation.R", pricing_volatility = pricing_volatility_hist_pct * (hist_realized_volatility_mean + pricing_volatility_hist_sd_z * hist_realized_volatility_sd) + (1 - pricing_volatility_hist_pct) * head(realized_volatility_vec, n=1)
#pricing_volatility_hist_weight = 0.75
#pricing_volatility_hist_sd_z = 1.96
##"simulation.R", pricing_volatility_adj = pricing_volatility_ratio * pricing_volatility
#pricing_volatility_ratio = 1.25

##	use a weighted version to calculate "pricing_volatility"; 	
#	pricing_volatility = pricing_volatility_hist_weight * (hist_realized_volatility_mean + pricing_volatility_hist_sd_z * hist_realized_volatility_sd) + (1 - pricing_volatility_hist_weight) * previous_realized_volatility

##	otherwise, use "previous_realized_volatility"
#	pricing_volatility = previous_realized_volatility

##volatility used for pricing and hedging are not the same, pricing volatility shall be higher than hedging volatility
#pricing_volatility_adj = pricing_volatility_ratio * pricing_volatility
#pricing_volatility_adj_vec = pricing_volatility_adj + pricing_volatility_spread

#pricing_volatility_ask_offset = 0.0
#pricing_volatility_bid_offset = 0.0	
##pricing_volatility_theo = pricing_volatility_hist_weight * hist_realized_volatility_mean + (1 - pricing_volatility_hist_weight) * previous_realized_volatility 
#pricing_volatility_theo = pricing_volatility_adj
#pricing_volatility_ask = (pricing_volatility_theo + pricing_volatility_ask_offset) + pricing_volatility_spread
#pricing_volatility_bid = (pricing_volatility_theo - pricing_volatility_bid_offset) + pricing_volatility_spread	

#print(paste("weight = ", pricing_volatility_hist_weight, " | pricing volatility ratio = ", pricing_volatility_ratio, sep=""))
#print(paste("pricing volatility ask = ", paste(round(pricing_volatility_ask*100,2), "%", c(" (v1) ", " (v2)"), sep="", collapse=""), sep=""))
#print(paste("pricing volatility bid = ", paste(round(pricing_volatility_bid*100,2), "%", c(" (v1) ", " (v2)"), sep="", collapse=""), sep=""))












