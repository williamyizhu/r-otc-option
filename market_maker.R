#print(paste(getwd(), "/market_maker.R", sep=""))

#theoretical volatility matrix for bid, mid(theo) and ask volatility 
mm_vol_matrix = cbind(theo_bid_vec, vol_theo_vec, theo_ask_vec)

#generate theoretical prices
option_type = c("call", "put")
greeks_mm = c("value", "delta", "vega")
option_mm_list = list()
for (j in option_style) {
	if (j == "vanilla") {
		for (k in option_type) {
			option_mm_list[[k]][[j]] = mapply(EuropeanOption, k, f_atm, X_vec, 0, risk_free_interest, days_to_maturity/365, mm_vol_matrix)	
		}	
	} else if (j == "geometric") {
		for (k in option_type) {
			option_mm_list[[k]][[j]] = mapply(AsianOption, j, k, f_atm, X_vec, 0, risk_free_interest, days_to_maturity/365, mm_vol_matrix)	
		}	
	} else if (j == "arithmetic") {	
		gg = mapply(OptionMCv, "arithmetic", f_atm, X_vec, risk_free_interest, 0, days_to_maturity, mm_vol_matrix, nSims, minSteps, avgRuns)
		for (k in option_type) {
#			call_value, call_delta, call_vega, etc.
			option_mm_list[[k]][[j]] = data.frame(gg[paste(k,greeks_mm,sep="_"),], row.names=greeks_mm)	
		}
	}
}
option_mm_list2 = list()
for (k in option_type) {
	for (j in option_style) {		
		tmp = vector()
		for (mk in greeks_mm) {
			tmp = cbind(tmp, matrix(as.vector(unlist(option_mm_list[[k]][[j]][mk,])), ncol=3, byrow=FALSE))
		}
		colnames(tmp) = paste(rep(greeks_mm,each=3), rep(c("bid","mid","ask"),3), sep="_")
		option_mm_list2[[k]][[j]] = tmp	
	}
}

#------------- update GUI widgets -------------
#create a price and greek matrix
cc = data.frame(type="put", style="vanilla", key=c("vega_mid","delta_mid","value_bid","value_mid","value_ask"))
cc = apply(cc,c(1,2),as.character)
dd = data.frame(type="call", style="vanilla", key=c("value_bid","value_mid","value_ask","delta_mid","vega_mid"))
dd = apply(dd,c(1,2),as.character)
mkk = vector()
for (i in 1:dim(cc)[1]){
	mkk = cbind(mkk, round(option_mm_list2[[cc[i,"type"]]][[cc[i,"style"]]][,cc[i,"key"]],2))
}
mkk = cbind(mkk, X_vec)
for (i in 1:dim(dd)[1]){
	mkk = cbind(mkk, round(option_mm_list2[[dd[i,"type"]]][[dd[i,"style"]]][,dd[i,"key"]],2))
}
colnames(mkk) = c("pVega","pDelta","pBid","pMid","pAsk","strike","cBid","cMid","cAsk","cDelta","cVega")
#update widgets
delete(.GlobalEnv$greeks_gframe_obj, .GlobalEnv$greeks_gtable_obj)
.GlobalEnv$greeks_gtable_obj = gtable(mkk, container=.GlobalEnv$greeks_gframe_obj)			
gtkTreeViewSetGridLines(getToolkitWidget(.GlobalEnv$greeks_gtable_obj), grid.lines=3)

#create a volatility matrix
mm_strike_vol_matrix = cbind(X_vec, round(mm_vol_matrix*100,4))
colnames(mm_strike_vol_matrix) = c("strike","volBid","volMid","volAsk")
delete(.GlobalEnv$volatility_gframe_obj, .GlobalEnv$volatility_gtable_obj)
.GlobalEnv$volatility_gtable_obj = gtable(mm_strike_vol_matrix, container=.GlobalEnv$volatility_gframe_obj)			
gtkTreeViewSetGridLines(getToolkitWidget(.GlobalEnv$volatility_gtable_obj), grid.lines=3)

#------------- export prices and greeks to file -------------
contract_symbol = toupper(paste(exchange, underlying, contract_year_month, sep="."))
contract_date = Sys.Date()
settlement_date = contract_date + days_to_maturity

#convert option_style from english to chinese, use "geometric" results to approximate "arithmetic" results
option_style_chs = c(vanilla="最后交易日结算价", geometric="期限内交易日结算价几何平均值", arithmetic="期限内交易日结算价算数平均值")
option_style_chs = c(vanilla="最后交易日结算价", geometric="期限内交易日结算价算数平均值", arithmetic="期限内交易日结算价算数平均值")

#e.g., file name "pricing_DCE.M.1501_2014-08-26。txt", only 1 pricing file per day
fpath = paste(getwd(), "/pricing/", "pricing_", contract_symbol, "_", contract_date, "_MM.txt", sep="")
if (file.exists(fpath)) {
	file.remove(fpath)	
}

sepstr = paste(rep("-",60), collapse="")
write.table(sepstr, fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
write.table(paste("报价日期 =", contract_date, "|", "合约 =", contract_symbol, "|", "结算价 =", f_atm), fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
write.table(paste("结算日期 =", settlement_date, "|", "期限 =", days_to_maturity, "天"), fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)

for (j in option_style) {
	write.table(sepstr, fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
	write.table(paste("到期结算价 =", option_style_chs[j]), fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)		
	write.table(sepstr, fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)	
	write.table(paste(c("买入/卖出（看跌）","执行价格","买入/卖出（看涨）"),"\t",sep="",collapse=""), fpath, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)	
	ppp = matrix(apply(format(round(option_mm_list2[["put" ]][[j]][,c("value_bid","value_ask")],2),nsmall=2), 1, function(x){paste(x,collapse="/")}))
	ccc = matrix(apply(format(round(option_mm_list2[["call"]][[j]][,c("value_bid","value_ask")],2),nsmall=2), 1, function(x){paste(x,collapse="/")}))	
	write.table(cbind(ppp,X_vec,ccc), fpath, append=TRUE, quote=FALSE, sep="\t\t", row.names=FALSE, col.names=FALSE)		
}

#library(gridExtra)
#pdf("data_output.pdf", height=11, width=8.5)
#mg = cbind(ppp,X_vec,ccc)
#colnames(mg) = c("明天","后天","xx")
#print(mg)
#grid.table(mg)
#dev.off()

##european style put options
#mm_put_matrix = mapply(EuropeanOption, "put", f_atm, X_vec, 0, risk_free_interest, days_to_maturity/365, mm_vol_matrix)
#mm_put_value = matrix(as.vector(unlist(mm_put_matrix[c("value"),])), ncol=3, byrow=FALSE)
#mm_put_delta = matrix(as.vector(unlist(mm_put_matrix[c("delta"),])), ncol=3, byrow=FALSE)
#mm_put_vega  = matrix(as.vector(unlist(mm_put_matrix[c("vega"),])),  ncol=3, byrow=FALSE)
#
##european style call options
#mm_call_matrix = mapply(EuropeanOption, "call", f_atm, X_vec, 0, risk_free_interest, days_to_maturity/365, mm_vol_matrix)
#mm_call_value = matrix(as.vector(unlist(mm_call_matrix[c("value"),])), ncol=3, byrow=FALSE)
#mm_call_delta = matrix(as.vector(unlist(mm_call_matrix[c("delta"),])), ncol=3, byrow=FALSE)
#mm_call_vega  = matrix(as.vector(unlist(mm_call_matrix[c("vega"),])),  ncol=3, byrow=FALSE)
#
##------------- update GUI widgets -------------
##create a price and greek matrix
#mm_greeks = cbind(round(mm_put_vega[,2]*0.01,4), round(mm_put_delta[,2],4), round(mm_put_value,4), X_vec, round(mm_call_value,4), round(mm_call_delta[,2],4), round(mm_call_vega[,2]*0.01,4))
#colnames(mm_greeks) = c("pVega","pDelta","pBid","pMid","pAsk","strike","cBid","cMid","cAsk","cDelta","cVega")
#delete(.GlobalEnv$greeks_gframe_obj, .GlobalEnv$greeks_gtable_obj)
#.GlobalEnv$greeks_gtable_obj = gtable(mm_greeks, container=.GlobalEnv$greeks_gframe_obj)			
#gtkTreeViewSetGridLines(getToolkitWidget(.GlobalEnv$greeks_gtable_obj), grid.lines=3)
##print(mm_greeks)

#tmm = function() {
#	tmp = vector()
#	for (mk in greeks_mm) {
#		tmp = cbind(tmp, matrix(as.vector(unlist(option_mm[mk,])), ncol=3, byrow=FALSE))
#	}
#	colnames(tmp) = paste(rep(greeks_mm,each=3), rep(c("bid","mid","ask"),3), sep="_")
#	option_mm_list[[k]][[j]] = tmp	
#}


#print(option_mm_list[["put"]][["vanilla"]][,c(paste(c("vega","delta"),"mid",sep="_"),paste("value",bma,sep="_"))])


#lapply(option_mm_list, function(x){print(colnames(x))})
#print(option_mm_list)
#option_mm = mapply(EuropeanOption, matrix(rep(option_type,7),ncol=2,byrow=TRUE), f_atm, X_vec, 0, risk_free_interest, days_to_maturity/365, mm_vol_matrix)



#print(option_mm)
#
#option_mm_list = list()
#for (k in option_type) {
##-------------------------------------- generate price matrix, which contains X --------------------------------------		
#
##-------------------------------------- calculate option price matrix, let S = f_atm --------------------------------------	
##	EuropeanOption("call", f_atm, 2951, 0, risk_free_interest, days_to_maturity/365, pricing_volatility_adj_vec[1])
#	for (j in option_style) {
##		calculate theoretical prices, option value of the first and second strike price
#		if (j == "vanilla") {			
#			option_mm = mapply(EuropeanOption, k, f_atm, X_vec, 0, risk_free_interest, days_to_maturity/365, mm_vol_matrix)
#		} else if (j == "geometric") {
#			option_mm = mapply(AsianOption, j, k, f_atm, X_vec, 0, risk_free_interest, days_to_maturity/365, mm_vol_matrix)
#		} else if (j == "arithmetic") {		
##			option_mm = mapply(OptionMCv, j, f_atm, matrix(X_vec), 0, risk_free_interest, days_to_maturity/365, theo_bid_vec, nSims, minSteps, avgRuns)
##			
##			option_mm = mapply(OptionMCv, j, f_atm, X_vec, risk_free_interest, 0, days_to_maturity, mm_vol_matrix, nSims, minSteps, avgRuns)
#			
##			nn = mapply(OptionMCv, j, f_atm, strike_matrix, risk_free_interest, 0, days_to_maturity, theo_bid_matrix, nSims, minSteps, avgRuns)
##			option_ask = rbind(option_mm, value=option_mm[paste(k,"value",sep="_"),])			
##			print(option_ask)
#			#			option_bid = rbind(nn, value=nn[paste(k,"value",sep="_"),])
#		} else {			
#		}
#		
##		print(option_mm)
##		
##		
##		greeks_mm = c("value", "delta", "vega")		
##		tmp = matrix(as.vector(unlist(option_mm[greeks_mm,])), ncol=3, byrow=TRUE)
##		colnames(tmp) = greeks_mm
##		xtt = vector()
##		for (mk in greeks_mm) {			
##			xtt = cbind(xtt, matrix(as.vector(unlist(tmp[,mk])), ncol=3, byrow=FALSE))
##		}		
##		colnames(xtt) = paste(rep(greeks_mm,each=3), rep(c("bid","mid","ask"),3), sep="_")
##
##		
##		option_mm_list[[k]][[j]] = xtt
#				
##				matrix(as.vector(unlist(option_mm[c("value","delta","vega"),])), ncol=3, byrow=TRUE)
#		
#		
#		
#		
###		extract "value" from "option_ask" and "option_bid"
##		option_ask_matrix = matrix(unlist(option_ask["value",]), nrow=dim(strike_matrix)[1])
##		option_bid_matrix = matrix(unlist(option_bid["value",]), nrow=dim(strike_matrix)[1])
#		
###		generate pricing matrix
##		strike_matrix_range = cbind(paste(strike_matrix[,1],"~ ",sep=""), matrix(paste(strike_matrix[,1],"~",strike_matrix[,-1],sep=""),nrow=dim(strike_matrix)[1]))
##		pricing_matrix = as.data.frame(matrix(paste(format(round(option_spread_bid,2),nsmall=2), "/", format(round(option_spread_ask,2),nsmall=2), "(", strike_matrix_range, ")", sep=""), nrow=dim(strike_matrix)[1], dimnames=list(rownames(strike_matrix),colnames(strike_matrix))))
###		save result to a list, both "call" and "put", export to a file
##		pricing_matrix_list[[k]][[j]] = pricing_matrix
#		
##		print("---------------------------------------------------------------------------")
##		print(paste("option type =", k, "|", "option style =", j))
##		print("---------------------------------------------------------------------------")
##		print(pricing_matrix)		
#	}	
#}
#
#
#print(option_mm_list)




#mm_put_greeks = matrix(as.vector(unlist(mm_put_matrix[c("value","delta","vega"),])), ncol=3, byrow=TRUE)
#
#mm_put_value = matrix(mm_put_greeks[,1], ncol=3, byrow=FALSE)
#mm_put_delta = matrix(mm_put_greeks[,2], ncol=3, byrow=FALSE)
#mm_put_vega  = matrix(mm_put_greeks[,3], ncol=3, byrow=FALSE)
#
#print(gg)
#print(mm_put_value)
#mm_call_greeks = matrix(as.vector(unlist(mm_call_matrix[c("value","delta","vega"),])), ncol=3, byrow=TRUE)
#mm_call_value = matrix(mm_call_greeks[,1], ncol=3, byrow=FALSE)
#mm_call_delta = matrix(mm_call_greeks[,2], ncol=3, byrow=FALSE)
#mm_call_vega  = matrix(mm_call_greeks[,3], ncol=3, byrow=FALSE)

