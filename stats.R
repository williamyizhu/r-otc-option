# TODO: Add comment

###############################################################################
rm(list=ls(all=TRUE)[! ls(all=TRUE) %in% c("run_result_summary")])
rm(list=ls(all=TRUE))
options(width = 438L)

##################################### create data set under Wind (www.wind.com.cn) ####################################
#exchange = "DCE"
#underlying = "M"
##[year + delivery month]
#contract_year_month = "1405"
#contract_symbol = toupper(paste(exchange, underlying, contract_year_month, sep="."))
#delivery_month = substr(contract_year_month, 3, 4)
##most recent year expressed as [20XX], and the number of years of historical data
#year_end = as.numeric(paste("20", substr(contract_year_month, 1, 2), sep=""))
#number_of_years = 10
#

contract_year_month_vec = c("1401", "1405", "1409")

run_result_summary = list()	
##variables inside ["C:/Users/William Yizhu/Documents/workspace/r-option/simulation_batch.R"] are all local to this file
for (contract_year_month in contract_year_month_vec) {
	print(contract_year_month)
	source(paste(getwd(), "/simulation_batch.R", sep=""), echo=FALSE, encoding="GBK")
}



####################################  ####################################
print(summary(run_result_summary))

realized_volatility_mean = data.frame()
realized_volatility_sd = data.frame()
for (i in names(run_result_summary)) {
#	iterate through each contract_symbol
	run_result = run_result_summary[[i]]
	run_result_expired = run_result[run_result$is_position_expired,]
#	calculate the mean and sd for "settlement_date_month"
	realized_volatility_mean = rbind(realized_volatility_mean, aggregate(realized_volatility~settlement_date_month, data=run_result_expired, FUN=mean))
	realized_volatility_sd = rbind(realized_volatility_sd, aggregate(realized_volatility~settlement_date_month, data=run_result_expired, FUN=sd))
}

#merge mean and sd
realized_volatility = merge(realized_volatility_mean, realized_volatility_sd, by="settlement_date_month")
colnames(realized_volatility) = c("settlement_date_month", "mean", "sd")
realized_volatility$settlement_date_month = as.numeric(as.character(realized_volatility$settlement_date_month))
realized_volatility = realized_volatility[order(realized_volatility$settlement_date_month), ]

#confidence interval, mean +/- 2 * sd
realized_volatility$upper = realized_volatility$mean + 2 * realized_volatility$sd
realized_volatility$lower = realized_volatility$mean - 2 * realized_volatility$sd

ylim = c(min(realized_volatility$upper, realized_volatility$lower), max(realized_volatility$upper, realized_volatility$lower))
plot(realized_volatility$settlement_date_month, realized_volatility$mean, ylim=ylim, type="b", xlab="Settlement Date Month", ylab="Realized Volatility (Mean)")
arrows(realized_volatility$settlement_date_month, realized_volatility$upper, realized_volatility$settlement_date_month, realized_volatility$lower, code=3, length=0.1, angle=90, col='red')

#export realized volatility stats to a file
aa = unlist(strsplit(names(run_result_summary)[1], "[.]"))
fpath = paste(getwd(), "/pricing/", paste("realized_volatility", paste(aa[c(1,2,4)], collapse="."), sep="_"), ".txt", sep="")
write.table(realized_volatility, file=fpath, append=FALSE, sep=",", row.names=FALSE)



#plot(realized_volatility$settlement_date_month, realized_volatility$sd, type="b", xlab="Settlement Date Month", ylab="Realized Volatility (SD)")
#text(seq(realized_volatility_mean$settlement_date_month), (realized_volatility_mean$realized_volatility)+0.002, round(realized_volatility_mean$realized_volatility,4))



