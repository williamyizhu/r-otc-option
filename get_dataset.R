# TODO: Add comment
# 
# Author: William Yizhu
###############################################################################
#################################### create data set under Wind (www.wind.com.cn) ####################################
library(WindR)
w.start()
w.menu()
w.isconnected()

#exchange = "DCE"
#underlying = "m"
#delivery_month = "09"
#
##most recent year and the number of years of historical data
#year_end = 2014
#number_of_years = 10

#----------------------------------------- create a data frame with "contract", "wsd_start", "wsd_end" -----------------------------------------
#the first contract is the [year_start+delivery_month], which is listed in the previous year, i.e., year_start is the expiration year
contract = paste(underlying, substr(seq(year_end-number_of_years+1, year_end), 3, 4), delivery_month, ".", exchange, sep="")

#create a vector which index which month we would like to get the data from wsd
month_start = seq(as.Date(paste(year_end-number_of_years, "-01-01", sep="")), length=12*number_of_years, by="1 month")
month_end = seq(as.Date(paste(year_end-number_of_years+1, "-02-01", sep="")), length=12*number_of_years, by="1 month") - 1
month_index = rep(FALSE, 12)
month_index[as.numeric(delivery_month)] = TRUE
year_index = rep(month_index, number_of_years)

#create a data frame for month_start and month_end, then we can extract data from wsd
contract_wsd_period = data.frame(as.character(month_start), as.character(month_end))
contract_wsd_period = cbind(contract, contract_wsd_period[year_index,])
colnames(contract_wsd_period) = c("contract", "wsd_start", "wsd_end")

#check to see if the required dataset has already been downloaded from wsd
ff = paste(head(contract_wsd_period, n=1)$contract, tail(contract_wsd_period, n=1)$contract, Sys.Date(), sep="_")
fpath = paste(getwd(), "/dataset/", ff,".csv", sep="")

if (file.exists(fpath)) {
#	use existing csv file if required dataset is available
	data = read.csv(fpath, header=TRUE, sep=",", row.names=1)
	data$date = as.Date(data$date)
} else {	
#	----------------------------------------- extract data from wsd -----------------------------------------
	data = data.frame()
# 	we extract data from wsd R connection, instead of using pre-downloaded data, may neet do verfity the data against downloaded version
	for (i in 1:dim(contract_wsd_period)[1]) {
#		extract data from wsd, do not include ";Fill=Previous" in w.wsd(), only use the "good" data entry, i.e., those without "NA" value 	
		w_wsd_data = w.wsd(contract_wsd_period[i,]$contract, "open,high,low,close,settle,volume,oi", as.character(contract_wsd_period[i,]$wsd_start), as.character(contract_wsd_period[i,]$wsd_end), paste("TradingCalendar=",tCalendarList[exchange],sep=""))
		is_na_vec = apply(w_wsd_data$Data, 1, function(x){any(is.na(x)==TRUE)})
#		does not rbind to "data" if all the entry is "NA"
		if (all(is_na_vec)) {			
		} else {			
			data = rbind(data, cbind(contract_wsd_period[i,]$contract, w_wsd_data$Data[!is_na_vec,]))
		}
	}	
#	modify the column names, and save the dataset to a file
	colnames(data) = c("contract", "date", "open", "high", "low", "close", "settlement", "volume", "open_interest")	
	write.table(data, file=fpath, append=FALSE, sep=",", row.names=TRUE, col.names=NA)
}

#----------------------------------------- validate data from pre-downloaded csv file -----------------------------------------
#read csv data
underlying_validate = paste(underlying, substr(year_end, 3, 4), delivery_month, sep="")
fpath = paste(getwd(), "/dataset/", underlying_validate, ".csv", sep="")

#validate the "data" if pre-downloaded file exits
if (file.exists(fpath)) {
#	read "data_validate" from pre-downloaded file, and change the date to "Date" format
	data_validate = read.csv(fpath, row.names=1)
	data_validate$date = as.Date(data_validate$date)
	
#	choose the columns which need to be validated
	col_validate = c("open", "high", "low", "close", "settlement", "volume", "open_interest")
	
#	need to validate every entry in "data"
	for (i in 1:dim(data)[1]) {
#		check if the data[i,] is in the data_validate dataset, i.e., check if data[i,] can be validated
		if (any(data_validate$date == data[i,]$date)) {
#			get the index of the entry in data_validate		
			index_validate = which(data_validate$date == data[i,]$date)
#			for the same "date", if there is any column value in data doesn't match with data_validate
			if (any((data_validate[index_validate, col_validate] == data[i, col_validate]) == FALSE)) {
				print("-------------------------------------------------------------------------------------")
				print(paste("index_validate = ", index_validate, " | ", "i = ", i, " | ", "data = ", data[i,]$date, " | ", "data_validate = ", data_validate[index_validate,]$date, sep=""))
				print("-------------------------------------------------------------------------------------")
				print(data[i,])
				print(data_validate[index_validate,])
			} else {			
			}		
		} else {
			print(paste("can not be validated =", data[i,]$date), sep="")
		}
	}
} else {
	print(paste("validation dataset [", fpath, "] does not exit!!!", sep=""))
}

