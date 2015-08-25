############################################################################################################################################################
# TODO # 
############################################################################################################################################################
#"volatility" is calculated based on "sqrt(252 * mean(log_return^2))", where "log_return = diff(log(settlement), lag=1)"
#therefore, "number_of_volatility_days" is not a determint factor, when it comes to calculation of "volatility"

#rm(list=ls(all=TRUE)) 
#options(width = 438L)

#print(exchange)
#print(underlying)
#print(contract_year_month)
#print(days_to_maturity)
#
#exchange = "DCE"
#underlying = "M"
###[year + delivery month]
#contract_year_month = "1505"

##number of calendar days, not trading days
#days_to_maturity = 30

##threshold for historical volatility used in calculation of log_return
#print(log_return_threshold)
#log_return_threshold = 0.025

#print(historical_volatility_calculation)
eDate = historical_volatility_calculation[1]
bDays = as.numeric(historical_volatility_calculation[2])
bYears = as.numeric(historical_volatility_calculation[3])
#print(class(bYears))

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
#print("---------------------------------------------------------------------------")
#print("head(data)")
#print("---------------------------------------------------------------------------")
#print(head(data))
#print("---------------------------------------------------------------------------")
#print("tail(data)")
#print("---------------------------------------------------------------------------")
#print(tail(data))

############################################################################################################################################################
# TODO # create dataset and analysis
############################################################################################################################################################
#convert from trading days to calendar days (cDays), fill with pervious trading days value if non-trading days
cDays = data.frame(date=seq(head(data,n=1)$date,tail(data,n=1)$date,by="1 day"))
data_cDays = na.locf(merge(data, cDays, by="date", all=TRUE))
data_cDays$date = as.Date(data_cDays$date)

#only use data between "start_date_vec" and "end_date_vec", "bDays" is the number of days back from "end_date_vec"
end_date_vec = as.Date(paste(c(unique(mapply(format,data_cDays$date,"%Y")),year_end), eDate, sep="-"))
start_date_vec = end_date_vec - bDays

#re-organize "data_cDays" into "contract_year_month" format
data_cDays_list = list()
for (i in 1:length(start_date_vec)) {
#	only select data between "start" and "end" period, use "$contract" (e.g., M1501.DCE) as list name
	ind = (start_date_vec[i] <= data_cDays$date) & (data_cDays$date <= end_date_vec[i])
	if (dim(data_cDays[ind,])[1] > 0) {
		data_cDays_list[[tail(data_cDays[ind,],n=1)$contract]] = data_cDays[ind,]
	}	
}

#calculate log_return list, used as a filter to exclude the outliers
log_return_list = lapply(data_cDays_list, function(ds,thr) {
			if (dim(ds)[1] > 1) {
				date2 = format(tail(ds$date,n=dim(ds)[1]-1),"%m-%d")				
				log_return = diff(log(as.numeric(ds$settlement)), lag=1)				
				is_outlier = abs(log_return) > thr						
				log_return_adj = ifelse(is_outlier, 0, log_return)				
				data.frame(date=tail(ds$date,n=dim(ds)[1]-1), date2=date2, log_return=log_return, is_outlier=is_outlier, log_return_adj=log_return_adj)
			}
		}, log_return_threshold)

#moving window of historical volatility, lr="log_return", vp="volatility period", dn="days normalizer, e.g., 365 or 252"
realized_volatility_list = lapply(log_return_list, function(lr,vp,dn) {
			if (dim(lr)[1] >= vp) {
				data.frame(date=tail(lr$date2,n=dim(lr)[1]-vp+1), realized_volatility=sqrt(dn*apply(embed(lr$log_return_adj^2,vp),1,mean)))
			}
		}, days_to_maturity, 365)

#re-arrange "realized_volatility_list" to "realized_volatility_mat"
for (i in names(realized_volatility_list)) {
	colnames(realized_volatility_list[[i]]) = c("date", i)
	realized_volatility_list[[i]]$date = as.character(realized_volatility_list[[i]]$date)
}
realized_volatility_mat = Reduce(function(x,y){join(x,y,by="date",type="left")}, realized_volatility_list)
rownames(realized_volatility_mat) = realized_volatility_mat[["date"]]
realized_volatility_mat = realized_volatility_mat[, !(colnames(realized_volatility_mat) %in% c("date"))]

#matplot(realized_volatility_mat, type="l", xaxt="n")
matplot_mat = realized_volatility_mat[,seq(dim(realized_volatility_mat)[2]-bYears,dim(realized_volatility_mat)[2])]
matplot(matplot_mat, ylab="Realized Volatility", type="l", lty=2, xaxt="n", col=dim(matplot_mat)[2]:1, cex.axis=0.75, cex.lab=0.75)
axis(1, at=1:length(rownames(matplot_mat)), label=rownames(matplot_mat), las=2, cex.axis=0.75)
#find the first non-na value for each column in "matplot_mat"
kk = apply(matplot_mat, 2, function(x){x[which(!is.na(x))[1]]})
text(0, kk, names(matplot_mat), col=dim(matplot_mat)[2]:1, cex=0.75)
grid()

#according to "log_return_threshold", the number of outlier for each contract 
number_of_outlier = data.frame(outlier=unlist(lapply(log_return_list, function(x){sum(x$is_outlier)})))
print(number_of_outlier)

###############################################################################################################
#
#
#xg=lapply(data_cDays_list, function(ds){diff(log(as.numeric(ds$settlement)),lag=1)})
#
#xg[['M1405.DCE']]
#hist(xg[['M1405.DCE']])
#
#xx=density(xg[['M1405.DCE']])
#plot(xx)
##lines(xx)
#
#gg=density(xg[['M1305.DCE']])
#lines(gg)
#
#sm.density.compare(xg)
#
#
#y <- rnorm(100)
#g <- rep(1:2, rep(50,2))
#sm.density.compare(y, g, model="equal")
#
#
#library(sm)
#attach(mtcars)
#
## create value labels 
#cyl.f <- factor(cyl, levels= c(4,6,8),
#		labels = c("4 cylinder", "6 cylinder", "8 cylinder")) 
#
## plot densities 
#sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
#title(main="MPG Distribution by Car Cylinders")
#
## add legend via mouse click
#colfill<-c(2:(2+length(levels(cyl.f)))) 
#legend(locator(1), levels(cyl.f), fill=colfill)
#
#
#tm=data.frame()
#for (i in names(xg)) {
##	print(xg[[i]])
#	tm = rbind(tm, data.frame(log_return=xg[[i]], contract=i))
#}
#sm.density.compare(tm[,1], tm[,2])
##
#plot(unlist(m[1]))
#lines(unlist(m[2]))
#lines(unlist(m[3]))
#plot(unlist(m[4]))
#lines(unlist(m[5]))
#lines(unlist(m[6]))
#lines(unlist(m[7]))
#lines(unlist(m[8]))
#lines(unlist(m[9]))

