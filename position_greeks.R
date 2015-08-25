##############################################################################################################################################################
# TODO: Add comment
##############################################################################################################################################################
#print(paste(getwd(), "/position_greeks.R", sep=""))
source(paste(getwd(), "/option_mc.R", sep=""), local=TRUE, echo=FALSE, encoding="GBK")

for (i in names(.GlobalEnv$position_list)) {	
#	".GlobalEnv$position_list" may contain complete.cases()==FALSE entry, only include complete.cases()==TRUE entry, in order to make sure the calculation of theo_vol and theo valid
	temp = .GlobalEnv$position_list[[i]][complete.cases(.GlobalEnv$position_list[[i]]),]	
#	print(.GlobalEnv$position_list[[i]])
#	print(temp)
	if (dim(temp)[1] > 0) {
#		initial values for delta and vega related variables
		temp[,c("value","delta","vega","position_delta","position_vega")] = 0		
		for (j in 1:dim(temp)[1]) {		
			if ((temp[j,]$type=="call")|(temp[j,]$type=="put")) {
#				".GlobalEnv$paramList" is synced, "p_" is for position, v.s. "r_" is for reference				
				for (k in rownames(.GlobalEnv$paramList)) {
					if ((.GlobalEnv$paramList[k,]$type=="gspinbutton") | (.GlobalEnv$paramList[k,]$type=="gslider")) {	
						eval(parse(text=paste("p_",k,"=.GlobalEnv$paramList['",k,"',]$value.",i,sep="")))							
					}
				}	
#				generate volatility curve
				p_vol_theo = volatility_wing_model(temp[j,]$strike_price, 
						p_days_to_maturity, p_alpha, p_f_atm, p_f_ref, p_SSR,
						p_vol_ref, p_VCR, p_slope_ref, p_SCR,
						p_dn_cf, p_up_cf, p_put_curv, p_call_curv,
						p_dn_sm, p_up_sm, p_dn_slope, p_up_slope)
#				use different option pricing method to calculate the Greeks
				if (temp[j,]$style=="vanilla") {
#					EuropeanOption(type, underlying, strike, dividendYield, riskFreeRate, maturity, volatility)						
					opt = EuropeanOption(as.character(temp[j,]$type), p_f_atm, temp[j,]$strike_price, 0, risk_free_interest, days_to_maturity/365, p_vol_theo$theo)
				} else if (temp[j,]$style=="geometric") {
#					AsianOption(averageType="geometric", type="call", underlying=S, strike=X, dividendYield=q, riskFreeRate=r, maturity=cDays/365, volatility=volatility)						
					opt = AsianOption(averageType="geometric", as.character(temp[j,]$type), p_f_atm, temp[j,]$strike_price, 0, risk_free_interest, days_to_maturity/365, p_vol_theo$theo)
				} else if (temp[j,]$style=="arithmetic") {
#					OptionMC_vec("arithmetic", S, X, r, q, cDays, volatility, nSims, minSteps, avgRuns, c("sigma","call_value","call_delta","put_value","put_delta"))					
					opt = OptionMC_vec("arithmetic", p_f_atm, temp[j,]$strike_price, risk_free_interest, 0, days_to_maturity, p_vol_theo$theo, nSims, minSteps, avgRuns, c("call_value","call_delta","call_vega","put_value","put_delta","put_vega"))					
					opt$value = opt[paste(as.character(temp[j,]$type),"value",sep="_")]
					opt$delta = opt[paste(as.character(temp[j,]$type),"delta",sep="_")]
					opt$vega = opt[paste(as.character(temp[j,]$type),"vega",sep="_")]
				} else {
					print(paste("unknow style: ", paste(colnames(temp[j,]), temp[j,], sep="=", collapse=" | "), sep=""))
				}						
#				add "value", "delta" and "vega" to the "temp" data.frame
				temp[j, c("value","delta","vega")] = c(opt$value, opt$delta, opt$vega)								
			} else if ((temp[j,]$type=="future")|(temp[j,]$type=="cash")) {
				temp[j, c("value","delta","vega")] = c(0, 1, 0)
			} else {
				print(paste("unknow type: ", paste(colnames(temp[j,]), temp[j,], sep="=", collapse=" | "), sep=""))
			}	
		}	
#		calculate position delta and vega for this expiry month, vega is expressed in terms of per 1% change
		temp$position_delta = temp$position * temp$delta
		temp$position_vega = temp$position * temp$vega * 0.01	

#		refresh update (i.e., delete/create) the greeks of "risk_gtable_obj_list" in gtbale objects, ggg is a temp varible for display in gtable
		.GlobalEnv$risk_list[[i]] = temp[,!(names(temp) %in% c("expiry_month","selected"))]			
		ggg = .GlobalEnv$risk_list[[i]]
		ggg[c("value","delta","vega","position_delta","position_vega")] = round(ggg[c("value","delta","vega","position_delta","position_vega")], 4)			
		delete(.GlobalEnv$risk_gframe_obj_list[[i]], .GlobalEnv$risk_gtable_obj_list[[i]])
		.GlobalEnv$risk_gtable_obj_list[[i]] = gtable(ggg, container=.GlobalEnv$risk_gframe_obj_list[[i]])				
		gtkTreeViewSetGridLines(getToolkitWidget(.GlobalEnv$risk_gtable_obj_list[[i]]), grid.lines=3)
		
#		combined_greek_list = list() is for selected position only, used in plotting greeks and portfolio analysis
		temp_selected = temp[temp$selected,]
		if (dim(temp_selected)[1] > 0) {
			.GlobalEnv$combined_greek_list[[i]] = aggregate(cbind(temp_selected$position_delta,temp_selected$position_vega), by=list(temp_selected$strike_price), FUN=sum)		
			colnames(.GlobalEnv$combined_greek_list[[i]]) = c("strike_price","position_delta","position_vega")			
		} else {
			print(paste("there is no selected entry in expiry month:", i))
#			print(.GlobalEnv$combined_greek_list[[i]])
		}
	} else {	
		print(paste("there is no complete.case()==TRUE entry in expiry month:", i))
	}
}




