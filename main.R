############################################################################################################################################################
# TODO # Add comment
# for "gspinbutton" and "gslider" objects, when calling svalue(obj)= method, need blockHandler(obj) and unblockHandler(obj) to suppress handler being called
############################################################################################################################################################
#check if the working directory is correct
if (tail(unlist(strsplit(getwd(),"/")),n=1) != "r-otc-option") {
	print(paste("Wrong working directory:", getwd()))
	break
}

rm(list=ls(all=TRUE)) 
options(width = 438L)
library(gWidgets2)
options("guiToolkit"="RGtk2")
#options("guiToolkit"="tcltk") # having too many bugs
#library(cairoDevice) # need to load this library before "ggraphics" is called
library(rgl)
library(RQuantLib)
library(WindR) # used in aUpdateMarketData to update current market price
w.start(showmenu=FALSE)
source(paste(getwd(), "/volatility_model.R", sep=""), echo=FALSE, encoding="GBK")
source(paste(getwd(), "/option_mc.R", sep=""), local=TRUE, echo=FALSE, encoding="GBK")
tCalendarList = list(CFFEX="CFE", DCE="DCE", SHFE="SHF")

main_function = function(h,...) {
	print("----- main_function -----")
	tryCatch({
#------------------------------------- create local temp variables -------------------------------------					
#				local variables for "pricing.R"
				exchange = .GlobalEnv$exchange
				underlying = .GlobalEnv$underlying				
				option_style = c("vanilla")				
				if (svalue(aOptionStyle_Geometric)) {
					option_style = c(option_style, "geometric")
				}
				if (svalue(aOptionStyle_Arithmetic)) {
					option_style = c(option_style, "arithmetic")
				}
				
#				group36 items
				contract_year_month = svalue(contract_year_month_obj)				
				x1_itm_otm_pct = as.vector(unlist(x1_x2_itm_otm_pct_obj[1]))
				x2_itm_otm_pct = as.vector(unlist(x1_x2_itm_otm_pct_obj[2]))				
				numerical_siumlation = as.vector(unlist(numerical_siumlation_obj[1]))				
				nSims = numerical_siumlation[1]
				minSteps = numerical_siumlation[2]
				avgRuns = numerical_siumlation[3]		
				
#				aSetReference, .GlobalEnv$paramList[,"reference.XXXX"] = .GlobalEnv$paramList[,"value.XXXX"]
#				aRevertCurrent, svalue(i_obj) = .GlobalEnv$paramList[,"reference.XXXX"]							
#				sync gwidgets objects with local variables, which can also be accessed from "pricing.R"
#				.GlobalEnv$paramList is a global variable, which allows passing between different handlers
#				i <- svalue(i_obj)
#				r_i <- .GlobalEnv$paramList[i]$reference.XXXX
#				.GlobalEnv$paramList[i]$value.XXXX = svalue(i_obj)
				for (i in rownames(.GlobalEnv$paramList)) {
#					only update model parameters for "gspinbutton" and "gslider" objects, does not update "glabel" objects
					if ((.GlobalEnv$paramList[i,]$type=="gspinbutton") | (.GlobalEnv$paramList[i,]$type=="gslider")) {		
						eval(parse(text=paste(i,"=svalue(",i,"_obj)",sep=""))) # sync "_obj" value with gwidget value
						eval(parse(text=paste("r_",i,"=.GlobalEnv$paramList['",i,"',]$reference.",contract_year_month,sep=""))) # reference values
						eval(parse(text=paste(".GlobalEnv$paramList['",i,"',]$value.",contract_year_month,"=svalue(",i,"_obj)",sep="")))	
					}
				}		

#------------------------------------- call "pricing.R" script and generate pricing matix, "f_atm" is automatically updated here -------------------------------------
				if (.GlobalEnv$is_gen_pricing) {
					.GlobalEnv$is_gen_pricing = FALSE
#					call "pricing.R" to generate pricing matrix and export to an external .csv file
					Sys.setlocale(locale="chs")
					source(paste(getwd(), "/pricing.R", sep=""), local=TRUE, echo=FALSE, encoding="GBK")	
#					source(paste(getwd(), "/pricing.R", sep=""), local=TRUE, echo=FALSE, encoding="UTF-8")	
					Sys.setlocale(locale="us")
				}					
				
#------------------------------------- calculate position book and its delta position -------------------------------------						
#				sync "position_gdf_obj_list" with ".GlobalEnv$position_list", "position_gdf_obj_list" is "Object of class GDf", both "complete.cases()==TRUE" and "complete.cases()==FALSE"
				for (expmth in names(position_gdf_obj_list)) {
					expmth_df = data.frame()
					for (pos in 1:dim(position_gdf_obj_list[[expmth]])[1]) {
						expmth_df = rbind(expmth_df, data.frame(position_gdf_obj_list[[expmth]][pos]))
					}	
# 					"selected" is created as a factor in "position_list", need to convert it to "logical" variable
					expmth_df$selected = as.logical(expmth_df$selected)
					.GlobalEnv$position_list[[expmth]] = cbind(expiry_month=expmth, expmth_df)					
				}	
				
#				either auto update position or update position only when "Update Position" is clicked, ".GlobalEnv$combined_greek_list" is a global variable
				if ((.GlobalEnv$is_position_update) | (.GlobalEnv$is_position_auto_update)) {
					.GlobalEnv$is_position_update = FALSE
					.GlobalEnv$combined_greek_list = list()
					source(paste(getwd(), "/position_greeks.R", sep=""), local=TRUE, echo=FALSE, encoding="GBK")					
				}
							
#------------------------------------- wing model, calculation and plot display -------------------------------------
#				strike price vector, calculate X_vec, for both "position delta" and "volatility" plot, "combined_greek_list" only contains valid position entry and "selected" entry
#				for a zero position (e.g., after delta netting), "strike_price" may still exist 
				is_position_exist = is.data.frame(.GlobalEnv$combined_greek_list[[contract_year_month]])				
				X_n_lft = ifelse(is_position_exist, max(X_n, ceiling((f_atm-min(1000000,.GlobalEnv$combined_greek_list[[contract_year_month]]$strike_price))/X_inc)), X_n)		
				X_n_rgt = ifelse(is_position_exist, max(X_n, ceiling((max(-1000000,.GlobalEnv$combined_greek_list[[contract_year_month]]$strike_price)-f_atm)/X_inc)), X_n)		
#				"volatility_model.R", log-moneyness, i.e., transformed strike price
#				f_syn = f_atm ^ (SSR / 100) * f_ref ^ (1 - SSR / 100)	
#				x = (1 / (days / 365)^alpha) * log(X / f_syn)	
				X_atm = round(f_atm / X_inc) * X_inc				
				X_vec = X_atm + c(-rev(seq(1:X_n_lft)),0,seq(1:X_n_rgt)) * X_inc				
				
#				** theoretical volatility **, apply wing model to strike price vector "X_vec"
				vol_theo = apply(matrix(X_vec), 1, volatility_wing_model, 
						days_to_maturity, alpha, f_atm, f_ref, SSR,
						vol_ref, VCR, slope_ref, SCR,
						dn_cf, up_cf, put_curv, call_curv,
						dn_sm, up_sm, dn_slope, up_slope)
#				extract values from "vol_theo", theoretical volatility, log-moneyness
				vol_theo_vec = as.vector(sapply(vol_theo, function(x){unlist(x["theo"])}, simplify=TRUE))		
				
#				** bid/ask offset **
				ba_offset = apply(matrix(X_vec), 1, volatility_wing_model, 
						days_to_maturity, alpha, f_atm, f_ref, SSR,
						vol_ref_offset, VCR_offset, slope_ref_offset, SCR_offset,
						dn_cf_offset, up_cf_offset, put_curv_offset, call_curv_offset,
						dn_sm_offset, up_sm_offset, dn_slope_offset, up_slope_offset)
#				extract values from "ba_offset", offset must be greater than 0
				ba_offset_vec = pmax(0, as.vector(sapply(ba_offset, function(x){unlist(x["theo"])}, simplify=TRUE)))		
#				same offsets for bid and ask, need to adjust "vol_theo" level
				theo_ask_vec = vol_theo_vec + ba_offset_vec
				theo_bid_vec = vol_theo_vec - ba_offset_vec		
				
#				** reference theoretical volatility **, apply wing model to strike price vector "X_vec"
				r_vol_theo = apply(matrix(X_vec), 1, volatility_wing_model, 
						r_days_to_maturity, r_alpha, r_f_atm, r_f_ref, r_SSR,
						r_vol_ref, r_VCR, r_slope_ref, r_SCR,
						r_dn_cf, r_up_cf, r_put_curv, r_call_curv,
						r_dn_sm, r_up_sm, r_dn_slope, r_up_slope)
#				extract values from "vol_theo"
				r_vol_theo_vec = as.vector(sapply(r_vol_theo, function(x){unlist(x["theo"])}, simplify=TRUE))		
				
#				** reference bid/ask offset **
				r_ba_offset = apply(matrix(X_vec), 1, volatility_wing_model, 
						r_days_to_maturity, r_alpha, r_f_atm, r_f_ref, r_SSR,
						r_vol_ref_offset, r_VCR_offset, r_slope_ref_offset, r_SCR_offset,
						r_dn_cf_offset, r_up_cf_offset, r_put_curv_offset, r_call_curv_offset,
						r_dn_sm_offset, r_up_sm_offset, r_dn_slope_offset, r_up_slope_offset)
#				extract values from "ba_offset", offset must be greater than 0
				r_ba_offset_vec = pmax(0, as.vector(sapply(r_ba_offset, function(x){unlist(x["theo"])}, simplify=TRUE)))	
#				same offsets for bid and ask, need to adjust "vol_theo" level
				r_theo_ask_vec = r_vol_theo_vec + r_ba_offset_vec
				r_theo_bid_vec = r_vol_theo_vec - r_ba_offset_vec		
				
#------------------------------------- update "glable" object display value, does not save to .GlobalEnv$paramList -------------------------------------							
#				either auto update position or update position only when "Update Position" is clicked, ".GlobalEnv$combined_greek_list" is a global variable
				if ((.GlobalEnv$is_mm_theo_update) | (.GlobalEnv$is_mm_theo_auto_update)) {
					.GlobalEnv$is_mm_theo_update = FALSE
					Sys.setlocale(locale="chs")					
					source(paste(getwd(), "/market_maker.R", sep=""), local=TRUE, echo=FALSE, encoding="GBK")
					Sys.setlocale(locale="us")					
				}
				
#------------------------------------- update "glable" object display value, does not save to .GlobalEnv$paramList -------------------------------------
#				extract variables and save to local temp variables, which can be access in plots
				for (i in c("f_syn","vol_curr","slope_curr","x1","x2","x0","x3")) {
					eval(parse(text=paste(i,"=vol_theo[[1]]$",i,sep="")))
				}
				for (i in c("vol_curr","slope_curr")) {
					eval(parse(text=paste(i,"_offset=ba_offset[[1]]$",i,sep="")))		
					eval(parse(text=paste("r_",i,"=r_vol_theo[[1]]$",i,sep="")))	
				}				
#				update "glable" object display value
				for (i in c("f_syn","vol_curr","slope_curr","vol_curr_offset","slope_curr_offset")) {
					eval(parse(text=paste("svalue(",i,"_obj)=",i,sep="")))						
				}
				
#------------------------------------- plot -------------------------------------
#				change graphic device tab
#				svalue(notebook_obj) = 1
				names(notebook_obj)[1] = paste(exchange,underlying,contract_year_month,sep=".")
#				switch the plot device for "current" volatility curve
				visible(graphic_volatility_curve) = TRUE				
				plot.new()
				graphic_dev_layout = layout(matrix(c(1,2),nrow=2), heights=c(1,3))
#				xlim is same for both "position delta" and "volatility" plots, while ylim is different, X_vec is calculated in the Wing model calculation
				xlim = range(X_vec)
				
#----------- plot 1: greek, note: par(mar = c(bottom, left, top, right)), combined_greek_list[[contract_year_month]] may not exist (i.e., ylim=c(0,0)) -----------					
				par(mar=c(0,5,1,1))
				ylim = range(c(0, .GlobalEnv$combined_greek_list[[contract_year_month]][paste("position",tolower(svalue(aPositionGreeks)),sep="_")]))		
				plot(X_vec, rep(0,length(X_vec)), xlim=xlim, ylim=ylim, xlab="", ylab=paste("Position",svalue(aPositionGreeks)), type="l", xaxt="n", cex.axis=0.75, cex.lab=0.75)		
				if (is_position_exist) {
					lines(.GlobalEnv$combined_greek_list[[contract_year_month]][,"strike_price"], .GlobalEnv$combined_greek_list[[contract_year_month]][,paste("position",tolower(svalue(aPositionGreeks)),sep="_")], type="h")
				}
				greek_sum = unlist(lapply(list(delta="delta",vega="vega"), function(x){sum(.GlobalEnv$combined_greek_list[[contract_year_month]][,paste("position",x,sep="_")])}))
				legend("topleft", legend=paste(names(greek_sum),round(greek_sum,4),sep="="), bty="n", cex=0.75)
				grid()		
				
#----------- plot 2: volatility-----------
				par(mar=c(5,5,0,1))
				ylim = range(c(theo_ask_vec,theo_bid_vec,r_theo_ask_vec,r_theo_bid_vec))		
				plot(X_vec, vol_theo_vec, xlim=xlim, ylim=ylim, xlab="", ylab="Volatility", type="b", lty=1, pch=16, col="black", xaxt="n", cex.axis=0.75, cex.lab=0.75)	
				
#				buttom x-axis display as "Strike Price (Delta)"
				option_call = mapply(EuropeanOption, "call", f_atm, X_vec, 0, risk_free_interest, days_to_maturity/365, vol_theo_vec)
				call_delta_vec = as.vector(unlist(option_call["delta",]))				
				delta_100_vec = round(c(1-call_delta_vec[call_delta_vec>=0.50], call_delta_vec[call_delta_vec<0.50]) * 100)				
				ltb = ifelse(call_delta_vec>=0.50, "[", "(")
				rtb = ifelse(call_delta_vec>=0.50, "]", ")")				
				axis(1, at=X_vec, labels=paste(X_vec,ltb,delta_100_vec,rtb,sep=""), las=2, cex.axis=0.75)
				mtext("Strike Price (Delta)", side=1, line=-1, adj=0, col="red", cex=0.75)
				grid()				
				
#				theoretical mid / bid / ask volatility, i.e., mid-point volatility, theoretical volatility at strike prices 
				lines(X_vec, vol_theo_vec, type="b", lty=1, pch=16, col="black")
				lines(X_vec, theo_ask_vec, type="b", lty=2, pch=6, col="black")
				lines(X_vec, theo_bid_vec, type="b", lty=2, pch=2, col="black")					
#				reference mid / bid / ask volatility
				lines(X_vec, r_vol_theo_vec, type="b", lty=1, pch=20, col="grey")
				lines(X_vec, r_theo_ask_vec, type="b", lty=2, pch=6, col="grey")
				lines(X_vec, r_theo_bid_vec, type="b", lty=2, pch=2, col="grey")	
				
#				volatility of strike prices in the "pricing.R" script
				cfpath = paste(getwd(), "/pricing/", paste("strike_matrix", exchange, underlying, contract_year_month, "call", sep="."), ".csv", sep="")
				pfpath = paste(getwd(), "/pricing/", paste("strike_matrix", exchange, underlying, contract_year_month, "put" , sep="."), ".csv", sep="")				
				if (file.exists(cfpath) & file.exists(pfpath)) {								
#					read in strike price matrix generated by "pricing.R" script
					strike_matrix_call = as.matrix(read.csv(cfpath, header=TRUE, row.names=1, check.names=FALSE, na.strings="NA", sep=","))
					strike_matrix_put  = as.matrix(read.csv(pfpath, header=TRUE, row.names=1, check.names=FALSE, na.strings="NA", sep=","))
					strike_price_vec = as.vector(unique(matrix(cbind(strike_matrix_call, strike_matrix_put),ncol=1)))					
#					** theoretical volatility **, apply wing model to strike price vector "strike_price_vec"
					strike_price_vol_theo = apply(matrix(strike_price_vec), 1, volatility_wing_model, 
							days_to_maturity, alpha, f_atm, f_ref, SSR,
							vol_ref, VCR, slope_ref, SCR,
							dn_cf, up_cf, put_curv, call_curv,
							dn_sm, up_sm, dn_slope, up_slope)
#					extract values from "vol_theo"
					strike_price_vol_theo_vec = as.vector(sapply(strike_price_vol_theo, function(x){unlist(x["theo"])}, simplify=TRUE))						
#					draw volatility of those strike prices in "pricing.R" on main plot
					points(strike_price_vec, strike_price_vol_theo_vec, pch="|", col="black")
				}
				
#				slope handle for both mid-point, bid/ask volatility, and reference mid-point volatility
				lines(f_syn*exp((days_to_maturity/365)^alpha*c(-0.05,0.05)), vol_curr+slope_curr*c(-0.05,0.05), type="b", lty=1, pch=16, col="black")
				lines(f_syn*exp((days_to_maturity/365)^alpha*c(-0.05,0.05)), vol_curr+vol_curr_offset+(slope_curr+slope_curr_offset)*c(-0.05,0.05), type="b", lty=2, pch=1, col="black")			
				lines(f_syn*exp((days_to_maturity/365)^alpha*c(-0.05,0.05)), r_vol_curr+r_slope_curr*c(-0.05,0.05), type="b", lty=1, pch=20, col="grey")
				
#				ATM, reference, synthetic forward price
				abline(v=f_atm, col="green4", pch=22, lty=2)			
				abline(v=f_ref, col="purple", pch=22, lty=2)
				abline(v=f_syn, col="black", pch=22, lty=1)				
				legend("bottomright", legend=c("ATM F. Price","R. Price","Syn. F. Price"), text.col=c("green4","purple","black"), lty=c(2,2,1), col=c("green4","purple","black"), cex=0.75)
				
#				wing model range
				abline(v=f_syn*exp(x0), col="blue", pch=22, lty=1)
				abline(v=f_syn*exp(x1), col="blue", pch=22, lty=1)
				abline(v=f_syn*exp(x2), col="red",  pch=22, lty=1)
				abline(v=f_syn*exp(x3), col="red",  pch=22, lty=1)					
#				xpos = (c(head(x_vec,n=1),x0,x1,0,x2,x3) + c(x0,x1,0,x2,x3,tail(x_vec,n=1))) / 2
#				text(f_syn*exp(xpos), rep(vol_curr,length(xpos)), c("Down Affine","Down Smoothing","Put Wing","Call Wing","Up Smoothing","Up Affine"), col=c("blue","blue","blue","red","red","red"), cex=0.75)
				
#				export plot to a file
				pfilename = paste(getwd(), "/pricing/volatility_", paste(exchange,underlying,contract_year_month,sep="."), "_", Sys.Date(), ".png", sep="")
				dev.print(png, width=480, height=640, filename=pfilename)				
								
			}, warning = function(war) {
				print(war)
			}, error = function(err) {
				print(err)				
			}, finally = {				
			})		
#	print("----- end of main_function -----")
}

############################################################################################################################################################
# TODO # create GUI
############################################################################################################################################################
#read paramters file, which specifies the properties of each control objects
#fpath = paste(getwd(), "/params/", paste("paramList", exchange, underlying, sep="."), ".csv", sep="")
fpath = file.choose(new = FALSE)
fname_vec = unlist(strsplit(basename(fpath), "[.]"))
exchange = fname_vec[2]
underlying = fname_vec[3]
paramList = read.csv(fpath, header=TRUE, row.names=1, na.strings="NA", sep=",")

#get the contract_year_month vector
paramList_col = colnames(paramList)
contract_year_month_vec = sapply(strsplit(paramList_col[grepl("value",paramList_col)], "[.]"), function(x){unlist(x)[2]}, simplify=TRUE)

#used in "position_greeks.R" for calculation of combined risks at different strike prices
combined_greek_list = list()

#=======================================================================================================================================
# TODO # main window frame, widgets groups and layout
#=======================================================================================================================================
#------------------------ main window frame, widgets groups and layout ------------------------
#main window
window = gwindow(paste("Volatility Model ",exchange,".",underlying,sep=""), width=1400, height=780)
#top level group
mainGroup = ggroup(horizontal=TRUE, container=window)
#"group1" is the paramters area, graphic notebook is in "group2"
group1 = ggroup(horizontal=TRUE, container=mainGroup)
group2 = ggroup(horizontal=FALSE, container=mainGroup, expand=TRUE)
#"group3" is only for "class=price" widgets, "group4" is for other widgets class, i.e., volatility, slope, wing, smoothing, affine
group3 = ggroup(horizontal=FALSE, container=group1)
group4 = ggroup(horizontal=TRUE, container=group1)
#within "group35" and "group36", widgets are placed vertically, both are within "group3"
group35 = ggroup(horizontal=FALSE, container=group3)
group36 = ggroup(horizontal=FALSE, container=group3, expand=TRUE)
#within "group45" and "group46", widgets are placed vertically, both are within "group4"
group45 = ggroup(horizontal=FALSE, container=group4)
group46 = ggroup(horizontal=FALSE, container=group4)

#------------------------ create objects from "paramList", i.e., model parameters ------------------------
#control parameters
func_params = c("value", "from", "to", "by", "digits")
func_params2 = c(paste("value.",contract_year_month_vec[1],sep=""), "from", "to", "by", "digits")

for (i in rownames(paramList)) {
#	create gWidgets objects	
	if ((paramList[i,]$type=="gspinbutton") | (paramList[i,]$type=="gslider")) {		
		evalstr = paste(i, "_obj=", paramList[i,]$type, "(", paste(func_params,"=",as.numeric(paramList[i,][func_params2]),sep="",collapse=", "), ", handler=main_function)", sep="")		
	} else if (paramList[i,]$type=="glabel") {		
		evalstr = paste(i, "_obj=", paramList[i,]$type, "(", "''", ", editable=FALSE)", sep="")
	} else {			
	}
#	print(evalstr)
#	X_inc_obj=gspinbutton(value=5, from=1, to=1e+06, by=1, digits=0, handler=main_function)
	eval(parse(text=evalstr))
	
#	"visible" property of a gwidget object, add objects to main window frame, a gwidget object may not be visible, but exists, i.e., svalue() returns a value
	if (paramList[i,]$visible == TRUE) {
		evalstr2 = paste("add(gframe('", paramList[i,]$description, "', container=group", paramList[i,]$group, "), ", i, "_obj, ", paramList[i,]$type_spec, ")", sep="")
#		print(evalstr2)
#		add(gframe('Strike Price Increment', container=group35), X_inc_obj, NA)
		eval(parse(text=evalstr2))
	}
	
#	"enabled" property of a gwidget object
	eval(parse(text=paste("enabled(", i, "_obj)=", paramList[i,]$enabled, sep="")))
}

#=======================================================================================================================================
# TODO # container=group36
#=======================================================================================================================================
#------------------------ contract_year_month, "gcombobox" ------------------------
contract_year_month_gframe_obj = gframe('Contract Expiration Month', container=group36, expand=FALSE)
contract_year_month_obj = gcombobox(contract_year_month_vec, editable=TRUE, container=contract_year_month_gframe_obj, handler=function(h,...) {				
#			only update model parameters for "gspinbutton" and "gslider" objects, does not update "glabel" objects			
#			svalue(i_obj) <- .GlobalEnv$paramList[i]$value.XXXX
			for (i in rownames(.GlobalEnv$paramList)) {
				if ((.GlobalEnv$paramList[i,]$type=="gspinbutton") | (.GlobalEnv$paramList[i,]$type=="gslider")) {	
#					need to blockHandler() then unblockHandler() for an object, otherwise, its "handler" function will be triggered
					eval(parse(text=paste("blockHandler(",i,"_obj)",sep="")))
					eval(parse(text=paste("svalue(",i,"_obj)=.GlobalEnv$paramList['",i,"',]$value.",svalue(h$obj),sep="")))
					eval(parse(text=paste("unblockHandler(",i,"_obj)",sep="")))
				}
			}
			main_function()
		}
)

#------------------------ number of ITM, OTM strikes and change pct, for both X1 and X2, "gdf" ------------------------
numerical_siumlation_gframe_obj = gframe('Numerical Simulation', container=group36, expand=FALSE)
numerical_siumlation_obj = gdf(matrix(c(100,500,1), nrow=1, byrow=TRUE, dimnames=list(c("V"),c("nSims","minSteps","avgRuns"))), container=numerical_siumlation_gframe_obj)

#------------------------ number of ITM, OTM strikes and change pct, for both X1 and X2, "gdf" ------------------------
x1_x2_itm_otm_pct_gframe_obj = gframe('Pricing Strikes X1', container=group36, expand=TRUE)
x1_x2_itm_otm_pct_obj = gdf(matrix(c(0,3,0.05,0,1,0.1), nrow=2, byrow=TRUE, dimnames=list(c("X1","X2"),c("ITM","OTM","Pct"))), container=x1_x2_itm_otm_pct_gframe_obj)

#=======================================================================================================================================
# TODO # container=notebook_obj
#=======================================================================================================================================
#------------------------ adding plot area, "ggraphics" ------------------------
#include plot window in the "group2", so that the plot can use rest of the space in the main window
notebook_obj = gnotebook(container=group2, expand=TRUE)

#------------------------ volatility curve and volatility surface ------------------------
graphic_volatility_curve = ggraphics(container=notebook_obj, visible=TRUE)
#volatility_surface_dev = ggraphics(container=graphic_notebook_obj, label="Volatility Surface", visible=TRUE)

#------------------------ theoretical prices and volatility, "gtable" ------------------------
#used in "market_maker.R" script, market maker tab, for calculation of theoretical prices, greeks and volatility
market_maker_group_obj = gpanedgroup(horizontal=TRUE, container=notebook_obj, expand=TRUE)
#pVega, pDelta, pBid, pMid, pAsk, strike, cBid, cMid, cAsk, cDelta, cVega
greeks_gframe_obj = gframe("Price & Greeks", container=market_maker_group_obj, expand=TRUE)	
greeks_gtable_obj = gtable(data.frame(), container=greeks_gframe_obj)		
#strike, volBid, volMid, volAsk
volatility_gframe_obj = gframe("Volatility %", container=market_maker_group_obj, expand=TRUE)		
volatility_gtable_obj = gtable(data.frame(), container=volatility_gframe_obj)		
#set the ratio of left and right pane
names(notebook_obj)[2] = "Market Maker"
svalue(market_maker_group_obj) = 0.7

#------------------------ Position & Risk, "gdf", "gtable" ------------------------
#create a ggroup obj and include it inside the "notebook_obj"
position_risk_group_obj = gpanedgroup(horizontal=TRUE, container=notebook_obj, expand=TRUE)
position_group_obj = ggroup(horizontal=FALSE, container=position_risk_group_obj, expand=TRUE)
risk_group_obj = ggroup(horizontal=FALSE, container=position_risk_group_obj, expand=TRUE)

#----- position -----
#read.csv all entry from the ".csv" file, and display in the gdf, but when export, only export "complete.cases()==TRUE" entry
fpath = paste(getwd(), "/params/", paste("positionList", exchange, underlying, sep="."), ".csv", sep="")
position_data = read.csv(fpath, header=TRUE, na.strings="NA", sep=",")
#1).split "position_data" with factor "expiry_month", "position_data" -> temp_list; 
#2).sort "x" w.r.t "strike_price", drop the first column "expiry_month", set the rownames to "1:dim(x)[1]"
temp_list = lapply(split(position_data,position_data$expiry_month), function(x){return(data.frame(x[with(x,order(strike_price)),-1],row.names=1:dim(x)[1]))})

#".GlobalEnv$position_list" is used in "position_greeks.R" script, and its value can be modified through "gdf"
position_list = list()
for (expmth in contract_year_month_vec) {
	if (!is.data.frame(temp_list[[expmth]])) {
		position_list[[expmth]] = data.frame(selected=factor(FALSE, levels=c(TRUE,FALSE)), strike_price=NA, type=factor(c("call"),levels=c("call","put","future","cash")), style=factor(c("vanilla"),levels=c("vanilla","geometric","arithmetic")), position=0)
	} else {
		position_list[[expmth]] = temp_list[[expmth]]
		position_list[[expmth]]$selected = factor(temp_list[[expmth]]$selected, levels=c(TRUE,FALSE))
		position_list[[expmth]]$type = factor(temp_list[[expmth]]$type, levels=c("call","put","future","cash"))
		position_list[[expmth]]$style = factor(temp_list[[expmth]]$style, levels=c("vanilla","geometric","arithmetic"))		
	}
}
#create a list which contains all the gdf objects, add gdf objects to notebook_obj with a gframe object around it
position_gframe_obj_list = list()
position_gdf_obj_list = lapply(position_list, function(x){gdf(x)})
for (expmth in names(position_gdf_obj_list)) {
	position_gframe_obj_list[[expmth]] = gframe(paste('Expiry Month = ',expmth,sep=""), container=position_group_obj, expand=TRUE)	
	add(position_gframe_obj_list[[expmth]], position_gdf_obj_list[[expmth]], expand=TRUE)
}

#----- risk -----
#".GlobalEnv$risk_list" is updated in "position_greeks.R" script, its value can not be modified
risk_list = lapply(position_list, function(x){
			m = cbind(x[,c("strike_price","type","style","position")],value=0,delta=0,vega=0,position_delta=0,position_vega=0)
			return(m[complete.cases(m),])
		}
)
risk_gframe_obj_list = list()		
risk_gtable_obj_list = lapply(risk_list, function(x){gtable(x)})
for (expmth in names(risk_gtable_obj_list)) {
	risk_gframe_obj_list[[expmth]] = gframe(paste('Expiry Month = ',expmth,sep=""), container=risk_group_obj, expand=TRUE)
	add(risk_gframe_obj_list[[expmth]], risk_gtable_obj_list[[expmth]], expand=TRUE)
}

#----- set the ratio of left and right pane -----
names(notebook_obj)[3] = "Position & Risk"
svalue(position_risk_group_obj) = 0.35

#=======================================================================================================================================
# TODO # menu bar list
#=======================================================================================================================================
#----- live data -----
MdUpdate = function(data) {
#	get the latest price and set f_atm
	contract_year_month = svalue(contract_year_month_obj)						
	wsqdata = w.wsq(paste(underlying,contract_year_month,".",tCalendarList[exchange],sep=""), "rt_latest")	
	print(wsqdata$Data)
#	update "f_atm_obj", "f_atm" is updated in main_function()
	blockHandler(f_atm_obj)
	svalue(f_atm_obj) = wsqdata$Data$RT_LATEST
	unblockHandler(f_atm_obj)		
	main_function()
}
aUpdateMarketData = gaction(label="Update Market Data", handler=function(h,...) {
			MdUpdate()
		}
)
##update market price every second, need to stop timer in "addHandlerUnrealize" before exiting the program
#md_update_gtimer_obj = gtimer(1000, MdUpdate, data=NULL, one.shot=FALSE, start=FALSE)
#aMdAutoUpdate = gcheckbox("Auto Update", checked=FALSE, handler=function(h,...) {
#			if (svalue(h$obj)) {
#				md_update_gtimer_obj$start_timer()
#			} else {
#				md_update_gtimer_obj$stop_timer()
#			}
#		}
#)
#----- Volatility -----
#set the reference volatility curve to current volatility curve, .GlobalEnv$paramList[,"reference.XXXX"] = .GlobalEnv$paramList[,"value.XXXX"]
aSetReference = gaction(label="Set Reference", handler=function(h,...) {
#			print(paste("aSetReference = ", svalue(contract_year_month_obj),sep=""))			
			contract_year_month = svalue(contract_year_month_obj)
			.GlobalEnv$paramList[,paste("reference.",contract_year_month,sep="")] = .GlobalEnv$paramList[,paste("value.",contract_year_month,sep="")]
			main_function()
		}
)
#revert current volatility curve to reference curve, svalue(i_obj) = .GlobalEnv$paramList[,"reference.XXXX"]
aRevertCurrent = gaction(label="Revert Current", handler=function(h,...) {
#			print(paste("aRevertCurrent = ", svalue(contract_year_month_obj),sep=""))
			contract_year_month = svalue(contract_year_month_obj)
			for (i in rownames(.GlobalEnv$paramList)) {
				if ((.GlobalEnv$paramList[i,]$type=="gspinbutton") | (.GlobalEnv$paramList[i,]$type=="gslider")) {	
#					need to blockHandler() then unblockHandler() for an object, otherwise, its "handler" function will be triggered
					eval(parse(text=paste("blockHandler(",i,"_obj)",sep="")))
					eval(parse(text=paste("svalue(",i,"_obj)=.GlobalEnv$paramList['",i,"',]$reference.",contract_year_month,sep="")))
					eval(parse(text=paste("unblockHandler(",i,"_obj)",sep="")))
				}
			}		
#			.GlobalEnv$paramList[,"value.XXXX"] will be updated in main_function()
			main_function()
		}
)
#Volatility Surface Plot, in order to explore the term structure of volatility curves for different expirations
aVolatilitySurface = gaction(label="Volatility Surface Plot", handler=function(h,...) {
			print("aVolatilitySurface")
			
			
#			value = paramList[,paramList_col[grepl("value",paramList_col)]]
#			
#			X_vec = unique(as.vector(apply(matrix(unlist(value["X_atm",])), 1, function(x,y,z){x+c(-rev(seq(1:y)),0,seq(1:y))*z}, max(value["X_n",]), max(value["X_inc",]))))
#			
#			vol_theo = mapply(volatility_wing_model, matrix(rep(X_vec,dim(value)[2]),ncol=dim(value)[2]),
#					value["days_to_maturity",], value["alpha",], value["f_atm",], value["f_ref",], value["SSR",],
#					value["vol_ref",], value["VCR",], value["slope_ref",], value["SCR",],
#					value["dn_cf",], value["up_cf",], value["put_curv",], value["call_curv",],
#					value["dn_sm",], value["up_sm",], value["dn_slope",], value["up_slope",])
#			
#			vol_theo_matrix = matrix(as.vector(unlist(vol_theo["theo",])), nrow=3)
#			
#			
#			year = c(2010,2011,2012)
#			
#			plot3d(X_vec, year, vol_theo_matrix)
		}
)
#----- market maker -----
#option style "Geometric" and "Arithmetic"
aOptionStyle_Geometric = gcheckbox("Geometric", checked=TRUE, handler=function(h, ...) {
		}
)
aOptionStyle_Arithmetic = gcheckbox("Arithmetic", checked=FALSE, handler=function(h, ...) {
		}
)
#click to run "pricing.R" script
.GlobalEnv$is_gen_pricing = FALSE
aGeneratePricing = gaction(label="Generate Pricing", handler=function(h,...) {
			.GlobalEnv$is_gen_pricing = TRUE
			main_function()
		}
)
#"market_maker.R" script takes a long time to run, no need to update on every parameter change
.GlobalEnv$is_mm_theo_auto_update = FALSE
aTheoAutoUpdate = gcheckbox("Auto Update", checked=FALSE, handler=function(h,...) {
			.GlobalEnv$is_mm_theo_auto_update = svalue(h$obj)
		}
)
#after modify all volatility model parameters, need to click this to call "main_function()" to update the theoretical volatility
.GlobalEnv$is_mm_theo_update = TRUE
aTheoUpdate = gaction(label="Update Theoretical", handler=function(h,...) {
			.GlobalEnv$is_mm_theo_update = TRUE
			main_function()
		}
)
#----- Position -----
#greek risk plot
aPositionGreeks = gradio(c("Vega"), handler=function(h, ...) {
			main_function()
		}
)
#"position_greeks.R" script takes a long time to run, no need to update on every parameter change
.GlobalEnv$is_position_auto_update = FALSE
aPositionAutoUpdate = gcheckbox("Auto Update", checked=FALSE, handler=function(h,...) {
			.GlobalEnv$is_position_auto_update = svalue(h$obj)
		}
)
#after update all the data in gdf, need to click this to call "main_function()" to update the "position_list"
.GlobalEnv$is_position_update = TRUE
aPositionUpdate = gaction(label="Update Position", handler=function(h,...) {
			.GlobalEnv$is_position_update = TRUE
			main_function()
		}
)
#----- add menu bar to main window -----
menu_bar_list = list(
#		"Market Data"=list(aUpdateMarketData, aMdAutoUpdate),
		"Market Data"=list(aUpdateMarketData),
		"Volatility"=list(aSetReference, aRevertCurrent, gseparator(), aVolatilitySurface), 
		"Market Maker"=list(aOptionStyle_Geometric, aOptionStyle_Arithmetic, gseparator(), aGeneratePricing, gseparator(), aTheoAutoUpdate, aTheoUpdate),
		"Position & Risk"=list(aPositionGreeks, gseparator(), aPositionAutoUpdate, aPositionUpdate)
)
menu_bar = gmenu(menu_bar_list, container=window)

#=======================================================================================================================================
# TODO # save varialbe list routine function
#=======================================================================================================================================
window_save_var = function(h) {
#	main_function()
#	choose whether export parameter list to an external file
	gbd_obj = gbasicdialog(title="Save variables and exit program", parent=h$obj, handler=function(h...) {									
				save_var_list = (svalue(save_var_list_obj))									
				if (any(save_var_list=="Parameter")) {
					fpath = paste(getwd(), "/params./", paste("paramList", .GlobalEnv$exchange, .GlobalEnv$underlying, sep="."), ".csv", sep="")
					write.csv(.GlobalEnv$paramList, fpath, quote=FALSE, row.names=TRUE)
					print(paste("Parameter list saved location: ",fpath,sep=""))
				} 									
				if (any(save_var_list=="Position")) {
					fpath = paste(getwd(), "/params/", paste("positionList", .GlobalEnv$exchange, .GlobalEnv$underlying, sep="."), ".csv", sep="")					
					temp = do.call("rbind",.GlobalEnv$position_list)[c("expiry_month","selected","strike_price","type","style","position")] # discard other columns, e.g., delta, vega, etc.
					write.csv(temp[complete.cases(temp),], fpath, quote=FALSE, row.names=FALSE) # only export complete.cases()==TRUE entry		
					print(paste("Position list saved location: ",fpath,sep=""))
				} 							
			}
	)
	save_var_list_obj = gcheckboxgroup(c("Parameter","Position"), horizontal=FALSE, checked=c(TRUE,TRUE), container=gbd_obj)
	return (visible(gbd_obj,set=TRUE))
}
addHandlerUnrealize(window, handler=function(h,...) {
			tryCatch({		
						if (window_save_var(h)) {
#							md_update_gtimer_obj$stop_timer()							
							print("Exit Volatility Model Successfully")
							return(FALSE) # destroy
						} else {
							return(TRUE) # don't destroy	
						}						
					}, warning = function(war) {
						print(war)
					}, error = function(err) {
						print(err)				
					}, finally = {			
					})				
		}
)
#hot key: Ctrl+s = "\023", or Command+s for mac, only save paramList and positionList, does not exit program
addHandlerKeystroke(window, function(h,...) {
			if(h$key=="\023") {
				window_save_var(h)
			}
		}
)

#------------------------ call main function ------------------------
main_function()

#delete(risk_gframe_obj_list[['1501']], risk_gtable_obj_list[['1501']])
#risk_gtable_obj_list[['1501']] = gtable(risk_list[['1501']], container=risk_gframe_obj_list[['1501']])
#
#delete(risk_gframe_obj_list[['1509']], risk_gtable_obj_list[['1509']])
#risk_gtable_obj_list[['1509']] = gtable(risk_list[['1501']], container=risk_gframe_obj_list[['1509']])

##					update "f_atm_obj" and "f_atm" 
#					blockHandler(f_atm_obj)
#					svalue(f_atm_obj) = atm
#					f_atm = atm
#					unblockHandler(f_atm_obj)

##------------------------------------- save to .GlobalEnv$paramList, which is global variable, allows passing between different handlers -------------------------------------
##				.GlobalEnv$paramList[i]$value.XXXX = svalue(i_obj)
#				for (i in rownames(.GlobalEnv$paramList)) {
#					if ((.GlobalEnv$paramList[i,]$type=="gspinbutton") | (.GlobalEnv$paramList[i,]$type=="gslider")) {	
#						eval(parse(text=paste(".GlobalEnv$paramList['",i,"',]$value.",contract_year_month,"=svalue(",i,"_obj)",sep="")))	
#					}
#				}
#			if (svalue(h$obj)) {
#				.GlobalEnv$is_mm_theo_auto_update = TRUE
#				main_function()
#			} else {
#				.GlobalEnv$is_mm_theo_auto_update = FALSE
#			}
#			if (svalue(h$obj)) {
#				.GlobalEnv$is_position_auto_update = TRUE
#				main_function()
#			} else {
#				.GlobalEnv$is_position_auto_update = FALSE
#			}