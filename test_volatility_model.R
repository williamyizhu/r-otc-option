############################################################################################################################################################
# TODO # Add comment
############################################################################################################################################################
rm(list=ls(all=TRUE)) 
options(width = 438L)
source(paste(getwd(), "/volatility_model.R", sep=""), echo=FALSE, encoding="GBK")

#=============================================================================================================================================================
# TODO # function: volatility_wing_model
#=============================================================================================================================================================
X_atm = 3000
X_inc = 50
X_n = 15

days_to_maturity = 60
alpha = 0

f_atm = 2950
f_ref = 3000
#f_syn = NA

SSR = 50
vol_ref = 0.2
VCR = 0
#vol_curr = NA

slope_ref = 0
SCR = 0
#slope_curr = NA

put_curv = 10
call_curv = 10
dn_cf = -0.1
up_cf = 0.1
dn_sm = 1
up_sm = 1
dn_slope = 0.0001
up_slope = 0.0001

X = 3000

vol_theo = volatility_wing_model(X, days_to_maturity, alpha, f_atm, f_ref, SSR,
		vol_ref, VCR, slope_ref, SCR,
		dn_cf, up_cf, put_curv, call_curv,
		dn_sm, up_sm, dn_slope, up_slope)

print(vol_theo)


#strike price vector
X_vec = X_atm + c(-rev(seq(1:X_n)),0,seq(1:X_n)) * X_inc

#** theoretical volatility **, apply wing model to strike price vector "X_vec"
vol_theo = apply(matrix(X_vec), 1, volatility_wing_model, 
		days_to_maturity, alpha, f_atm, f_ref, SSR,
		vol_ref, VCR, slope_ref, SCR,
		dn_cf, up_cf, put_curv, call_curv,
		dn_sm, up_sm, dn_slope, up_slope)

#extract values from "vol_theo"
vol_theo_vec = as.vector(sapply(vol_theo, function(x){unlist(x["theo"])}, simplify=TRUE))		

#=============================================================================================================================================================
# TODO # function: volatility_wing_model_vec
#=============================================================================================================================================================



volatility_wing_model_vec = function(X_vec, days, alpha, f_atm, f_ref, SSR,
		vol_ref, VCR, slope_ref, SCR,
		dn_cf, up_cf, put_curv, call_curv,
		dn_sm, up_sm, dn_slope, up_slope) {	
#	** theoretical volatility **, apply wing model to strike price vector "X_vec"
	vol_theo = apply(matrix(X_vec), 1, volatility_wing_model, 
			days_to_maturity, alpha, f_atm, f_ref, SSR,
			vol_ref, VCR, slope_ref, SCR,
			dn_cf, up_cf, put_curv, call_curv,
			dn_sm, up_sm, dn_slope, up_slope)
	
#	extract values from "vol_theo"
	theo_vec = as.vector(sapply(vol_theo, function(x){unlist(x["theo"])}, simplify=TRUE))
	x_vec = as.vector(sapply(vol_theo, function(x){unlist(x["x"])}, simplify=TRUE))		
	
	mm = c("f_syn","vol_curr","slope_curr","x1","x2","x0","x3")
	
#	return value:
	eval(parse(text=paste("list(", "theo_vec=theo_vec,", "x_vec=x_vec,", paste(mm,"=vol_theo[[1]]$",mm,sep="",collapse=","), ")", sep="")))
}

#strike price vector
X_vec = X_atm + c(-rev(seq(1:X_n)),0,seq(1:X_n)) * X_inc
volatility_wing_model_vec(X_vec, days, alpha, f_atm, f_ref, SSR,
		vol_ref, VCR, slope_ref, SCR,
		dn_cf, up_cf, put_curv, call_curv,
		dn_sm, up_sm, dn_slope, up_slope) 






