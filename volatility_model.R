############################################################################################################################################################
# TODO # Add comment
############################################################################################################################################################
#------------------------------------- wing model -------------------------------------
volatility_wing_model = function(X, days, alpha, f_atm, f_ref, SSR,
		vol_ref, VCR, slope_ref, SCR,
		dn_cf, up_cf, put_curv, call_curv,
		dn_sm, up_sm, dn_slope, up_slope) {	
#	synthetic forward price
	f_syn = f_atm ^ (SSR / 100) * f_ref ^ (1 - SSR / 100)	
#	current volatility and current slope, i.e., when log-moneyness x = 0
	vol_curr = vol_ref - VCR * SSR * (f_syn - f_ref) / f_ref	
	slope_curr = slope_ref - SCR * SSR * (f_syn - f_ref) / f_ref			
	
#	end points of the range
	x1 = dn_cf
	x2 = up_cf
	x0 = (1 + dn_sm) * dn_cf
	x3 = (1 + up_sm) * up_cf
	
#	"sigma_x1" is derived from put wing function 
	sigma_x1 = vol_curr + slope_curr * x1 + put_curv * x1^2
	d_sigma_x1 = slope_curr + put_curv * x1			
	
#	"sigma_x0" is derived from down smoothing range function
	dn_sm_c = (d_sigma_x1 - dn_slope) / (2 * (x1 - x0))
	dn_sm_b = dn_slope - (d_sigma_x1 - dn_slope) * x0 / (x1 - x0)
	dn_sm_a = sigma_x1 - dn_sm_b * x1 - dn_sm_c * x1^2		
	sigma_x0 = dn_sm_a + dn_sm_b * x0 + dn_sm_c * x0^2

#	"sigma_x2" is derived from call wing function 
	sigma_x2 = vol_curr + slope_curr * x2 + call_curv * x2^2	
	d_sigma_x2 = slope_curr + call_curv * x2			
	
#	"sigma_x3" is derived from up smoothing range function
	up_sm_c = (d_sigma_x2 - up_slope) / (2 * (x2 - x3))
	up_sm_b = up_slope - (d_sigma_x2 - up_slope) * x3 / (x2 - x3)
	up_sm_a = sigma_x2 - up_sm_b * x2 - up_sm_c * x2^2		
	sigma_x3 = up_sm_a + up_sm_b * x3 + up_sm_c * x3^2	
	
#	log-moneyness, i.e., transformed strike price
	x = (1 / (days / 365)^alpha) * log(X / f_syn)	
	
#	regions of the wing model
	if ((x1 <= x) & (x < 0)) {
#		put wing		
		theo = vol_curr + slope_curr * x + put_curv * x^2		
	} else if ((0 <= x) & (x <= x2)	) {
#		call wing
		theo = vol_curr + slope_curr * x + call_curv * x^2	
	} else if ((x0 <= x) & (x < x1)) {
#		down smoothing range			
		theo = dn_sm_a + dn_sm_b * x + dn_sm_c * x^2		
	} else if ((x2 < x) & (x <= x3)	) {
#		up smoothing range		
		theo = up_sm_a + up_sm_b * x + up_sm_c * x^2
	} else if (x < x0) {
#		down affine range		
		theo = dn_slope * (x - x0) + sigma_x0
	} else if (x3 < x) {
#		up affine range		
		theo = up_slope * (x - x3) + sigma_x3
	} else {		
	}	
	
#	return value:
	list(f_syn=f_syn, vol_curr=vol_curr, slope_curr=slope_curr, x1=x1, x2=x2, x0=x0, x3=x3, x=x, theo=theo)
}


