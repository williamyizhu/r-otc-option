#########################################################################################################################################################
# TODO: Add comment
#########################################################################################################################################################
#print(paste(getwd(), "/market_maker.R", sep=""))

#theoretical volatility matrix for bid, mid(theo) and ask volatility 
mm_vol_matrix = cbind(theo_bid_vec, vol_theo_vec, theo_ask_vec)

#european style put options
mm_put_matrix = mapply(EuropeanOption, "put", f_atm, X_vec, 0, risk_free_interest, days_to_maturity/365, mm_vol_matrix)
mm_put_greeks = matrix(as.vector(unlist(mm_put_matrix[c("value","delta","vega"),])), ncol=3, byrow=TRUE)
mm_put_value = matrix(mm_put_greeks[,1], ncol=3, byrow=FALSE)
mm_put_delta = matrix(mm_put_greeks[,2], ncol=3, byrow=FALSE)
mm_put_vega = matrix(mm_put_greeks[,3], ncol=3, byrow=FALSE)

#european style call options
mm_call_matrix = mapply(EuropeanOption, "call", f_atm, X_vec, 0, risk_free_interest, days_to_maturity/365, mm_vol_matrix)
mm_call_greeks = matrix(as.vector(unlist(mm_call_matrix[c("value","delta","vega"),])), ncol=3, byrow=TRUE)
mm_call_value = matrix(mm_call_greeks[,1], ncol=3, byrow=FALSE)
mm_call_delta = matrix(mm_call_greeks[,2], ncol=3, byrow=FALSE)
mm_call_vega = matrix(mm_call_greeks[,3], ncol=3, byrow=FALSE)

#create a price and greek matrix
mm_greeks = cbind(round(mm_put_vega[,2]*0.01,4), round(mm_put_delta[,2],4), round(mm_put_value,4), X_vec, round(mm_call_value,4), round(mm_call_delta[,2],4), round(mm_call_vega[,2]*0.01,4))
colnames(mm_greeks) = c("pVega","pDelta","pBid","pMid","pAsk","strike","cBid","cMid","cAsk","cDelta","cVega")
delete(.GlobalEnv$greeks_gframe_obj, .GlobalEnv$greeks_gtable_obj)
.GlobalEnv$greeks_gtable_obj = gtable(mm_greeks, container=.GlobalEnv$greeks_gframe_obj)			
gtkTreeViewSetGridLines(getToolkitWidget(.GlobalEnv$greeks_gtable_obj), grid.lines=3)
#print(mm_greeks)

#create a volatility matrix
mm_strike_vol_matrix = cbind(X_vec, round(mm_vol_matrix*100,4))
colnames(mm_strike_vol_matrix) = c("strike","volBid","volMid","volAsk")
delete(.GlobalEnv$volatility_gframe_obj, .GlobalEnv$volatility_gtable_obj)
.GlobalEnv$volatility_gtable_obj = gtable(mm_strike_vol_matrix, container=.GlobalEnv$volatility_gframe_obj)			
gtkTreeViewSetGridLines(getToolkitWidget(.GlobalEnv$volatility_gtable_obj), grid.lines=3)
#print(mm_strike_vol_matrix)


