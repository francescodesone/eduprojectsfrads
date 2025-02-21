
library(quantmod)
library(forecast)
library(TTR)  
library(dplyr)
library(PerformanceAnalytics)



getSymbols(c("EURUSD=X","^GSPC","GC=F"), from="2004-01-01", to="2024-12-31")
eurusd = na.approx(Cl(`EURUSD=X`))  
gold=na.approx(Cl(`GC=F`))
sp=na.approx(Cl(GSPC))

## compute returns of Gold

goldret=dailyReturn(gold,type="log")


### Compute MACD
macd_result = MACD(Cl(gold), nFast=12, nSlow=26, nSig=9, wilder=FALSE)
macd = macd_result$macd  # MACD line
signal_line = macd_result$signal  # Signal line

### generate signal
signal_macd = Lag(ifelse(macd > signal_line, 1, -1),k=1)  

macd_xts = xts(signal_macd, order.by = index(macd_result)[length(gold) - length(signal_macd) + 1:length(signal_macd)])

macd_xts = na.omit(macd_xts)


sign_change1 = ifelse(macd_xts != Lag(macd_xts, k=1), 1, 0)  

macdret=na.omit(goldret*signal_macd)

cummacdret=cumsum(macdret)


commission_rate = 0.02  

adjusted_cumulative_returns1 = na.omit(macdret[-1,])

adj11=adjusted_cumulative_returns1

sign_change11=na.omit(sign_change1)


apply_commission_on_sign_change <- function(equity, sign_changes, commission_rate) {
  
  if(length(equity) != length(sign_changes)) {
    stop("La lunghezza di 'equity' e 'sign_changes' deve essere la stessa.")
  }
  
  for (i in 2:length(sign_changes)) {
    if (sign_changes[i] == 1) {  
      
      
      if (equity[i] < 0) {
        equity[i] = equity[i] * (1 - (commission_rate*-1))  
      } else {
        equity[i] = equity[i] * (1 - commission_rate)
      }
    }
  }
  
  return(equity)  
}

adjusted_equity1 = apply_commission_on_sign_change(adj11, sign_change11, commission_rate)

adjustedcumret1=cumsum(adjusted_equity1)


### plot both cumulative and cumulative with trading costs


plot(cummacdret)
plot(adjustedcumret1)

