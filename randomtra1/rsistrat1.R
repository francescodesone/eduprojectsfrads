



### Disclaimer
# 
# # #### General disclosure: This material is intended for information purposes only,
# # and does not constitute investment advice, a recommendation or an offer or
# # solicitation to purchase or sell any securities to any person in any jurisdiction
# # in which an offer, solicitation, purchase or sale would be unlawful under the
# # securities laws of such jurisdiction. The opinions expressed are subject to
# # change without notice. Reliance upon information in this material is at the
# # sole discretion of the reader. Investing involves risks.
# Once again, all the information provided by this document is offered
# solely for educational and instructional purposes. 
# They do not constitute
# professional financial advice or investment recommendations.
# Investments
# carry inherent risks. Past performance is not indicative of future results.
# Before making any investment, it is essential to conduct your own research
# and carefully assess the risks. It is advisable to consult a professional financial
# advisor before making significant financial decisions. 











library(quantmod)
library(forecast)
library(dplyr)
library(PerformanceAnalytics)


getSymbols(c("EURUSD=X","^GSPC","GC=F"), from="2004-01-01", to="2024-12-31")
eurusd = na.approx(Cl(`EURUSD=X`))  
gold=na.approx(Cl(`GC=F`))
sp=na.approx(Cl(GSPC))

## compute returns of Gold

goldret=dailyReturn(gold,type="log")

### Compute rsi
rsi7 = RSI(gold$`GC=F.Close`, n=7)  
rsi14 = RSI(gold$`GC=F.Close`, n=14)  

### remove NAs
rsi7 = na.omit(rsi7)  
rsi14 = na.omit(rsi14)  

### create empty vectors
signal_rsi7 = rep(NA, length(rsi7))  
signal_rsi14 = rep(NA, length(rsi14))  


### function for creating RSI signals
signalrsifun = function(rsi) {
  signal = rep(NA, length(rsi))  
  for (i in 1:length(rsi)) {
    if (rsi[i] > 70 | rsi[i] >30) {
      signal[i] = -1  
    } else if (rsi[i] < 30) {
      signal[i] = 1  
    } else {
      signal[i] = NA  
    }
  }
  return(signal)  
}

signal_rsi7 = signalrsifun(rsi7)
signal_rsi14 = signalrsifun(rsi14)



### fix the index
signal_rsi7_xts = Lag(xts(signal_rsi7, order.by = index(rsi7)),k=1)
signal_rsi14_xts = Lag(xts(signal_rsi14, order.by = index(rsi14)),k=1)


goldret_aligned7 = goldret[index(signal_rsi7_xts)]
goldret_aligned14 = goldret[index(signal_rsi14_xts)]

strategy_returns_rsi7 = signal_rsi7_xts * goldret_aligned7  
cumulative_returns_rsi7 = cumsum(na.omit(strategy_returns_rsi7))  

strategy_returns_rsi14 = signal_rsi14_xts * goldret_aligned14  
cumulative_returns_rsi14 = cumsum(na.omit(strategy_returns_rsi14))  


### trading costs
commission_rate = 0.02  


sign_change1 = ifelse(signal_rsi7_xts != Lag(signal_rsi7_xts, k=1), 1, 0)  
sign_change2 = ifelse(signal_rsi14_xts != Lag(signal_rsi14_xts, k=1), 1, 0)  


sign_change1=na.omit(sign_change1)
sign_change2=na.omit(sign_change2)


adj1=na.omit(strategy_returns_rsi7[-1,])
adj2=na.omit(strategy_returns_rsi14[-1,])
adj11=adj1[-1,]
adj22=adj2[-1,]

apply_commission_on_sign_change <- function(equity, sign_changes, commission_rate) {
  
  if(length(equity) != length(sign_changes)) {
    stop("La lunghezza di 'equity' e 'sign_changes' deve essere la stessa.")
  }
  
  for (i in 2:length(sign_changes)) {
    if (sign_changes[i] == 1) {  
      
      
      if (equity[i] < 0) {
        equity[i] = equity[i] * (1 - (commission_rate * -1))  
      } else {
        equity[i] = equity[i] * (1 - commission_rate)
      }
    }
  }
  
  return(equity)  
}


adj11=apply_commission_on_sign_change(adj11,sign_changes = sign_change1,commission_rate)
adj22=apply_commission_on_sign_change(adj22,sign_changes = sign_change2,commission_rate)

cumretadj1=cumsum(adj11)
cumretadj2=cumsum(adj22)



### plot both cumulative and cumulative with trading costs
plot(cumulative_returns_rsi14,type="l")
plot(cumulative_returns_rsi7,type = "l")

plot(cumretadj1,type="l")
plot(cumretadj2,type="l")





