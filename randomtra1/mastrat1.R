



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
library(TTR)  
library(dplyr)
library(PerformanceAnalytics)




getSymbols(c("EURUSD=X","^GSPC","GC=F"), from="2004-01-01", to="2024-12-31")
eurusd = na.approx(Cl(`EURUSD=X`))  
gold=na.approx(Cl(`GC=F`))
sp=na.approx(Cl(GSPC))

## compute returns of Gold

goldret=dailyReturn(gold,type="log")


### example on Gold

### Compute Moving Averages
ma20 = SMA(gold$`GC=F.Close`, n=20)  
ma50 = SMA(gold$`GC=F.Close`, n=50)  
ma200 = SMA(gold$`GC=F.Close`, n=200)  

### define the signal based on cross of MA 20 50
signal1 = ifelse(ma20 > ma50, 1, -1)  
signal1 = Lag(signal1, k=1)  


sign_change1 = ifelse(signal1 != Lag(signal1, k=1), 1, 0)  


strategy_returns1 = signal1 * goldret  
cumulative_returns1 = cumsum(na.omit(strategy_returns1)) 

### define the signal based on cross of MA 200 50
signal2 = ifelse(ma50 > ma200, 1, -1)  
signal2 = Lag(signal2, k=1)  

sign_change2 = ifelse(signal2 != Lag(signal2, k=1), 1, 0)  


strategy_returns2 = signal2 * goldret  
cumulative_returns2 = cumsum(na.omit(strategy_returns2))  



commission_rate = 0.02  

adjusted_cumulative_returns1 = na.omit(strategy_returns1[-1,])
adjusted_cumulative_returns2 = na.omit(strategy_returns2[-1,])

adj11=adjusted_cumulative_returns1[-1,]
adj22=adjusted_cumulative_returns2[-1,]

sign_change11=na.omit(sign_change1)
sign_change22=na.omit(sign_change2)

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
adjusted_equity2 = apply_commission_on_sign_change(adj22, sign_change22, commission_rate)

adjustedcumret1=cumsum(adjusted_equity1)
adjustedcumret2=cumsum(adjusted_equity2)

### plot both cumulative and cumulative with trading costs
plot(adjustedcumret1,type="l")
plot(adjustedcumret2,type="l")

plot(cumulative_returns2,type="l")
plot(cumulative_returns1,type="l")


