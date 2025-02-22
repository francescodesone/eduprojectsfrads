
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



### set the number of simulations for each day as well as the seed
n_simulations = 1000
set.seed(100)


### set the vectors and matrix for the results
simulated_returns = matrix(NA, nrow=length(gold), ncol=n_simulations)
final_signal = matrix(NA, nrow=length(gold), ncol=n_simulations)
ZZ1=matrix(NA, nrow=length(gold), ncol=n_simulations)
ZZ2=matrix(NA, nrow=length(gold), ncol=n_simulations)
Signalff=matrix(NA, nrow=length(gold), ncol=1)

### run the simulation
for (i in 1:length(gold)) {
  
  for( j in 1: n_simulations){
    
    
    ZZ1[i,j] = runif(1, min = -1, max = 1)  
    
    ZZ2[i,j] = ifelse(ZZ1[i,j] > 0, 1, -1) 
    
  }
  
  Signalff[i,]=ifelse(sum(ZZ2[i,] == 1) > sum(ZZ2[i,] == -1), 1, -1)
  
}

finalsig=Lag(Signalff,k=1)
retstrat=goldret*finalsig

sign_change1 = ifelse(finalsig != Lag(finalsig, k=1), 1, 0) 

cumret=cumsum(na.omit(retstrat))


commission_rate = 0.02  

adjusted_cumulative_returns1 = na.omit(retstrat[-1,])

adj11=adjusted_cumulative_returns1[-1,]

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


### plot for results
plot(cumret,type="l")
lines(adjustedcumret1,col="red")









