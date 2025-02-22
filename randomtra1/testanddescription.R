


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
library(PerformanceAnalytics)
library(e1071)
library(vrtest)


### download data
getSymbols(c("EURUSD=X","^GSPC","GC=F"), from="2004-01-01", to="2024-12-31")
eurusd = na.approx(Cl(`EURUSD=X`))  
gold=na.approx(Cl(`GC=F`))
sp=na.approx(Cl(GSPC))


### compute returns
euroret = dailyReturn(eurusd$`EURUSD=X.Close`, type="log")  
goldret = dailyReturn(gold$`GC=F.Close`, type="log")  
spret = dailyReturn(sp$GSPC.Close, type="log")  


### summary statistics
summary(as.numeric(euroret$daily.returns))*100
summary(as.numeric(goldret$daily.returns))*100
summary(as.numeric(spret$daily.returns))*100
sd(euroret$daily.returns)*100
sd(goldret$daily.returns)*100
sd(spret$daily.returns)*100

skewness(euroret$daily.returns)
skewness(spret$daily.returns)
skewness(goldret$daily.returns)
kurtosis(euroret$daily.returns,type=3)
kurtosis(goldret$daily.returns)
kurtosis(spret$daily.returns)



### Auto Q test

Auto.Q(euroret$daily.returns)
Auto.Q(goldret$daily.returns)
Auto.Q(spret$daily.returns)






### rolling Auto Q test
goldvec=vector()
spvec=vector()
eurovec=vector()


lengold=length(goldret)
eurolen=length(euroret)
splen=length(spret)

Win=504

Ngold=lengold-Win
Neuro=eurolen-Win
Nsp=splen-Win


for ( i in 1:Neuro ){
  
  initial=i
  final=Win+i
  euroroll=as.numeric(euroret[initial:final])
  
  eurovec[i]=Auto.Q(euroroll,lags = 10)[[2]]
  
}


for ( i in 1:Ngold ){
  
  initial=i
  final=Win+i
  goldroll=as.numeric(goldret[initial:final])
  
  goldvec[i]=Auto.Q(goldroll,lags = 10)[[2]]
  
}

for ( i in 1:Nsp ){
  
  initial=i
  final=Win+i
  sproll=as.numeric(spret[initial:final])
  
  spvec[i]=Auto.Q(sproll,lags = 10)[[2]]
  
}


### create plots

library(ggplot2)

df_euro <- data.frame(
  Time = 1:Neuro,        
  Value = eurovec,       
  ActualTime = as.yearmon(index(eurusd)[1:Neuro + Win])  
)

df_gold <- data.frame(
  Time = 1:Ngold,        
  Value = goldvec,      
  ActualTime = as.yearmon(index(gold)[1:Ngold + Win])  
)

df_sp <- data.frame(
  Time = 1:Nsp,          
  Value = spvec,         
  ActualTime = as.yearmon(index(sp)[1:Nsp + Win])  
)



### Eurusd plot
ggplot(df_euro, aes(x = ActualTime, y = Value)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = c(0.01, 0.05, 0.1), linetype = "dashed", color = "black") +
  labs(title = "Rolling P-value from Auto Q Test EUR/USD",
       x = "Time", y = "P-value") +
  theme_minimal()

### gold plot
ggplot(df_gold, aes(x = ActualTime, y = Value)) +
  geom_line(color = "green") +
  geom_hline(yintercept = c(0.01, 0.05, 0.1), linetype = "dashed", color = "black") +
  labs(title = "Rolling P-value from Auto Q Test Gold",
       x = "Time", y = "P-value") +
  theme_minimal() 

### SP plot
ggplot(df_sp, aes(x = ActualTime, y = Value)) +
  geom_line(color = "red") +
  geom_hline(yintercept = c(0.01, 0.05, 0.1), linetype = "dashed", color = "black") +
  labs(title = "Rolling P-value from Auto Q Test S&P500",
       x = "Time", y = "P-value") +
  theme_minimal() 



### ACF and PACF plot

ACFEUR=acf(as.numeric(euroret$daily.returns),lag.max = 15, main="ACF EUR/USD")
PACFEUR=pacf(as.numeric(euroret$daily.returns),lag.max = 15,main="PACF EUR/USD")


ACFEUR=acf(as.numeric(goldret$daily.returns),lag.max = 15, main="ACF Gold")
PACFEUR=pacf(as.numeric(goldret$daily.returns),lag.max = 15,main="PACF Gold")

ACFEUR=acf(as.numeric(spret$daily.returns),lag.max = 15, main="ACF S&P 500")
PACFEUR=pacf(as.numeric(spret$daily.returns),lag.max = 15,main="PACF S&P 500")






### Auto VR test with bootstrap

AutoBoot.test(as.numeric(euroret), nboot = 10000, wild = "Mammen")
AutoBoot.test(as.numeric(goldret), nboot = 10000, wild = "Mammen")
AutoBoot.test(as.numeric(spret), nboot = 10000, wild = "Mammen")


### Rolling Auto VR test with bootstrap

goldvec=vector()
spvec=vector()
eurovec=vector()


lengold=length(goldret)
eurolen=length(euroret)
splen=length(spret)

Win=504

Ngold=lengold-Win
Neuro=eurolen-Win
Nsp=splen-Win


for ( i in 1:Neuro ){
  
  initial=i
  final=Win+i
  euroroll=as.numeric(euroret[initial:final])
  
  eurovec[i]=AutoBoot.test(euroroll, nboot = 10, wild = "Mammen")[3]
  
}


for ( i in 1:Ngold ){
  
  initial=i
  final=Win+i
  goldroll=as.numeric(goldret[initial:final])
  
  goldvec[i]=AutoBoot.test(goldroll, nboot = 10, wild = "Mammen")[3]
  
}

for ( i in 1:Nsp ){
  
  initial=i
  final=Win+i
  sproll=as.numeric(spret[initial:final])
  
  spvec[i]=AutoBoot.test(sproll, nboot = 10, wild = "Mammen")[3]
  
}





library(ggplot2)

df_euro <- data.frame(
  Time = 1:Neuro,        
  Value = as.numeric(eurovec),      
  ActualTime = as.yearmon(index(eurusd)[1:Neuro + Win])  
)

df_gold <- data.frame(
  Time = 1:Ngold,       
  Value = as.numeric(goldvec),       
  ActualTime = as.yearmon(index(gold)[1:Ngold + Win])  
)

df_sp <- data.frame(
  Time = 1:Nsp,          
  Value = as.numeric(spvec),         
  ActualTime = as.yearmon(index(sp)[1:Nsp + Win])  
)

### eurusd plot
ggplot(df_euro, aes(x = ActualTime, y = Value)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = c(0.01, 0.05, 0.1), linetype = "dashed", color = "black") +
  labs(title = "Rolling P-value from Variance Ratio Test EUR/USD",
       x = "Time", y = "P-value") +
  theme_minimal()

### gold plot
ggplot(df_gold, aes(x = ActualTime, y = Value)) +
  geom_line(color = "green") +
  geom_hline(yintercept = c(0.01, 0.05, 0.1), linetype = "dashed", color = "black") +
  labs(title = "Rolling P-value from Variance Ratio Test Gold",
       x = "Time", y = "P-value") +
  theme_minimal() 

### sp plot
ggplot(df_sp, aes(x = ActualTime, y = Value)) +
  geom_line(color = "red") +
  geom_hline(yintercept = c(0.01, 0.05, 0.1), linetype = "dashed", color = "black") +
  labs(title = "Rolling P-value from Variance Ratio Test S&P500",
       x = "Time", y = "P-value") +
  theme_minimal() 




### Heteroscedasticity adjusted VR tests, Lo and MaCkinlay and CD with bootstrap

vec1=c(2,4,6,8,10)


Boot.test(as.numeric(euroret),kvec = vec1,nboot = 10000,wild="Mammen")
Boot.test(as.numeric(goldret),kvec = vec1,nboot = 10000,wild="Mammen")
Boot.test(as.numeric(spret),kvec = vec1,nboot = 10000,wild="Mammen")















