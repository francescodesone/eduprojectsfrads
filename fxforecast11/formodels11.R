


### Disclaimer
# 
# # #### General disclosure: This material is intended for information purposes only,
# # and does not constitute investment advice, a recommendation or an offer or
# # solicitation to purchase or sell any securities to any person in any jurisdiction
# # in which an offer, solicitation, purchase or sale would be unlawful under the
# # securities laws of such jurisdiction. The opinions expressed are subject to
# # change without notice. Reliance upon information in this material is at the
# # sole discretion of the reader. Investing involves risks.
# Once again, all the information provided by this document/Rfile/code is offered
# solely for educational and instructional purposes. 
# This do not constitute
# professional financial advice or investment recommendations.
# Investments
# carry inherent risks. Past performance is not indicative of future results.
# Before making any investment, it is essential to conduct your own research
# and carefully assess the risks. It is advisable to consult a professional financial
# advisor before making significant financial decisions. 



# Load the necessary packages
library(quantmod)
library(forecast)
library(zoo)  # For the na.approx() function
library(e1071)  # For calculating skewness and kurtosis

# Define the tickers for the major currency pairs
tickers <- c("JPY=X", "GBPUSD=X", "AUDUSD=X", "CAD=X", "CHF=X", "EURUSD=X", "NZDUSD=X")

# Download data from Yahoo Finance for each ticker
data_list <- lapply(tickers, function(ticker) {
  getSymbols(ticker, src = "yahoo", auto.assign = FALSE, from="2010-01-01", to="2025-01-30")
})

# Name the variables for the data
names(data_list) <- c("USD/JPY", "GBP/USD", "AUD/USD", "USD/CAD", "USD/CHF", "EUR/USD", "NZD/USD")

# Interpolate missing values (NA) for each dataset using na.approx()
data_list <- lapply(data_list, function(data) {
  na.approx(data)  # Interpolate missing values
})

# Extract the closing prices and create a data frame
price_data_list <- lapply(data_list, function(data) {
  Cl(data)  # Extract only the closing prices
})

# Create a data frame with all the closing prices
price_df <- do.call(merge, price_data_list)

# Calculate the daily returns for each symbol
returns_list <- lapply(price_data_list, function(data) {
  dailyReturn(data)  # Calculate the daily returns
})

# Name the variables for the returns
names(returns_list) <- names(price_data_list)

# Calculate the statistical moments (mean, standard deviation, skewness, kurtosis)
moments_list <- lapply(returns_list, function(returns) {
  mean_ret <- median(returns) * 100
  sd_ret <- sd(returns) * 100
  skew_ret <- skewness(returns)
  kurt_ret <- kurtosis(returns)
  
  # Return the results as a vector
  c(Median = mean_ret, SD = sd_ret, Skewness = skew_ret, Kurtosis = kurt_ret)
})

# Name the variables for the moments
names(moments_list) <- names(returns_list)

# Create a data frame for the moments
moments_df <- do.call(rbind, moments_list)

# Add a column for the currency pair name
moments_df <- data.frame(Currency = rownames(moments_df), moments_df)

# Display the moments
print(moments_df)

# Display the price data frame
print(price_df)

# Initialize vectors for the forecasts of each symbol
eurorw <- vector()
gbprw <- vector()
audrw <- vector()
cadrw <- vector()
chfrw <- vector()
nzdwrw <- vector()
jpyrw <- vector()

# Parameters
T1 <- length(price_df$EURUSD.X.Close)
Win <- 252  # Window of 252 days
N <- T1 - Win + 1

# Rolling loop for all currency pairs
for(i in 1:N){
  
  # EUR/USD
  initial <- i
  final <- i + Win - 1
  priceseur <- price_df$EURUSD.X.Close[initial:final]
  eurorw[i] <-  as.numeric(rwf(log(priceseur), h = 1,drift = TRUE)$mean)
  
  # GBP/USD
  pricesgbp <- price_df$GBPUSD.X.Close[initial:final]
  gbprw[i] <-  as.numeric(rwf(log(pricesgbp), h = 1,drift = TRUE)$mean)
  
  # AUD/USD
  pricesaud <- price_df$AUDUSD.X.Close[initial:final]
  audrw[i] <- as.numeric(rwf(log(pricesaud), h = 1,drift = TRUE)$mean)
  
  # USD/CAD
  pricescad <- price_df$CAD.X.Close[initial:final]
  cadrw[i] <- as.numeric(rwf(log(pricescad), h = 1,drift = TRUE)$mean)
  
  # USD/CHF
  priceschf <- price_df$CHF.X.Close[initial:final]
  chfrw[i] <- as.numeric( rwf(log(priceschf), h = 1,drift = TRUE)$mean)
  
  # NZD/USD
  pricesnzd <- price_df$NZDUSD.X.Close[initial:final]
  nzdwrw[i] <- as.numeric(rwf(log(pricesnzd), h = 1,drift = TRUE)$mean)
  
  # USD/JPY
  pricesjpy <- price_df$JPY.X.Close[initial:final]
  jpyrw[i] <- as.numeric( rwf(log(pricesjpy), h = 1,drift = TRUE)$mean)
}



### ARIMA Model

# ARIMA function
ARIMAfun = function(R){
  
  Pri = as.numeric(R)
  fit1 = auto.arima(R, max.p = 5, max.q = 5, stationary = FALSE)
  
  fore = forecast(fit1, h = 1)$mean
  
  return(as.numeric(fore))
}

# Initialize vectors for the forecasts of each symbol
eurorima <- vector()
gbprima <- vector()
audrima <- vector()
cadrima <- vector()
chfrima <- vector()
nzdwrima <- vector()
jpyrima <- vector()

# Parameters
T1 <- length(price_df$EURUSD.X.Close)
Win <- 252  # Window of 252 days
N <- T1 - Win + 1

# Rolling loop for all currency pairs
for(i in 1:N){
  
  # EUR/USD
  initial <- i
  final <- i + Win - 1
  priceseur <- price_df$EURUSD.X.Close[initial:final]
  eurorima[i] <-  ARIMAfun(priceseur)
  
  # GBP/USD
  pricesgbp <- price_df$GBPUSD.X.Close[initial:final]
  gbprima[i] <-  ARIMAfun(pricesgbp)
  
  # AUD/USD
  pricesaud <- price_df$AUDUSD.X.Close[initial:final]
  audrima[i] <- ARIMAfun(pricesaud)
  
  # USD/CAD
  pricescad <- price_df$CAD.X.Close[initial:final]
  cadrima[i] <- ARIMAfun(pricescad)
  
  # USD/CHF
  priceschf <- price_df$CHF.X.Close[initial:final]
  chfrima[i] <- ARIMAfun(priceschf)
  
  # NZD/USD
  pricesnzd <- price_df$NZDUSD.X.Close[initial:final]
  nzdwrima[i] <- ARIMAfun(pricesnzd)
  
  # USD/JPY
  pricesjpy <- price_df$JPY.X.Close[initial:final]
  jpyrima[i] <- ARIMAfun(pricesjpy)
}

### ARFIMA Model

# ARFIMA function
ARFIMAfun = function(R){
  
  Pri = as.numeric(R)
  fit1 = arfima(Pri, estim = "ls", drange = c(0, 0.5))
  
  fore = forecast(fit1, h = 1)$mean
  
  return(as.numeric(fore))
}


# Initialize vectors for the forecasts of each symbol
eurofima <- vector()
gbpfima <- vector()
audfima <- vector()
cadfima <- vector()
chffima <- vector()
nzdwfima <- vector()
jpyfima <- vector()

# Parameters
T1 <- length(price_df$EURUSD.X.Close)
Win <- 252  # Window of 252 days
N <- T1 - Win + 1

# Rolling loop for all currency pairs
for(i in 1:N){
  
  # EUR/USD
  initial <- i
  final <- i + Win - 1
  priceseur <- price_df$EURUSD.X.Close[initial:final]
  eurofima[i] <-  ARFIMAfun(priceseur)
  
  # GBP/USD
  pricesgbp <- price_df$GBPUSD.X.Close[initial:final]
  gbpfima[i] <-  ARFIMAfun(pricesgbp)
  
  # AUD/USD
  pricesaud <- price_df$AUDUSD.X.Close[initial:final]
  audfima[i] <- ARFIMAfun(pricesaud)
  
  # USD/CAD
  pricescad <- price_df$CAD.X.Close[initial:final]
  cadfima[i] <- ARFIMAfun(pricescad)
  
  # USD/CHF
  priceschf <- price_df$CHF.X.Close[initial:final]
  chffima[i] <- ARFIMAfun(priceschf)
  
  # NZD/USD
  pricesnzd <- price_df$NZDUSD.X.Close[initial:final]
  nzdwfima[i] <- ARFIMAfun(pricesnzd)
  
  # USD/JPY
  pricesjpy <- price_df$JPY.X.Close[initial:final]
  jpyfima[i] <- ARFIMAfun(pricesjpy)
}
