library(readr)
library(dplyr)

evaluate <- function(K, invest, maxEURUSD, maxEURPLN, maxUSDPLN, priceEURUSD, priceEURPLN, priceUSDPLN)
{
	for(i in 1:length(invest$B))
	{
	  K = K + K*switch(
	    invest$EX[i],
	    abs(priceEURUSD[maxEURUSD[invest$E[i]]] - priceEURUSD[maxEURUSD[invest$B[i]]]),
	    abs(priceEURPLN[maxEURPLN[invest$E[i]]] - priceEURPLN[maxEURPLN[invest$B[i]]]),
	    abs(priceUSDPLN[maxUSDPLN[invest$E[i]]] - priceUSDPLN[maxUSDPLN[invest$B[i]]])
	  )
	}
	return(K)
}
N <- 100
K <- 100

EURPLN <- read_csv("DAT_MT_EURPLN_M1_2016.csv", 
                   col_names = FALSE, col_types = cols(X1 = col_date(format = "%Y.%m.%d"), 
                                                       X2 = col_time(format = "%H:%M"), 
                                                       X7 = col_skip()))
colnames(EURPLN) <- c("date", "time", "price1", "price2", "price3","price4")
EURPLN <- EURPLN %>% mutate( time = as.POSIXct(paste(date,time,sep = " ")))
EURPLN$date <- NULL

EURUSD <- read_csv("DAT_MT_EURUSD_M1_2016.csv", 
                   col_names = FALSE, col_types = cols(X1 = col_date(format = "%Y.%m.%d"), 
                                                       X2 = col_time(format = "%H:%M"), 
                                                       X7 = col_skip()))
colnames(EURUSD) <- c("date", "time", "price1", "price2", "price3","price4")
EURUSD <- EURUSD %>% mutate( time = as.POSIXct(paste(date,time,sep = " ")))
EURUSD$date <- NULL

USDPLN <- read_csv("DAT_MT_USDPLN_M1_2016.csv", 
                   col_names = FALSE, col_types = cols(X1 = col_date(format = "%Y.%m.%d"), 
                                                       X2 = col_time(format = "%H:%M"), 
                                                       X7 = col_skip()))
colnames(USDPLN) <- c("date", "time", "price1", "price2", "price3","price4")
USDPLN <- USDPLN %>% mutate( time = as.POSIXct(paste(date,time,sep = " ")))
USDPLN$date <- NULL


plot(EURUSD$time, EURUSD$price1,type = 'l', col = 'black', xlab = "Time", ylab = "EXCH")
par(new=TRUE)
plot(EURPLN$time, EURPLN$price1, type = 'l', col = 'blue', xlab = "Time", ylab = "EXCH")
par(new=TRUE)
plot(USDPLN$time, USDPLN$price1, type = 'l', col = 'red', xlab = "Time", ylab = "EXCH")

print(length(data$price))
maxEURUSD = which(diff(sign(diff(EURUSD$price1)))==-2)+1
maxEURPLN = which(diff(sign(diff(EURPLN$price1)))==-2)+1
maxUSDPLN = which(diff(sign(diff(USDPLN$price1)))==-2)+1
plot(EURUSD$time[maxEURUSD], EURUSD$price1[maxEURUSD], type = 'l', col = 'black', xlab = "Maxima", ylab = "EXCH")
par(new=TRUE)
plot(EURPLN$time[maxEURPLN], EURPLN$price1[maxEURPLN], type = 'l', col = 'blue', xlab = "Maxima", ylab = "EXCH")
par(new=TRUE)
plot(USDPLN$time[maxUSDPLN], USDPLN$price1[maxUSDPLN], type = 'l', col = 'red', xlab = "Maxima", ylab = "EXCH")


# Niewięcej transakcji niz maximów
if (2*length(maxEURUSD) < N)
{
	N = as.integer(length(maxEURUSD)/2)
}
if (2*length(maxEURPLN) < N)
{
  N = as.integer(length(maxEURPLN)/2)
}
if (2*length(maxUSDPLN) < N)
{
  N = as.integer(length(maxUSDPLN)/2)
}


invest <- data.frame(
  B = seq(1, 2*N, 2), 
  E = seq(2, 2*N + 1, 2), 
  EX = rep(1, N)
  )

print(evaluate(K, invest, maxEURUSD, maxEURPLN, maxUSDPLN, EURUSD$price1, EURPLN$price1, USDPLN$price1))
