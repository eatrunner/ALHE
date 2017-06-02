
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

EURPLN <- read.csv("EURPLN2016.csv")
EURUSD <- read.csv("EURUSD2016.csv")
USDPLN <- read.csv("USDPLN2016.csv")

plot(EURUSD$price,type = 'l', col = 'black', xlab = "Time", ylab = "EXCH")
par(new=TRUE)
plot(EURPLN$price, type = 'l', col = 'blue', xlab = "Time", ylab = "EXCH")
par(new=TRUE)
plot(USDPLN$price, type = 'l', col = 'red', xlab = "Time", ylab = "EXCH")

print(length(data$price))
maxEURUSD = which(diff(sign(diff(EURUSD$price)))==-2)+1
maxEURPLN = which(diff(sign(diff(EURPLN$price)))==-2)+1
maxUSDPLN = which(diff(sign(diff(USDPLN$price)))==-2)+1
plot(maxEURUSD, EURUSD$price[maxEURUSD], type = 'l', col = 'black', xlab = "Maxima", ylab = "EXCH")
par(new=TRUE)
plot(maxEURPLN, EURPLN$price[maxEURPLN], type = 'l', col = 'blue', xlab = "Maxima", ylab = "EXCH")
par(new=TRUE)
plot(maxUSDPLN, USDPLN$price[maxUSDPLN], type = 'l', col = 'red', xlab = "Maxima", ylab = "EXCH")


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

print(evaluate(K, invest, maxEURUSD, maxEURPLN, maxUSDPLN, EURUSD$price, EURPLN$price, USDPLN$price))
