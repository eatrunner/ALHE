library(dplyr)
library(readr)
library(GA)

evaluate <- function(invest, K, maxEURUSD, maxEURPLN, maxUSDPLN, priceEURUSD, priceEURPLN, priceUSDPLN){
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
invest_mutation <- function(ga = NULL, invest, pB, pE, pEX, min, max, maxEURUSD, maxEURPLN, maxUSDPLN, priceEURUSD, priceEURPLN, priceUSDPLN){
  sd1 = 10
  sd2 = 1
  #losowanie nowych wartosci. Jezeli jest bledna wartosc losujemy dalej.
  for(i in 1:length(invest$B))
  {
    i
    if(runif(1) < pB)
    {
      while( (tmp = as.integer(rnorm(1, invest$B[i], sd1))) > max || tmp < min || tmp >= invest$E[i]){next}
      
      invest$B[i] = tmp
    }
    if(runif(1) < pE)
    {
      while( (tmp = as.integer(rnorm(1, invest$E[i], sd1))) > max || tmp  < min || tmp <= invest$B[i]){next}
      invest$E[i] = tmp
    }
    if(runif(1) < pEX)
    {
      while( (tmp = as.integer(rnorm(1, invest$EX[i], sd2))) > 3 || tmp < 1 ){next}
      
      invest$EX[i] = tmp
    }
    #sprawdzanie poprawnosci wyniku i korygowanie
    if(i > 1 && invest$B[i] < invest$E[i-1])
    {
      val1 <- switch(
        invest$EX[i-1],
        abs(priceEURUSD[maxEURUSD[invest$E[i-1]]] - priceEURUSD[maxEURUSD[invest$B[i-1]]]),
        abs(priceEURPLN[maxEURPLN[invest$E[i-1]]] - priceEURPLN[maxEURPLN[invest$B[i-1]]]),
        abs(priceUSDPLN[maxUSDPLN[invest$E[i-1]]] - priceUSDPLN[maxUSDPLN[invest$B[i-1]]])
      )
      val2 <- switch(
        invest$EX[i],
        abs(priceEURUSD[maxEURUSD[invest$E[i]]] - priceEURUSD[maxEURUSD[invest$B[i]]]),
        abs(priceEURPLN[maxEURPLN[invest$E[i]]] - priceEURPLN[maxEURPLN[invest$B[i]]]),
        abs(priceUSDPLN[maxUSDPLN[invest$E[i]]] - priceUSDPLN[maxUSDPLN[invest$B[i]]])
      )
      if (val1 > val2)
      {
        invest$B[i] = invest$E[i-1];
      }else
      {
        invest$E[i-1] = invest$B[i];
      }
    }
    if(i < length(invest$B) && invest$E[i] > invest$B[i+1])
    {
      val1 <- switch(
        invest$EX[i],
        abs(priceEURUSD[maxEURUSD[invest$E[i]]] - priceEURUSD[maxEURUSD[invest$B[i]]]),
        abs(priceEURPLN[maxEURPLN[invest$E[i]]] - priceEURPLN[maxEURPLN[invest$B[i]]]),
        abs(priceUSDPLN[maxUSDPLN[invest$E[i]]] - priceUSDPLN[maxUSDPLN[invest$B[i]]])
      )
      val2 <- switch(
        invest$EX[i+1],
        abs(priceEURUSD[maxEURUSD[invest$E[i+1]]] - priceEURUSD[maxEURUSD[invest$B[i+1]]]),
        abs(priceEURPLN[maxEURPLN[invest$E[i+1]]] - priceEURPLN[maxEURPLN[invest$B[i+1]]]),
        abs(priceUSDPLN[maxUSDPLN[invest$E[i+1]]] - priceUSDPLN[maxUSDPLN[invest$B[i+1]]])
      )
      if (val1 < val2)
      {
         invest$B[i+1] = invest$E[i];
      }else
      {
        invest$E[i] = invest$B[i+1];
      }
      i
    }
  }
  return (invest)
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

replace.na <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}

a <- USDPLN[,1:2]
colnames(a) <- c("time" , "USDPLN") 
b <- EURUSD[,1:2]
colnames(b) <- c("time" , "EURUSD") 
c <- EURPLN[,1:2]
colnames(c) <- c("time" , "EURPLN") 

ab <- full_join(a,b,by = "time")
abc <- full_join(ab,c,by="time")
prices <- abc %>% arrange(time)
prices <- prices %>% mutate( USDPLN = f1(USDPLN)) %>% mutate(EURPLN = f1(EURPLN)) %>% mutate(EURUSD = f1(EURUSD))



plot(EURUSD$time, EURUSD$price1,type = 'l', col = 'black', xlab = "Time", ylab = "EXCH")
par(new=TRUE)
plot(EURPLN$time, EURPLN$price1, type = 'l', col = 'blue', xlab = "Time", ylab = "EXCH")
par(new=TRUE)
plot(USDPLN$time, USDPLN$price1, type = 'l', col = 'red', xlab = "Time", ylab = "EXCH")

maxEURUSD = which(diff(sign(diff(EURUSD$price1)))==-2)+1
maxEURPLN = which(diff(sign(diff(EURPLN$price1)))==-2)+1
maxUSDPLN = which(diff(sign(diff(USDPLN$price1)))==-2)+1
plot(EURUSD$time[maxEURUSD], EURUSD$price1[maxEURUSD], type = 'l', col = 'black', xlab = "Maxima", ylab = "EXCH")
par(new=TRUE)
plot(EURPLN$time[maxEURPLN], EURPLN$price1[maxEURPLN], type = 'l', col = 'blue', xlab = "Maxima", ylab = "EXCH")
par(new=TRUE)
plot(USDPLN$time[maxUSDPLN], USDPLN$price1[maxUSDPLN], type = 'l', col = 'red', xlab = "Maxima", ylab = "EXCH")

# Niewięcej transakcji niz maximów
if (2*length(maxEURUSD) < N){
	N = as.integer(length(maxEURUSD)/2)
}
if (2*length(maxEURPLN) < N){
  N = as.integer(length(maxEURPLN)/2)
}
if (2*length(maxUSDPLN) < N){
  N = as.integer(length(maxUSDPLN)/2)
}


invest <- data.frame(
  B = seq(1, 50*N, 50),
  E = seq(6, 50*N + 5, 50),
  EX = rep(1, N)
  )


print(evaluate(invest, K,maxEURUSD, maxEURPLN, maxUSDPLN, EURUSD$price1, EURPLN$price1, USDPLN$price1))

for(i in 1:30){
  invest <- invest_mutation(NULL , invest, 0.5, 0.5, 0.1, 1, length(maxEURPLN), maxEURUSD, maxEURPLN, maxUSDPLN, EURUSD$price1, EURPLN$price1, USDPLN$price1)
}
plot(invest$B, type = 'p', col = 'red', xlab = "Maxima", ylab = "EXCH")
points(invest$E, type = 'p', col = 'blue', xlab = "Maxima", ylab = "EXCH") # tak robimy wykres na wykresie :)


print(evaluate(invest, K ,maxEURUSD, maxEURPLN, maxUSDPLN, EURUSD$price1, EURPLN$price1, USDPLN$price1))

basePopulation <- function(ga) {
  N <- ga@popSize
  sample()
  # TODO jak to zrobić
}

pB <- 0.5
pE <- 0.5
pEX <- 0.1
min <-  1
max <- length(maxEURPLN)
popSize <- 10
maxiter <- 1000
pmutation <- 0.9
pcrossover <- 0
ga(type = "real-valued", 
   fitness = function (invest) -1*evaluate(invest, K ,maxEURUSD, maxEURPLN, maxUSDPLN, EURUSD$price1, EURPLN$price1, USDPLN$price1),
   min = 0,
   max = Inf, #może trzeba zmienić na jakieś rep(Inf, N)
   popSize = popSize,
   maxiter = maxiter,
   population = function(ga) basePopulation(ga), 
   mutation = function(ga, invest) invest_mutation(ga, invest, pB, pE, pEX, min, max, maxEURUSD, maxEURPLN, maxUSDPLN, priceEURUSD, priceEURPLN, priceUSDPLN),
   pmutation = pmutation, 
   pcrossover = pcrossover)
