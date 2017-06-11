library(dplyr)
library(readr)
library(GA)

evaluate <- function(invest, K, prices){
	for(i in 1:length(invest$B))
	{
	  K = K + K*switch(
	    invest$EX[i],
	    abs(prices$EURUSD[invest$E[i]] - prices$EURUSD[invest$B[i]])*prices$USDPLN[invest$B[i]],#Bo zysk jest w zloty
	    abs(prices$EURPLN[invest$E[i]] - prices$EURPLN[invest$B[i]]),
	    abs(prices$USDPLN[invest$E[i]] - prices$USDPLN[invest$B[i]])
	  )
	  
	}
	return(K)
}

invest_mutation <- function(invest, pB, pE, pEX, min, max, prices){
  sd1 = 10
  sd2 = 1
  #losowanie nowych wartosci. Jezeli jest bledna wartosc losujemy dalej.
  for(i in 1:length(invest$B)){
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
        abs(prices$EURUSD[invest$E[i-1]] - prices$EURUSD[invest$B[i-1]]),
        abs(prices$EURPLN[invest$E[i-1]] - prices$EURPLN[invest$B[i-1]]),
        abs(prices$USDPLN[invest$E[i-1]] - prices$USDPLN[invest$B[i-1]])
      )
      val2 <- switch(
        invest$EX[i],
        abs(prices$EURUSD[invest$E[i]] - prices$EURUSD[invest$B[i]]),
        abs(prices$EURPLN[invest$E[i]] - prices$EURPLN[invest$B[i]]),
        abs(prices$USDPLN[invest$E[i]] - prices$USDPLN[invest$B[i]])
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
        abs(prices$EURUSD[invest$E[i]] - prices$EURUSD[invest$B[i]]),
        abs(prices$EURPLN[invest$E[i]] - prices$EURPLN[invest$B[i]]),
        abs(prices$USDPLN[invest$E[i]] - prices$USDPLN[invest$B[i]])
      )
      val2 <- switch(
        invest$EX[i+1],
        abs(prices$EURUSD[invest$E[i+1]] - prices$EURUSD[invest$B[i+1]]),
        abs(prices$EURPLN[invest$E[i+1]] - prices$EURPLN[invest$B[i+1]]),
        abs(prices$USDPLN[invest$E[i+1]] - prices$USDPLN[invest$B[i+1]])
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

select.one.of.two <- function(population,K,prices) {
  ind <- sample(1:length(population),2)
  f1 <- evaluate(population[[ind[1]]],K,prices)
  f2 <- evaluate(population[[ind[2]]],K,prices)
  ifelse(f1 > f2, population[ind[[1]]],population[[ind[2]]])[[1]]
}
invests.selection1 <- function(population,K,prices){
  lapply(1:length(population), function(i) select.one.of.two(population,K,prices))
}

generate.invidual <- function(ticks.number, trans.number) {
  pos <- tibble( i = 1:(2*trans.number) ,p = sort(sample(1:ticks.number,size = 2 * trans.number)))
  p <- pos %>% filter(i %% 2 == 1)
  k <- pos %>% filter(i %% 2 == 0)
  data.frame(B = p$p, E = k$p, EX = sample(1:3,trans.number,replace = TRUE))
}
basePopulation <- function(ticks.number, trans.number,pop.size) {
  N <- pop.size
  bp <- lapply(1:N , function(i) generate.invidual(ticks.number,trans.number))
  return(bp)
}

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


load.data <- function() {
  EURPLN <- read_csv("DAT_MT_EURPLN_M1_2016.csv", 
                     col_names = FALSE, col_types = cols(X1 = col_date(format = "%Y.%m.%d"), 
                                                         X2 = col_time(format = "%H:%M"), 
                                                         X7 = col_skip()))
  colnames(EURPLN) <- c("date", "time", "price1", "price2", "price3","price4")
  EURPLN <- EURPLN %>% mutate( time = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S"))
  EURPLN$date <- NULL
  
  EURUSD <- read_csv("DAT_MT_EURUSD_M1_2016.csv", 
                     col_names = FALSE, col_types = cols(X1 = col_date(format = "%Y.%m.%d"), 
                                                         X2 = col_time(format = "%H:%M"), 
                                                         X7 = col_skip()))
  colnames(EURUSD) <- c("date", "time", "price1", "price2", "price3","price4")
  EURUSD <- EURUSD %>% mutate( time = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S"))
  EURUSD$date <- NULL
  
  USDPLN <- read_csv("DAT_MT_USDPLN_M1_2016.csv", 
                     col_names = FALSE, col_types = cols(X1 = col_date(format = "%Y.%m.%d"), 
                                                         X2 = col_time(format = "%H:%M"), 
                                                         X7 = col_skip()))
  colnames(USDPLN) <- c("date", "time", "price1", "price2", "price3","price4")
  USDPLN <- USDPLN %>% mutate( time = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S"))
  USDPLN$date <- NULL
  
  a <- USDPLN[,1:2]
  colnames(a) <- c("time" , "USDPLN") 
  b <- EURUSD[,1:2]
  colnames(b) <- c("time" , "EURUSD") 
  c <- EURPLN[,1:2]
  colnames(c) <- c("time" , "EURPLN") 
  
  prices <- a %>% full_join(b,by = "time") %>% 
    full_join(c,by="time") %>% 
    arrange(time) %>% 
    mutate( USDPLN = replace.na(USDPLN)) %>% 
    mutate(EURPLN = replace.na(EURPLN)) %>% 
    mutate(EURUSD = replace.na(EURUSD))
  return(prices)
}
prices <- load.data()

{
#plot(EURUSD$time, EURUSD$price1,type = 'l', col = 'black', xlab = "Time", ylab = "EXCH")
#par(new=TRUE)
#plot(EURPLN$time, EURPLN$price1, type = 'l', col = 'blue', xlab = "Time", ylab = "EXCH")
#par(new=TRUE)
#plot(USDPLN$time, USDPLN$price1, type = 'l', col = 'red', xlab = "Time", ylab = "EXCH")

#invest <- data.frame(
#  B = seq(1, 50*N, 50),
#  E = seq(6, 50*N + 5, 50),
#  EX = rep(1, N)
#  )
#print(evaluate(invest, K,prices))

#for(i in 1:30){
  invest <- invest_mutation(NULL , invest, 0.5, 0.5, 0.1, 1, length(prices$time), prices)
}
{
#plot(invest$B, type = 'p', col = 'red', xlab = "Maxima", ylab = "EXCH")
#points(invest$E, type = 'p', col = 'blue', xlab = "Maxima", ylab = "EXCH") # tak robimy wykres na wykresie :)
#print(evaluate(invest, K, prices))
}


N <- 100
K <- 100
ticks.number <- nrow(prices)
trans.number <- N

pB <- 0.5
pE <- 0.5
pEX <- 0.1
min <-  1
max <- length(prices$time)
popSize <- 10
maxiter <- 10
pmutation <- 0.9

#wlasciwa petla ewolucji
selection <- select.one.of.two
pop <- basePopulation(ticks.number,trans.number,popSize)

invest <- pop[[1]]

for(i in 1:maxiter){
  pop <- selection(pop,K,prices)
  #mutacja
  for(j in 1:popSize)
  {
    pop[j]<-invest_mutation(pop[j], pB, pE, pEX, min, max,prices)
  }
}

#wybor najlepszego
best <- pop[1]
bestVal<- evaluate(K, best, prices)
for(i in 2:popSize)
{
  if(evaluate(K, pop[i], prices) > bestVal)
  {
    best <- pop[i]
    bestVal<- evaluate(K, best, prices)
  }
}
plot(best$B, type = 'p', col = 'red', xlab = "Maxima", ylab = "EXCH")
points(best$E, type = 'p', col = 'blue', xlab = "Maxima", ylab = "EXCH") # tak robimy wykres na wykresie :)
print(bestVal)


