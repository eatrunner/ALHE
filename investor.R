
evaluate <- function(K, invest, maxima, price)
{
	i = 2
	# mnożnik {1, -1} w zależności czy posiadamy USD czy PLN
	j = -1
	while (i <= length(invest))
	{
		K = K + j*(price[maxima[invest[i]]] - price[maxima[invest[i-1]]]) * K
		i = i+1
		j = -1*j
	}
	return(K)
}
N <- 100
K <- 100

data <- read.csv("Parsed_file.csv")
# jpeg(file = "line_chart_label_colored.jpg")
X11()
plot(data$price,type = 'l', col = 'black', xlab = "Time", ylab = "EUR/USD")
print(length(data$price))
maxima = which(diff(sign(diff(data$price)))==-2)+1
X11()
plot(maxima, data$price[maxima], type = 'l', col = 'black', xlab = "Time", ylab = "EUR/USD")
print(length(maxima))
print(length(maxima)/length(data$price))

# Niewięcej transakcji niz maximów
if (length(maxima) < N)
{
	N <- length(maxima)
}

# Parzysta ilosc transakcji
if (N%%2 == 1)
{
	N <- N - 1
}

invest <- 1:N


print(evaluate(K, invest, maxima, data$price))

lines(maxima[invest], data$price[maxima[invest]], type = 'p', col = 'red', xlab = "Time", ylab = "EUR/USD")


Sys.sleep(60)
