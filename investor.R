data <- read.csv("Parsed_file.csv")
# jpeg(file = "line_chart_label_colored.jpg")
X11()
plot(data$val1,type = 'p', col = 'black', xlab = "Time", ylab = "EUR/USD")
stock <- data$val1

maxima = which(diff(sign(diff(data$val1)))==-2)+1
X11()
plot(maxima, data$val1[maxima], type = 'p', col = 'black', xlab = "Time", ylab = "EUR/USD")

Sys.sleep(60)
