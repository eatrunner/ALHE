import sys
if len(sys.argv) < 3:
	print("Not enough arguments. Provide inputfile and output file")
	exit()
f = open(sys.argv[1], 'r')
data = f.read()
f.close()
# data = data.replace(" ", "")
newdata = "date, time, price, price1, price2, price3, val\n" + data.replace(";",",")
f = open(sys.argv[2], 'w')
f.write(newdata)
f.close()