import sys
if len(sys.argv) < 3:
	print("Not enough arguments. Provide inputfile and output file")
	exit()
f = open(sys.argv[1], 'r')
data = f.read()
f.close()
# data = data.replace(" ", "")
newdata = "date, val1, val2, val3, val4, val5\n" + data.replace(";",",")
f = open(sys.argv[2], 'w')
f.write(newdata)
f.close()