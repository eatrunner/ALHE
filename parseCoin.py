import sys
if len(sys.argv) < 3:
	print("Not enough arguments. Provide inputfile and output file")
	exit()
f = open(sys.argv[1], 'r')
data = f.read()
f.close()
# data = data.replace(" ", "")
newdata = "stamp, price, volume\n" + data
f = open(sys.argv[2], 'w')
f.write(newdata)
f.close()