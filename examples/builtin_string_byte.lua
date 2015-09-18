str = "12,34,56"
i = string.byte(str) - 48
i2 = string.byte(str,4,5) - 48
s = i+i2
print(i,i2,s)
