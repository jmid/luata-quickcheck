t = {1,2,3,4,5}
i = table.concat(t,",",5)
i2 = table.concat(t,",",1,1)
s = i+i2
print(table.concat(t,","),s)
