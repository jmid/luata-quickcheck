local s = {a=1,b=2,c=3}
local t = {}

for key, value in next,s,nil do
   print(key,value)
   t[key] = value
end


