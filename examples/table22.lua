local t = { }

t["str"] = "a"
t[42] = "b"
t[true] = "c"
t[print] = "d"
t[function (x) return x end] = "e"
t[{ g = "inner" }] = "f"

local s = {}

local k,v = next(t,nil)

while k do
-- print(k,v)
   s[k] = v
   k,v = next(t,k)
end
