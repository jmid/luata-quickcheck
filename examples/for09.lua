local t = { }

t["str"] = "a"
t[42] = "b"
t[true] = "c"
t[print] = "d"
t[function (x) return x end] = "e"
t[{ g = "inner" }] = "f"

local s = {}

for k,v in pairs(t) do
   print(k,v)
   s[k] = v
end
