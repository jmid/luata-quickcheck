local t = { }

t[1] = "str"
t[2] = 42
t[3] = true
t[4] = print
t[5] = function (x) return x end
t[6] = { g = "inner" }

local s = {}

for k,v in ipairs(t) do
   print(k,v)
   s[k] = v
end
