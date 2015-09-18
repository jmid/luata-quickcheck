local t = { }

t["nonnumber"] = "str"

local s = {}

for k,v in ipairs(t) do
   print(k,v)
   s[k] = v
end
