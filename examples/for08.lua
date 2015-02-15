local t = { a = "str",
	    b = 42,
	    c = true,
	    d = print,
	    e = function (x) return x end,
	    f = { g = "inner" }
}
local s = {}

for k,v in pairs(t) do
   print(k,v)
   s[k] = v
end
