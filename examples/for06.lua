local t = { a = "str",
	    b = 42,
	    c = true,
	    d = print,
	    e = function (x) return x end,
	    f = { g = "inner" }
}
local s = {}

for k,v in next,t,nil do
   print(k,v)
   s[k] = v
end
