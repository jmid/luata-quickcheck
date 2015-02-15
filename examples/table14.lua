local t = { a = "str",
	    b = 42,
	    c = true,
	    d = print,
	    e = function (x) return x end,
	    f = { g = "inner" }
}

local k,v = next(t,nil)
print(k,v)
