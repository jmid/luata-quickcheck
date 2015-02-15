local t = { "str",
	    42,
	    true,
	    print,
	    function (x) return x end,
	    { g = "inner" }}

local s = {}

for k,v in ipairs(t) do
   print(k,v)
   s[k] = v
end
