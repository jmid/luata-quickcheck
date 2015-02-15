--
-- An adapted example from http://lua-users.org/wiki/MetamethodsTutorial
--

local x = { value   = 5, -- fix ; as separator
	    __index = function (lhs, rhs) -- "index" event handler
	                return { value = lhs.value ^ rhs.value }
	              end }

setmetatable(x, x) -- use x itself as the metatable for "x"

local y = x[x]
local yval = y.value
print(yval) --> 3125
