--
-- An adapted example from http://lua-users.org/wiki/MetamethodsTutorial
--
local z = { value   = 3, --- fix parser
	    __index = function (lhs, rhs) -- "index" event handler
	                return { value = lhs.value ^ rhs.value }
	              end }

local x = { value   = 5 }
x.__index = z

setmetatable(z, z) -- use z itself as the metatable for "z"
setmetatable(x, x) -- use x itself as the metatable for "x"

local y = x[x]
local yval = y.value
print(yval) --> 3125
