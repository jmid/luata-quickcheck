--
-- An adapted example from http://lua-users.org/wiki/MetamethodsTutorial
--
local z,x

z = { value   = 3 } --- fix parser
x = { value   = 5 }

x.__index = z
z.__index = x

setmetatable(z, z) -- use z itself as the metatable for "z"
setmetatable(x, x) -- use x itself as the metatable for "x"

local y = x[x]
--local yval = y.value
print(y) --> 3125
