--
-- An adapted example from http://lua-users.org/wiki/MetamethodsTutorial
--

local x = {value = 5}

local mt = {
  __index = function (lhs, rhs) -- "index" event handler
    return { value = lhs.value ^ rhs.value }
  end
}

setmetatable(x, mt) -- use "mt" as the metatable for "x"

local y = x[x]
local yval = y.value
print(yval) --> 125
