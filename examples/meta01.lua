--
-- An example from http://lua-users.org/wiki/MetamethodsTutorial
--

local x = {value = 5}

local mt = {
  __add = function (lhs, rhs) -- "add" event handler
    return { value = lhs.value + rhs.value }
  end
}

setmetatable(x, mt) -- use "mt" as the metatable for "x"
local m = getmetatable(x)
print("same?",m,mt)
