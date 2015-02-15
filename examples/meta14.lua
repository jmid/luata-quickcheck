--
-- An adapted example from http://lua-users.org/wiki/MetamethodsTutorial
--

local x = {value = 5}

local mt = {
  __pow = function (lhs, rhs) -- "sub" event handler
    return 1,2,3
  end
}

setmetatable(x, mt) -- use "mt" as the metatable for "x"

local a,b,c = x ^ x
print(a,b,c) --> 1,nil,nil
