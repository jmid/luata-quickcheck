--
-- An adapted example from http://lua-users.org/wiki/MetamethodsTutorial
--

local x = { value   = 5 }
x.__index = x  -- according to interpreter spec, this should loop
               -- http://www.lua.org/manual/5.1/manual.html#2.8
               -- Lua impl catches the loop though
setmetatable(x, x) -- use x itself as the metatable for "x"

local y = x[x]
--local yval = y.value
print(y) --> nil
