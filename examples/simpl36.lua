function f ()
   return 42, "hello"
end

local x,y = f()
print(x,y)
local a,b = (f())
print(a,b)
