function f(t)
   local x,y = t.x,t.y
   print(x,y)
   return x+y
end

local r = f {x=1,y=2}
