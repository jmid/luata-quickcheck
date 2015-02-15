function f()
   return "foo", true
end

local l
local t = {}
t.r1,l,t.r2,r3 = f(),"jens",f(),"hest",42
print(t.r1,l,t.r2,r3)
