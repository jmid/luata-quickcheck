function f()
   return "foo", true
end

local l
local t = {}
t.r1,l,t.r2,t.r3,r4 = f(),"jens"
print(t.r1,l,t.r2,t.r3,r4)
