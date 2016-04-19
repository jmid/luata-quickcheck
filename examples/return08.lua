-- From Eric Mertens
local function f(x)
   if x > 10 then
      return 1,2
   else
      return 3
   end
end

a,b=f(0)
c=type(b)
print(c)
