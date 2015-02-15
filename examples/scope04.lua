function foo ()
   local iter = nil
   iter = function (i)
      if (i==0)
      then return "done"
      else return iter(i-1)
      end
   end
   return iter(10)
end

local r = foo()
print(r)
r = iter
