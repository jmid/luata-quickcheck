function foo ()
   local even = nil
   local odd = nil
   even = function (i)
--   local function even(i)
--   function even(i)
      if (i==0)
      then return true
      else return odd(i-1)
      end
   end
   odd = function (i)
--   local function odd(i)
--   function odd(i)
      if (i==0)
      then return false
      else return even(i-1)
      end
   end
   return even(10)
end

local r = foo()
print(r)

