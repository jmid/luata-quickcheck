local even = function (i)
   return odd(i-1)
end

local odd = function (i)
   return false
end

local r = even(10)
print(r)
