v = nil

function f(x,y)
   if x
   then 
      v = x
   else
      v = y
   end
end

f('foo',42)
