function f(t)
   return function()
            local x,y = t.x,t.y
	    print(x,y)
	    return x+y
          end
end

local r = f {x=1,y=2} ()
