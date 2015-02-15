foo = {}

function f(t)
   foo = 42
   return function()
	    foo = "hello"
	    return t.x+t.y
          end
end

f {x=1,y=2} ()
local r = foo
print(r)
