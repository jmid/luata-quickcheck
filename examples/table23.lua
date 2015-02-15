local t = { "str",
	    42;
	    f = { "str3",
		  44;
		  g = function (x,y)
		        local foo = x + y
			foo = foo * 2
			return foo
		      end,
		  f = "abc"
	    }
}

local k,v = next(t,nil)

while k do
   print(k,v)
   k,v = next(t,k)
end
