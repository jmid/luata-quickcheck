x = 0 -- a global counter

-- a coroutine that can increment the counter (through side effects)
local co = coroutine.create(function (arg)
			       print("arg: ", arg);
			       x = x + 1
			       arg = coroutine.yield("first")
			       print("arg: ", arg);
			       x = x + 1
			       arg = coroutine.yield("second")
			       print("arg: ", arg);
			       x = x + 1
			       return "I'm done"
			    end)

print(x)
local b,arg = coroutine.resume(co, "resume1")
print(b,arg)
print(x)
b,arg = coroutine.resume(co, "resume2")
print(b,arg)
print(x)
b,arg = coroutine.resume(co, "resume3")
print(b,arg)
print(x)
b,arg = coroutine.resume(co, "resume4")
print(b,arg)

