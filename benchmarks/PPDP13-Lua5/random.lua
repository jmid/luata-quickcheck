LAST = 42
function gen_random(max)
       LAST = (LAST * 3877 + 29573) %  139968
       return( (max * LAST) / 139968 )
end

local N = tonumber(arg[1])
local i = 1
while i <= N do
    gen_random(100)
    i = i + 1
end
io.write(gen_random(100), "\n")
