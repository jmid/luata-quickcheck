local n = tonumber(arg[1])

local x = {}
local y = {}
local i = 1
while i <= n do
  x[i] = i
  i = i + 1
end

local j = n
while j >= 1 do
  y[j] = x[j]
  j = j - 1
end

io.write(y[n-1], "\n")
