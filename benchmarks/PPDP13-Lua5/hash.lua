local n = tonumber(arg[1])

local X={}
local i = 1
while i <= n do
  X[string.format("%x", i)] = i
  i = i + 1
end

local c = 0
local i = n
while i >= 1 do
  if X[tostring(i)] then c = c+1 end
  i = i - 1
end
io.write(c, '\n')
