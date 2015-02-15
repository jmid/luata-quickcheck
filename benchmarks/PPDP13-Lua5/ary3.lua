local last = tonumber(arg[1]) - 1
local x = {}
local y = {}

local i = 0
while i <= last do
  x[i] = i + 1
  y[i] = 0
  i = i + 1
end
local k = 1
while k <= 1000 do
  local j = last
  while j >= 0 do
    y[j] = y[j] + x[j]
    j = j - 1
  end
  k = k + 1
end

io.write(y[0], " ", y[last], "\n")
