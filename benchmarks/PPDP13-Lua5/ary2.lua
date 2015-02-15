local n = tonumber(arg[1])

local x = {}
local y = {}
local i = 0
while i < n do
    x[i] = i; i = i + 1
    x[i] = i; i = i + 1
    x[i] = i; i = i + 1
    x[i] = i; i = i + 1
    x[i] = i; i = i + 1

    x[i] = i; i = i + 1
    x[i] = i; i = i + 1
    x[i] = i; i = i + 1
    x[i] = i; i = i + 1
    x[i] = i; i = i + 1
end

i = n-1
while i >= 0 do
    y[i] = x[i]; i = i - 1
    y[i] = x[i]; i = i - 1
    y[i] = x[i]; i = i - 1
    y[i] = x[i]; i = i - 1
    y[i] = x[i]; i = i - 1

    y[i] = x[i]; i = i - 1
    y[i] = x[i]; i = i - 1
    y[i] = x[i]; i = i - 1
    y[i] = x[i]; i = i - 1
    y[i] = x[i]; i = i - 1
end

io.write(y[n-1], "\n")
