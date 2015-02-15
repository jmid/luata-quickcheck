function A(i, j)
  local ij = i+j-1
  return 1.0 / (ij * (ij-1) * 0.5 + i)
end

function Av(x, y, N)
  local i = 1; while i <= N do
    local a = 0
    local j = 1; while j <= N do a = a + x[j] * A(i, j); j = j + 1 end
    y[i] = a
    i = i + 1
  end
end

function Atv(x, y, N)
  local i = 1; while i <= N do
    local a = 0
    local j = 1; while j <= N do a = a + x[j] * A(j, i); j = j + 1 end
    y[i] = a
    i = i + 1
  end
end

function AtAv(x, y, t, N)
  Av(x, t, N)
  Atv(t, y, N)
end

local N = tonumber(arg[1])
local u = {}; local v = {}; local t = {}
local i = 1; while i <= N do u[i] = 1; i = i + 1 end

local i = 1; while i <= 10 do AtAv(u, v, t, N); AtAv(v, u, t, N); i = i + 1 end

local vBv = 0; local vv = 0
local i = 1; while i <= N do
  local ui = u[i]; local vi = v[i]
  vBv = vBv + ui*vi
  vv = vv + vi*vi
  i = i + 1
end
io.write(math.sqrt(vBv / vv), '\n')
