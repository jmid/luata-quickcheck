function incr (v) return v + 1 end

local c = 0
local i = 1; while i <= tonumber (arg[1]) do
  c = incr (c)
  c = incr (c)
  c = incr (c)
  c = incr (c)
  c = incr (c)
  c = incr (c)
  c = incr (c)
  c = incr (c)
  c = incr (c)
  c = incr (c)
  i = i + 1
end
io.write (c, '\n')
