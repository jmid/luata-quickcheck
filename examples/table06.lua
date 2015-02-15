local t = {
   42,
   true,
   function (x) return x end,
   'hello' }

local i = 1

while i < 5 do
   x = t[i]
end
