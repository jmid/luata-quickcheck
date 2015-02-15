local x = 5
while x > 0 do
   local function g(i)
      break
      return i
   end
   g(x)
   x = x + 1
end
