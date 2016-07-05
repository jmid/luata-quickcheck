local complexMt = { __metatable = "Complex number metatable protected" }

local function Complex(real,imag)
   local n = {real = real, imag = imag}
   setmetatable(n, complexMt)
   print(getmetatable(n))
   return n
end

function complexMt.__add(x,y)
   return Complex(x.real + y.real, x.imag + y.imag)
end

function complexMt:__tostring()
   return self.real .. "+" .. self.imag .. "i"
end

z = Complex(1,0) + Complex(0,2)
print(z)
