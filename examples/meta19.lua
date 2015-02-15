---
--- example adopted from
--- http://stackoverflow.com/questions/325323/is-there-anyway-to-avoid-this-security-issue-in-lua
---

mt = {} 
t = {}
setmetatable(t, mt)
newmt = nil
if (unknown) then newmt = {} end
setmetatable(t, newmt) -- this may delete or set another mt (due to imprecision)
mt2 = getmetatable(t)
print(mt,mt2)
