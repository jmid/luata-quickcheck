---
--- example adopted from
--- http://stackoverflow.com/questions/325323/is-there-anyway-to-avoid-this-security-issue-in-lua
---

mt = {} 
t = {}
setmetatable(t, mt)
setmetatable(t, mt) -- this shouldn't fail
mt2 = getmetatable(t)
print(mt,mt2)
