---
--- example adopted from
--- http://stackoverflow.com/questions/325323/is-there-anyway-to-avoid-this-security-issue-in-lua
---

mt = { __metatable = true } 
t = {}
setmetatable(t, mt)
setmetatable(t, mt) -- this should fail
