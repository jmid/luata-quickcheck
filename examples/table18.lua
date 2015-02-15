local t = { }
t[1] = "foo"
local r1,r2 = t[1],t[2]
-- r1 should contain "foo", r2 should contain nil
