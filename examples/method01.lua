local o = { x = 42 }
function o:get() return self.x end
function o:set(val) self.x = val end

o:set("boo") -- methcallstmt
r = o:get() -- methcall
print(r)
