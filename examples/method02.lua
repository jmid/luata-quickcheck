local o = { x = 42 }
function o:get() return self.x end
function o:set(val) self.x = val end
function o:getself() return self end

o:getself():getself():getself():set("boo") -- methcallstmt
r = o:getself():getself():getself():get() -- methcall
print(r)
