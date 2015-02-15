-- from http://lua-users.org/lists/lua-l/2012-03/msg00583.html
local a = {};
local b = {};
b[a] = 'hallo';
print(b[a]);
local r = b[a]
print(b.a);
r = b.a
