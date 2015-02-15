--
--  set example from book, p.129
--


Set = {}

local mt = {} -- metatable for sets

-- create a new set with the values of a given list
function Set.new (l)
   local set = {}
   setmetatable(set,mt)
   for _, v in ipairs(l) do set[v] = true end
   return set
end

function Set.union (a, b)
   local res = Set.new{}
   for k in pairs(a) do res[k] = true end
   for k in pairs(b) do res[k] = true end
   return res
end

-- function Set.intersection (a, b)
--    local res = Set.new{}
--    for k in pairs(a) do
--       res[k] = b[k]
--    end
-- end

-- presents a set as a string
function Set.tostring (set)
   local l = {}
   for e in pairs(set) do
      l[#l + 1] = e
   end
   return "{" .. table.concat(l, ", ") .. "}"
end

-- print a set
function Set.print (s)
   print(Set.tostring(s))
end


mt.__add = Set.union



s1 = Set.new{10,20,30,50}
s2 = Set.new{30,1}
s3 = s1 + s2
Set.print(s1)
Set.print(s2)
Set.print(s3)
