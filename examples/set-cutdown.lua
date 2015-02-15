--  Set example from "Programming in Lua", p.129

Set = {}

-- create a new set with the values of a given list
function Set.new (l)
   local set = {}
   for _, v in ipairs(l) do set[v] = true end
   return set
end

s1 = Set.new{10,20,30,50}
s2 = Set.new{30,1}

