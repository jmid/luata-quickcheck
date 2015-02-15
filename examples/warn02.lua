Set = {}

function Set.tostring (set)
   local l = {}
   return "{" .. table.cancan(l, ", ") .. "}"
end

function Set.print (s)
   print(Set.tostring(s))
end

Set.print({10,20,30})
print('hello')
