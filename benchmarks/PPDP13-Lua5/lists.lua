if(not nextkey) then
    nextkey = next
end

--------------------------------------------------------------
-- List module
-- defines a prototipe for lists
--------------------------------------------------------------

List = {first = 0, last = -1}

function List.new (self)
  local n = {}
  local k = nextkey(self, nil)
  local v = nil
  while k do
     v = self[k]
     n[k] = v
     k = nextkey(self, k)
  end
  return n
end

function List.length (self)
  return self.last - self.first + 1
end

function List.pushleft (self, value)
  local first = self.first - 1
  self.first = first
  self[first] = value
end

function List.pushright (self, value)
  local last = self.last + 1
  self.last = last
  self[last] = value
end

function List.popleft (self)
  local first = self.first
  if first > self.last then error("list is empty") end
  local value = self[first]
  self[first] = nil  -- to allow collection
  self.first = first+1
  return value
end

function List.popright (self)
  local last = self.last
  if self.first > last then error("list is empty") end
  local value = self[last]
  self[last] = nil  -- to allow collection
  self.last = last-1
  return value
end

function List.reverse (self)
  local i = self.first
  local j = self.last
  while i<j do
    local t = self[i]
    self[i] = self[j]
    self[j] = t
    i = i+1
    j = j-1
  end
end

function List.equal (self, otherlist)
  if self.length(self) ~= otherlist.length(self) then return nil end
  local diff = otherlist.first - self.first
  local i1 = self.first
  while i1 <= self.last do
    if self[i1] ~= otherlist[i1+diff] then return nil end
    i1 = i1 + 1
  end
  return 1
end

-----------------------------------------------------------
-----------------------------------------------------------

-- Some tests

function test (testsize)
  local SIZE = testsize
  -- create a list with elements 1..SIZE
  local l1 = List.new(List)
  local i = 1
  while i <= SIZE do
    l1.pushright(l1, i)
    i = i + 1
  end
  -- creates a copy of l1
  local l2 = l1.new(l1)
  -- remove each individual item from left side of l2 and
  -- append to right side of l3 (preserving order)
  local l3 = List.new(List)
  while l2.length(l2) > 0 do
    l3.pushright(l3, l2.popleft(l2))  
  end
  -- remove each individual item from right side of l3 and
  -- append to right side of l2 (reversing list)
  while l3.length(l3) > 0 do
    l2.pushright(l2, l3.popright(l3))
  end
  -- reverse l1 in place
  l1.reverse(l1)
  -- compare Li1 and Li2 for equality
  -- and return length of the list
  if not l1.equal(l1, l2) then return nil
  else return l1.length(l1)
  end
end

local testsize = tonumber(arg[1])
io.write(test(testsize), '\n')
