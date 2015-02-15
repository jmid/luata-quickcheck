-- http://www.bagley.org/~doug/shootout/
-- from Roberto Ierusalimschy

if(not nextkey) then
    nextkey = next
end

-- this version uses a custom string buffer

------------------------------------------------------------------
-- Buffer library
------------------------------------------------------------------

Buffer = {n=0}

function Buffer.new (self)
  local new = {}
  local k = nextkey(self, nil)
  local v = nil
  while k do  v = self[k]; new[k] = v; k = nextkey(self, k)  end
  new.n = 0
  return new
end

function tinsert(t, v)
  t.n = t.n + 1
  t[t.n] = v
end

function tremove(t)
  local v = t[t.n]
  t[t.n] = nil
  t.n = t.n - 1
  return v
end

function Buffer.add (self, s)
  tinsert(self, s)
  local i = self.n-1
  while i > 0 do
    if (string.len(self[i]) <= string.len(self[i+1])) then 
      local top = tremove(self)
      self[i] = self[i]..top
    end
    i = i - 1
  end
end

function Buffer.close (self)
  for i=self.n-1, 1, -1 do
    local top = tremove(self)
    self[i] = self[i]..top
  end
  return self[1]
end


------------------------------------------------------------------
-- Test
------------------------------------------------------------------

local n = 1
while n <= tonumber(arg[1]) do
    local buff = Buffer.new(Buffer)
    local i = 1
    while i <= n do
      buff.add(buff, "hello")
      i = i + 1
    end
    hellostring = buff.close(buff)
    io.write(string.len(hellostring), "\n")
    n = n + 1
end
