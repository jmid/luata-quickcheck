local n = tonumber(arg[1])
local lim = n * 100

if(not nextkey) then
    nextkey = next
end

local hash1={}
local i = 0
while i <= lim do
    hash1["foo_"..i] = i
    i = i + 1
end
local hash2={}
local i = 1
while i <= n do
    local k = nextkey(hash1, nil)
    while k do
        local v = hash1[k]
        if not hash2[k] then
            hash2[k] = 0
        end
        hash2[k] = v + hash2[k]
        k = nextkey(hash1, k)
    end
    i = i + 1
end

io.write(hash1["foo_1"], " ", hash1["foo_" .. lim], " ",
	 hash2["foo_1"], " ", hash2["foo_" .. lim])
