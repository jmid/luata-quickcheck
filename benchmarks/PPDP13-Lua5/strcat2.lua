-- this version uses the native string concatenation operator

local n = 1
while n <= tonumber(arg[1]) do
    local str = ""
    local i = 1
    while i <= n do
        str = str.."hello"
        i = i + 1
    end
    io.write(string.len(str), "\n")
    n = n + 1
end
