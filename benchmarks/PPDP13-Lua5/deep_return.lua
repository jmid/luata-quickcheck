-- This benchmark is designed to investigate the performance of a deeply nested
-- return statement.  Due to each of the loop iteration involving 3 local
-- variable lookups, compilation ought to have a big impact.
function deep_return ()
    local max = 1
    local a = 1; while a <= max do a = a + 1
    local b = 1; while b <= max do b = b + 1
    local c = 1; while c <= max do c = c + 1
    local d = 1; while d <= max do d = d + 1
    local e = 1; while e <= max do e = e + 1
    local f = 1; while f <= max do f = f + 1
    local g = 1; while g <= max do g = g + 1
    local h = 1; while h <= max do h = h + 1
    local i = 1; while i <= max do i = i + 1
    local j = 1; while j <= max do j = j + 1
    local k = 1; while k <= max do k = k + 1
    local l = 1; while l <= max do l = l + 1
    local m = 1; while m <= max do m = m + 1
    local n = 1; while n <= max do n = n + 1
    local o = 1; while o <= max do o = o + 1
    local p = 1; while p <= max do p = p + 1
    local q = 1; while q <= max do q = q + 1
    local r = 1; while r <= max do r = r + 1
    local s = 1; while s <= max do s = s + 1
    local t = 1; while t <= max do t = t + 1
    local u = 1; while u <= max do u = u + 1
    local v = 1; while v <= max do v = v + 1
    local w = 1; while w <= max do w = w + 1
    local x = 1; while x <= max do x = x + 1
    local y = 1; while y <= max do y = y + 1
    local z = 1; while z <= max do z = z + 1
        return 42
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
    end
end

local i = 1; while i <= tonumber (arg[1]) do i = i + 1
    deep_return ()
end
io.write (deep_return (), "\n", i)
