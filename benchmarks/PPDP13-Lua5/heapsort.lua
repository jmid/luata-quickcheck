LAST = 42
function gen_random(max)
       LAST = (LAST * 3877 + 29573) % 139968
       return( (max * LAST) / 139968 )
end

function heapsort(n, ra)
    local j = nil
    local i = nil
    local rra = nil
    local l = math.floor(n/2) + 1
    local ir = n;
    while 1 do
        if l > 1 then
            l = l - 1
            rra = ra[l]
        else
            rra = ra[ir]
            ra[ir] = ra[1]
            ir = ir - 1
            if (ir == 1) then
                ra[1] = rra
                return nil
            end
        end
        i = l
        j = l * 2
        while j <= ir do
            if (j < ir) then
                if (ra[j] < ra[j+1]) then
                    j = j + 1
                end
            end
            if rra < ra[j] then
                ra[i] = ra[j]
                i = j
                j = j + i
            else
                j = ir + 1
            end
        end
        ra[i] = rra
    end
end

local ary = {}
local N = tonumber(arg[1])

local i = 1
while i <= N do
    ary[i] = gen_random(1.0)
    i = i + 1
end

heapsort(N, ary)
io.write(ary[N], "\n")
