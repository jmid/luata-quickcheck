function main(max)
    local flags = {}
    local num_prime = 0
    for i=2,max do
        flags[i] = 1;
    end
    for i=2,max do
        if flags[i] == 1 then
            local k = i + i; while k <= max do
                flags[k] = 0
                k = k + i
            end
            num_prime = num_prime + 1
        end
    end
    return num_prime
end

local max = tonumber(arg[1])
local num_prime = main(max)
io.write("Number of primes less than or equal to ", max, ": ", num_prime)
