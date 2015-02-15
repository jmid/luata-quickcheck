function gen_random() return 139968 end

ary = {}
while true do
    ary[1] = gen_random()
end

r = ary[1]
print(r)
