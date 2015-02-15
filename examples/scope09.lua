local function run(runs)
   local function iter(i)
      if i <= 0
      then print('done')
      else
	 print('going')
	 iter(i-1)
      end
   end
   iter(runs)
end

local r = run(5)
print(r)
