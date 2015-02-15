-- from Manual 5.2, sec.3.5 'Visibility rules'

x = "10"                -- global variable
do                    -- new block
   local x = x         -- new 'x', with value 10
   print(x)            --> 10
   x = x .. 1
   do                  -- another block
      local x = x .. 1     -- another 'x'
      print(x)          --> 1011
   end
   print(x)            --> 101
end
print(x)              --> 10  (the global one)
