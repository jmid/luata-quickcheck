do
   local old_print = print
   local print = "hest"
   old_print(print)
end
print(4)
