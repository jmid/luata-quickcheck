-- support ; as field separator?
function make_node(head,tail)
   return { hd = head,
	    tl = tail }
end

-- function print_list(node)
--    if (node == nil)
--    then return nil
--    else
--       print(node.hd, ' ')
--       print_list(node.tl)
--    end
-- end

local list = nil
local point = nil
for i=1,10 do
   list = make_node(i, list)
   if (i == 5)
   then
      list.foo = "hey jude"
      point = list  -- recordmiddle of list pointer
   end
end

local r = point.foo
print(r)

-- print_list(list)
--list.tl.tl = nil
--print_list(list)
--local r = list.tl
