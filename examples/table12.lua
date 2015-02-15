-- support ; as field separator?
function make_node(head,tail)
   return { hd = head,
	    tl = tail }
end

function print_list(node)
   if (node == nil)
   then return nil
   else
      print(node.hd, ' ')
      print_list(node.tl)
   end
end

local list = make_node(1, make_node(2, make_node(3, nil)))

print_list(list)
list.tl.tl = nil
print_list(list)
local r = list.tl
