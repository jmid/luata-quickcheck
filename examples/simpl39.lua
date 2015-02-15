local normal_str = " 'bar' \a \b \f \n \r \t \v \\ \" \' \9 \250 \x4a \x4A "
local char_str   = ' "bar" \a \b \f \n \r \t \v \\ \" \' \9 \250 \x4a \x4A '
local long_str   = [=[ 
                     \a \b \f \n \r \t \v \\ \" \' \z \9 \250 \x4a \x4A "
                     "foo" and 'foo'
                   ]=]
--local illegal    = "\357"
--local illegal    = "\[ \]"
--local illegal    = "\u128 "
print(normal_str)
print(char_str)
print(long_str)
