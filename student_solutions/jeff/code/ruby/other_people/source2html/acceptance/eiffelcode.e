class
    EIFFELCODE
	-- This is a comment
feature
    make is
	local
	    c: COUNTER
	do
	    create c.make
	    c.increment
	    io.put_string ("Hello")
	    io.put_integer (c.value)
	    io.put_string ("%N")
	end
end
