class
    COUNTER

	-- Count events

creation
    make
    
feature -- Creation
    
    make is 
	do
	    value = 0
	end

feature -- Queries

    value: INTEGER
	    -- Current value of counter
    
feature -- Modification
    
    increment is
	do
	    value := value + 1
	ensure
	    incremented: value = old value + 1
	end

invariant
    positive: value >= 0

end
