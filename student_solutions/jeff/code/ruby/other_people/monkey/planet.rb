# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

class Planet
	attr_reader :owner, :row, :col, :defense
	
	HOME = 5
	LARGE = 3
	MEDIUM = 2
	SMALL = 1
	UNKNOWN = 0

	def initialize(size, row, col)
		@size = size
		@row = row
		@col = col
		@defense = 1
	end
	
	def maxDefense
		return 6
	end

	def size
		if (!owner)
			return UNKNOWN
		end
		return @size
	end
	
	def value
		return self.size
	end
	
	def owner=(race)
		@owner = race
		@defense = 1
		return nil
	end
	
	def defenseUpBy(by)
		@defense += by
		if (@defense > maxDefense)
			@defense = maxDefense
		end
		return nil
	end
	
	def defenseDownBy(by)
		@defense -= by
		if (@defense < 0)
			@defense = 0
		end
		return nil
	end

end
