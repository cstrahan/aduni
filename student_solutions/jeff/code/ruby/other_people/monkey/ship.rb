# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

class Ship
	attr_reader :size, :row, :col, :owner
	
	CRUISER = 4
	BATTLESTAR = 8
	DREADNOUGHT = 10
	MONITOR = 12
	
	def initialize(size, owner)
		@size = size
		@owner = owner
	end
	
	def Ship.create(size, owner)
		ship = new(size, owner)
		return ship
	end
	
	def Ship.isValidSize(size)
		if ([CRUISER, BATTLESTAR, DREADNOUGHT, MONITOR].index(size) )
			return true
		end
		return false
	end
	
	def Ship.turnsToConstruct(size)
		return size
	end
	
	def moveTo(row, col)
		@row = row
		@col = col
	end
	
	def attack
		return @size
	end
	
	def defense
		return @size
	end
	
	def Ship.name(size)
		case size
			when CRUISER
				return "Cruiser"
			when BATTLESTAR
				return "Battlestar"
			when DREADNOUGHT
				return "Dreadnought"
			when MONITOR
				return "Monitor"
		end
		return ""
	end
	
	private_class_method :new
end
