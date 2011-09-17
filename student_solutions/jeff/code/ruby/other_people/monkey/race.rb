# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'ship'

class Race
	attr_reader :quad, :newShipSize, :newShipLeft
	
	def initialize(quad)
		@quad = quad
	end

	def build(shipSize)
		if (@newShipSize == nil)
			@newShipSize = shipSize
			@newShipLeft = Ship.turnsToConstruct(shipSize)-1
		elsif (@newShipSize == shipSize && @newShipLeft > 0)
			@newShipLeft -= 1
		end
		return nil
	end
	
	def launch
		if (!canLaunch)
			return nil
		end
		ship = Ship.create(@newShipSize, self)
		@newShipSize = nil
		@newShipLeft = nil
		return ship
	end

	def canLaunch
		return (@newShipSize && @newShipLeft == 0)
	end
	
end
