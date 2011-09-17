# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'planet'
require 'ship'
require 'race'


class Board
	attr_reader :size
	
	def initialize(size)
		@size = size
		clear
	end
	
	def clear
		@races = []
		@planets = []
		@ships = []
	end
	
	def addRace(race)
		@races << race
		return nil
	end
	
	def removeRace(race)
		@races.delete(race)
		return nil
	end
	
	def addPlanet(planet)
		@planets << planet
		return nil
	end
	
	def addShip(ship)
		@ships << ship
		return nil
	end
	
	def removeShip(ship)
		@ships.delete(ship)
		return nil
	end
	
	def races
		return @races
	end
	
	def planets(race = nil)
		result = []
		@planets.each do
			| planet |
			if (race == nil || race == planet.owner)
				result << planet
			end
		end
		return result
	end
	
	def ships(race = nil)
		result = []
		@ships.each do
			| ship |
			if (race == nil || race == ship.owner)
				result << ship
			end
		end
		return result
	end
	
end

