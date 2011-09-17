# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

class Command
	attr_reader :race, :command, :row, :col, :ship
	attr_reader :planet, :shipSize, :targetShip
	
	MOVE = 1
	LAUNCH = 2
	COLONIZE = 3
	INVADE = 4
	BUILD = 5
	DEFENSE = 6
	ATTACK = 7
	
	private_class_method :new
	
	def initialize(race, command, row, col, ship, planet, shipSize, targetShip)
		@race = race
		@command = command
		@row = row
		@col = col
		@ship = ship
		@planet = planet
		@shipSize = shipSize
		@targetShip = targetShip
	end
	
	def Command.move(race, ship, row, col)
		return new(race, MOVE, row, col, ship, nil, nil, nil)
	end
	
	def Command.launch(race, planet)
		return new(race, LAUNCH, nil, nil, nil, planet, nil, nil)
	end

	def Command.colonize(race, ship, planet)
		return new(race, COLONIZE, nil, nil, ship, planet, nil, nil)
	end
	
	def Command.invade(race, ship, planet)
		return new(race, INVADE, nil, nil, ship, planet, nil, nil)
	end
	
	def Command.build(race, shipSize)
		return new(race, BUILD, nil, nil, nil, nil, shipSize, nil)
	end
	
	def Command.defense(race, planet)
		return new(race, DEFENSE, nil, nil, nil, planet, nil, nil)
	end
	
	def Command.attack(race, ship, targetShip)
		return new(race, ATTACK, nil, nil, ship, nil, nil, targetShip)
	end
end
