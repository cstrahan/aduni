# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

class Player
	attr_reader :game, :race

	def initialize(skill = 0)
		@skill = skill	
	end
		
	def joinGame(game, race)
		if (@game || @race)
			return false
		end
		if (@refuseToJoin)
			return false
		end
		@game = game
		@race = race
		return true
	end
	
	def leaveGame(game)
		if (@game == game)
			@game = nil
			@race = nil
		end
		return nil
	end
	
	def setViewCommand(vc)
		# virtual method overridden by Human subclass
	end
	
	def launch
		planets = @game.planets(@race)
		if(@skill >= 2)
			planet = planets[rand(planets.size)]
		else
			planet = planets[0]
		end
		cmd = Command.launch(@race, planet)
		@game.doCommand(cmd)
		return nil
	end
	
	def build(size)
		cmd = Command.build(@race, size)
		@game.doCommand(cmd)
		return nil
	end
	
	def move(ship, row, col)
		cmd = getSingleMove(ship, row, col)
		if (@game.isLegal(cmd))
			@game.doCommand(cmd)
		else
			attackIfClose(ship)
		end
		return nil
	end		

	def colonize(ship, planet)
		cmd = Command.colonize(@race, ship, planet)
		@game.doCommand(cmd)
		return nil
	end
	
	def invade(ship, planet)
		cmd = Command.invade(@race, ship, planet)
		@game.doCommand(cmd)
		return nil
	end
	
	def defense(planet)
		cmd = Command.defense(@race, planet)
		@game.doCommand(cmd)
		return nil
	end
	
	def attack(ship, target)
		cmd = Command.attack(@race, ship, target)
		@game.doCommand(cmd)
		return nil
	end
	
	def nothing(ship)
		row = rand(@game.boardSize)
		col = rand(@game.boardSize)
		cmd = getSingleMove(ship, row, col)
		@game.doCommand(cmd)
		return nil
	
	end


	def closestFreePlanet(ship)
		dest = closestItemConditional(ship, @game.planets) do
			| ship, planet |
			planet.owner == nil
		end
	end
	
	def closestEnemyItem(from, toList)
		dest = closestItemConditional(from, toList) do
			| from, to |
			(to.owner != nil && to.owner != from.owner)
		end
	end

	def closestItemConditional(from, toList)
		dest = nil
		closest = 9999
		toList.each do
			| item |
			if (yield(from, item))
				distance = distance(from, item)
				if (distance < closest)
					dest = item
					closest = distance
				end
			end
		end
		return dest
	end
	
	def distance(from, to)
		return (from.row-to.row).abs + (from.col-to.col).abs
	end
	
	def getSingleMove(ship, row, col)
		rowDelta = row <=> ship.row
		if (rowDelta == 0)
			colDelta = col <=> ship.col
		else
			colDelta = 0
		end
		
		row = ship.row + rowDelta
		col = ship.col + colDelta
		return Command.move(@race, ship, row, col)
	end
	
	def attackIfClose(ship)
		dest = closestEnemyItem(ship, @game.ships)
		if (dest)
			if (distance(ship, dest) == 1)
				attack(ship, dest)
				return nil
			end
		end

		nothing(ship)
	end

	def findWeakPlanet(planets)
		planets.each do
			| planet |
			if (planet.defense < planet.maxDefense)
				return planet
			end
		end
		return nil
	end
	
	def findFreePlanets
		planets = []
		@game.planets.each do
			| planet |
			if (!planet.owner)
				planets << planet
			end
		end
		return planets
	end
end

# skill definitions:
# 0 = original blue/green
# 1 = original yellow, except defend before build
# 2 = improved yellow
class PlayerSimple < Player
	def initialize(skill = 0)
		super
		if(@skill == 1)
			@shipType = Ship::BATTLESTAR
		else
			@shipType = Ship::MONITOR
		end
	end
	
	def takeTurn
		shipType = @shipType
		weakest = findWeakPlanet(@game.planets(@race))
		freeCount = findFreePlanets.size

		if (@skill >=1 && freeCount == 0 && weakest )
			defense(weakest)
			return nil
		end

		if (@race.canLaunch)
			launch
			return nil
		end
		
		ships = @game.ships(@race)
		if (ships.size == 0)
			if(@skill > 1 && freeCount > 0)
				shipType = Ship::CRUISER
			end
			build(shipType)
			return nil
		end

		ship = ships[0]		
		closestFree = closestFreePlanet(ship)
		if (closestFree)
			if (distance(ship, closestFree) == 0)
				colonize(ship, closestFree)
				return nil
			end

			move(ship, closestFree.row, closestFree.col)
			return nil
		end
		
		closestPlanet = closestEnemyItem(ship, @game.planets)
		if (closestPlanet)
			if (distance(ship, closestPlanet) == 0)
				invade(ship, closestPlanet)
				return nil
			end
			move(ship, closestPlanet.row, closestPlanet.col)
			return nil
		end

		attackIfClose(ship)
		return nil
	end
	

end

class PlayerHuman < Player
	def initialize(infoPointer)
		super()
		@infoPointer = infoPointer
	end
	
	def joinGame(game, race)
		super
		@dlg.setPlayer(self)
		planet = game.planets(race)[0]
		@infoPointer.moveTo(planet.row, planet.col)
		return true
	end
	
	def setViewCommand(viewCommand)
		@dlg = viewCommand
		return nil
	end

	def rowDelta(dir)
		delta = {'n' => -1, 's' => 1, 'e' => 0, 'w' => 0}
		if (!delta[dir])
			return 0
		end
		return delta[dir]
	end
	
	def colDelta(dir)
		delta = {'n' => 0, 's' => 0, 'e' => 1, 'w' => -1}
		if (!delta[dir])
			return 0
		end
		return delta[dir]
	end
	
	def shipType(char)
		types = {'c' => Ship::CRUISER, 'b' => Ship::BATTLESTAR,
					'd' => Ship::DREADNOUGHT, 'm' => Ship::MONITOR}
		type = types[char]
		if (!type)
			type = Ship::MONITOR
		end
		return type
	end
		
	def createCommand(string)
		cmd = nil
		parts = string.downcase.split(' ')
		if (!parts[1])
			parts[1] = 0
		end
		if (!parts[2])
			parts[2] = 0
		end
		case parts[0]
			when 'a'
				row = parts[1].to_i
				col = parts[2].to_i
				ship = getShip(row, col)
				if (ship)
					destRow = ship.row + rowDelta(parts[3])
					destCol = ship.col + colDelta(parts[3])
					target = getShip(destRow, destCol)
					if (target)
						cmd = Command.attack(@race, ship, target)
					end
				end
			when 'b'
				cmd = Command.build(@race, shipType(parts[1]))
			when 'c'
				row = parts[1].to_i
				col = parts[2].to_i
				ship = getShip(row, col)
				if (ship)
					planet = getPlanet(ship.row, ship.col)
					if (planet)
						cmd = Command.colonize(@race, ship, planet)
					end
				end
			when 'i'
				row = parts[1].to_i
				col = parts[2].to_i
				ship = getShip(row, col)
				if (ship)
					planet = getPlanet(ship.row, ship.col)
					if (planet)
						cmd = Command.invade(@race, ship, planet)
					end
				end
			when 'd'
				row = parts[1].to_i
				col = parts[2].to_i
				planet = getPlanet(row, col)
				if (planet)
					cmd = Command.defense(@race, planet)
				end
			when 'l'
				row = parts[1].to_i
				col = parts[2].to_i
				planet = getPlanet(row, col)
				if (planet)
					cmd = Command.launch(@race, planet)
				end
			when 'm'
				row = parts[1].to_i
				col = parts[2].to_i
				ship = getShip(row, col)
				if (ship)
					destRow = ship.row + rowDelta(parts[3])
					destCol = ship.col + colDelta(parts[3])
					cmd = Command.move(@race, ship, destRow, destCol)
				end
			else
				puts "Can\'t parse #{string}"
		end
		return cmd
	end
	
	def getPlanet(row, col)
		return @game.planets.find do
			| planet |
			planet.row == row && planet.col == col
		end
	end

	def getShip(row, col)
		return @game.ships.find do
			| ship |
			ship.row == row && ship.col == col
		end
	end

	def takeTurn
		begin
			cmd = createCommand(@dlg.getCommand)
			@dlg.clear
		end until (@game.isLegal(cmd))
		@game.doCommand(cmd)
		return nil
	end
end
