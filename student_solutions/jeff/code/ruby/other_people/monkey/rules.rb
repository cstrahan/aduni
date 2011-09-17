# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'board'
require 'command'

class Rules
	MAX_RACES = 4

	def size
		return 19
	end

	def createGenericPlanets(board)
		4.times do
			| quad |
			addPlanet(board, quad, Planet::LARGE)
			addPlanet(board, quad, Planet::MEDIUM)
			addPlanet(board, quad, Planet::MEDIUM)
			addPlanet(board, quad, Planet::SMALL)
			addPlanet(board, quad, Planet::SMALL)
		end
	end

	def createRace(board)
		if (!canAddRace(board))
			return nil
		end
		race = Race.new(nextQuad(board.races.size))
		board.addRace(race)
		planet = createHomePlanet(board, race)
		return race
	end
	
	def destroyRace(board, race)
		board.planets(race).each do
			| planet |
			planet.owner = nil
		end
		board.ships(race).each do
			| ship |
			removeShip(ship)
		end
		board.removeRace(race)
	end
	
	def hasLost(board, race)
		if (board.planets(race).size == 0 && board.ships(race).size == 0)
			return true
		end
		return false
	end

	def victoryPoints(board, race)
		score = 0
		board.planets(race).each do
			| planet |
			score += planet.value
		end
		return score
	end
		
	def hasWon(board, race)
		return victoryPoints(board, race) >= 31
	end
	
	def attemptInvade(board, cmd)
		roll = yield(cmd.ship.attack) + 1
		case roll
			when 1..2
				board.removeShip(cmd.ship)
			when 3..5
				if (!repelShip(board, cmd.ship))
					board.removeShip(cmd.ship)
				end
			when 6
				# nothing happens
			when 7..99
				cmd.planet.defenseDownBy(2)
				if (cmd.planet.defense <= 0)
					cmd.planet.owner = cmd.race
				end
			else
				raise "BAD dice roll"
		end
		return nil
	end
	
	def attemptAttack(board, cmd)
		roll1, roll2 = yield(cmd.ship.attack, cmd.targetShip.defense)
		if (roll1 > roll2)
			board.removeShip(cmd.targetShip)
		else
			board.removeShip(cmd.ship)
		end
	end
	
	def isLegal(board, cmd)
		case cmd.command
			when Command::MOVE
				return isMoveLegal(board, cmd)
			when Command::LAUNCH
				return isLaunchLegal(board, cmd)
			when Command::COLONIZE
				return isColonizeLegal(board, cmd)
			when Command::INVADE
				return isInvadeLegal(board, cmd)
			when Command::BUILD
				return isBuildLegal(board, cmd)	
			when Command::DEFENSE
				return isDefenseLegal(board, cmd)
			when Command::ATTACK
				return isAttackLegal(board, cmd)	
		end
		raise "Unknown Command.command"
	end
	
protected
	def findNewPlanetLocation(board, quad)
		startRow, startCol = quadStart(board, quad)
		row = 0
		col = 0
		tries = 0
		begin
			tries += 1
			if (tries > 100)
				raise "Unable to find a place for the planet"
			end
			row = startRow + rand(quadSize(board.size))
			col = startCol + rand(quadSize(board.size))
		end until canPlacePlanet(board, row, col)
		return row, col
	end
	
	def canPlacePlanet(board, row, col)
		size = board.size
		if (row == 0 || col == 0 ||
				row == mid(size) || col == mid(size) ||
				row == size-1 || col == size-1)
			return false
		end
		board.planets.each do
			| planet |
			if ( (planet.row-row).abs <= 1 && (planet.col-col).abs <= 1)
				return false
			end
		end
		return true
	end
	
	def quadStart(board, quad)
		size = board.size
		return (quad/2 * mid(size)) + 1, colStart = (quad.modulo(2) * mid(size)) + 1
	end

	def quadSize(size)
		return (size - 3) / 2
	end
	
	def canAddRace(board)
		if (board.races.size >= MAX_RACES)
			return false
		end
		return true
	end
	
	def isMoveLegal(board, cmd)
		if (cmd.ship.owner != cmd.race)
			return false
		end
		if (cmd.row < 0 || cmd.row >= board.size)
			return false
		end
		if (cmd.col < 0 || cmd.col >= board.size)
			return false
		end
		if (!shipOnBoard(board, cmd.ship))
			return false
		end
		board.ships.each do
			| ship |
			if (ship != cmd.ship && 
					(ship.row == cmd.row && ship.col == cmd.col))
				return false
			end
		end
		return true
	end
	
	def isLaunchLegal(board, cmd)
		if (cmd.planet.owner != cmd.race)
			return false
		end
		if (!planetOnBoard(board, cmd.planet))
			return false
		end
		if (!planetInBoard(board, cmd.planet))
			return false
		end
		if (!cmd.race.canLaunch)
			return false
		end
		board.ships.each do
			| ship |
			if (ship.row == cmd.planet.row && ship.col == cmd.planet.col)
				return false
			end
		end
		return true		
	end
	
	def isColonizeLegal(board, cmd)
		if (cmd.ship.owner != cmd.race)
			return false
		end
		if (cmd.planet.owner != nil)
			return false
		end
		if (!planetOnBoard(board, cmd.planet))
			return false
		end
		if (!planetInBoard(board, cmd.planet))
			return false
		end
		if (!shipOnBoard(board, cmd.ship))
			return false
		end
		if (!shipInBoard(board, cmd.ship))
			return false
		end
		if (cmd.ship.row != cmd.planet.row ||
				cmd.ship.col != cmd.planet.col)
			return false
		end
		return true
	end

	def isInvadeLegal(board, cmd)
		if (cmd.ship.owner != cmd.race)
			return false
		end
		if (cmd.planet.owner == nil)
			return false
		end
		if (cmd.planet.owner == cmd.race)
			return false
		end
		if (!planetOnBoard(board, cmd.planet))
			return false
		end
		if (!planetInBoard(board, cmd.planet))
			return false
		end
		if (!shipOnBoard(board, cmd.ship))
			return false
		end
		if (!shipInBoard(board, cmd.ship))
			return false
		end
		if (cmd.ship.row != cmd.planet.row ||
				cmd.ship.col != cmd.planet.col)
			return false
		end
		return true
	end
	
	def isBuildLegal(board, cmd)
		if (!Ship.isValidSize(cmd.shipSize))
			return false
		end
		if (cmd.race.newShipSize && (cmd.race.newShipSize != cmd.shipSize) )
			return false
		end
		if (cmd.race.newShipSize && (cmd.race.newShipLeft == 0) )
			return false
		end
		return true
	end
	
	def isDefenseLegal(board, cmd)
		if (!planetOnBoard(board, cmd.planet))
			return false
		end
		if (!planetInBoard(board, cmd.planet))
			return false
		end
		if (cmd.planet.owner != cmd.race)
			return false
		end
		if (cmd.planet.defense >= cmd.planet.maxDefense)
			return false
		end
		return true
	end
	
	def isAttackLegal(board, cmd)
		if (!shipOnBoard(board, cmd.ship))
			return false
		end
		if (!shipOnBoard(board, cmd.targetShip))
			return false
		end
		if (cmd.ship.owner != cmd.race)
			return false
		end
		if (cmd.targetShip.owner == cmd.race)
			return false
		end
		return true
	end

	def createHomePlanet(board, race)
		p = addPlanet(board, race.quad, Planet::HOME)
		p.owner = race
		p.defenseUpBy(p.maxDefense)
		return p
	end
	
	def nextQuad(numRaces)
		return [0, 3, 1, 2][numRaces]
	end

	def mid(size)
		return quadSize(size) + 1
	end

	def planetInBoard(board, planet)
		return itemInBounds(board, planet)
	end
	
	def planetOnBoard(board, planet)
		board.planets.each do
			| p |
			if (p == planet)
				return true
			end
		end
		return false
	end
	
	def shipInBoard(board, ship)
		return itemInBounds(board, ship)
	end
	
	def shipOnBoard(board, ship)
		board.ships.each do
			| p |
			if (p == ship)
				return true
			end
		end
		return false
	end

	def itemInBounds(board, item)
		return inBounds(board, item.row, item.col)
	end
	
	def inBounds(board, row, col)
		if (!row || !col)
			return false
		end
		if (row < 0 || row >= board.size)
			return false
		end
		if (col < 0 || col >= board.size)
			return false
		end
		return true
	end	
	
	def repelShip(board, ship)
		options = [ [ship.row+1, ship.col], [ship.row-1, ship.col],
					[ship.row, ship.col+1], [ship.row, ship.col-1] ]
		while (options.size > 0)
			attempt = rand(options.size)
			destRow = options[attempt][0]
			destCol = options[attempt][1]
			options.delete_at(attempt)
			if (inBounds(board, destRow, destCol) && 
						!isShipAt(board, destRow, destCol))
				ship.moveTo(destRow, destCol)
				return true
			end
		end
		return false
	end
	
	def isShipAt(board, row, col)
		board.ships.each do
			| ship |
			if (ship.row == row && ship.col == col)
				return true
			end
		end
		return false
	end
	
	def addPlanet(board, quad, size)
		row, col = findNewPlanetLocation(board, quad)
		p = Planet.new(size, row, col)
		board.addPlanet(p)
		return p
	end
	
end
