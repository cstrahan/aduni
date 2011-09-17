# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'runit/testcase'
require 'rules'

class RulesForTesting < Rules

	public :findNewPlanetLocation
	public :canPlacePlanet
	public :quadStart
	public :quadSize
	public :addPlanet
end

class TestRules < RUNIT::TestCase
	def setup
		srand(0)
		@r = RulesForTesting.new
		@b = Board.new(@r.size)
	end

	def testCreateGenericPlanets
		@b.clear
		@r.createGenericPlanets(@b)
		assert_equal(4*5, countPlanets(@b))

		all = (0..18)
		[0, @b.size/2, @b.size].each do
			| rowCol |
			assert_equal(0, countSomePlanets(@b, [rowCol..rowCol], all))			
			assert_equal(0, countSomePlanets(@b, all, [rowCol..rowCol]))			
		end
		
		4.times do
			| quad |
			rows = quadRows(@b, quad)
			cols = quadCols(@b, quad)
			assert_equal(5, countSomePlanets(@b, rows, cols))
		end
		
		@b.planets.each do
			| planet |
			planet.owner = 1
		end
		
		large = medium = small = 0
		@b.planets.each do
			| planet |
			case planet.size
				when Planet::HOME
					assert_fail('Should be no HOME planets')
				when Planet::LARGE
					large += 1
				when Planet::MEDIUM
					medium += 1
				when Planet::SMALL
					small += 1
				else
					assert_fail('Unknown planet size')
			end
		end
		assert_equal(4, large)
		assert_equal(8, medium)
		assert_equal(8, small)
	end

	def testCreateRace
		@b.clear
		race = @r.createRace(@b)
		assert(race)
		assert_equal(0, race.quad)
		foundHome = @b.planets[0]
		assert_equal(Planet::HOME, foundHome.size)
		assert(foundHome.row >= 1)
		assert(foundHome.col >= 1)
		assert(foundHome.row <= 8)
		assert(foundHome.col <= 8)
		assert_equal(foundHome.maxDefense, foundHome.defense)

		@b.clear		
		4.times do
			assert(@r.createRace(@b))
		end
		assert_equal(4, countPlanets(@b))
		assert_equal(0, countShips(@b))
		
		assert(!@r.createRace(@b))
	end
	
	def testHasLost
		@b.clear
		race = @r.createRace(@b)
		assert_equal(false, @r.hasLost(@b, race))
		@b.planets(race)[0].owner = nil
		assert_equal(true, @r.hasLost(@b, race))
		ship = Ship.create(Ship::CRUISER, race)
		@b.addShip(ship)
		assert_equal(false, @r.hasLost(@b, race))
		
	end
	
	def testHasWon
		@b.clear
		race = @r.createRace(@b)
		26.times do
			assert_equal(false, @r.hasWon(@b, race))
			planet = Planet.new(Planet::SMALL, 1,1)
			planet.owner = race
			@b.addPlanet(planet)
		end
		assert_equal(true, @r.hasWon(@b, race))

		@b.planets(race)[0].owner = nil
		assert_equal(false, @r.hasWon(@b, race))
		@b.planets(race)[0].owner = nil
		assert_equal(false, @r.hasWon(@b, race))
		2.times do
			assert_equal(false, @r.hasWon(@b, race))
			planet = Planet.new(Planet::LARGE, 1,1)
			planet.owner = race
			@b.addPlanet(planet)
		end
		assert_equal(true, @r.hasWon(@b, race))

		4.times do
			@b.planets(race)[0].owner = nil
			assert_equal(false, @r.hasWon(@b, race))
		end
		2.times do
			assert_equal(false, @r.hasWon(@b, race))
			planet = Planet.new(Planet::MEDIUM, 1,1)
			planet.owner = race
			@b.addPlanet(planet)
		end
		assert_equal(true, @r.hasWon(@b, race))
		@b.planets(race)[0].owner = nil
		assert_equal(false, @r.hasWon(@b, race))
	end
	
	def testVictoryPoints
		@b.clear
		race = @r.createRace(@b)
		assert_equal(5, @r.victoryPoints(@b, race))
		2.times do
			planet = Planet.new(Planet::SMALL, 1,1)
			planet.owner = race
			@b.addPlanet(planet)
		end
		assert_equal(5 + 2*1, @r.victoryPoints(@b, race))

		@b.clear
		race = @r.createRace(@b)
		assert_equal(5, @r.victoryPoints(@b, race))
		2.times do
			planet = Planet.new(Planet::MEDIUM, 1,1)
			planet.owner = race
			@b.addPlanet(planet)
		end
		assert_equal(5 + 2*2, @r.victoryPoints(@b, race))
		
		@b.clear
		race = @r.createRace(@b)
		assert_equal(5, @r.victoryPoints(@b, race))
		2.times do
			planet = Planet.new(Planet::LARGE, 1,1)
			planet.owner = race
			@b.addPlanet(planet)
		end
		assert_equal(5 + 2*3, @r.victoryPoints(@b, race))

		@b.clear
		race = @r.createRace(@b)
		assert_equal(5, @r.victoryPoints(@b, race))
		2.times do
			planet = Planet.new(Planet::HOME, 1,1)
			planet.owner = race
			@b.addPlanet(planet)
		end
		assert_equal(5 + 2*5, @r.victoryPoints(@b, race))
	end
	
	def testAttemptInvade
		7.times do
			| roll |
			@b.clear
			race = @r.createRace(@b)
			planet = Planet.new(Planet::SMALL, 9,9)
			@b.addPlanet(planet)
			ship = Ship.create(Ship::CRUISER, race)
			ship.moveTo(9,9)
			@b.addShip(ship)
			cmd = Command.invade(race, ship, planet)
			@r.attemptInvade(@b, cmd) do 
				| die |
				assert_equal(4, die)
				roll
			end
			case roll + 1
				when 1..2
					assert_equal(nil, planet.owner)
					assert_equal(0, @b.ships.size)
				when 3..5
					assert_equal(nil, planet.owner)
					assert_equal([ship], @b.ships)
					assert_equal(1, (9-ship.row).abs + (9-ship.col).abs)
				when 6
					assert_equal(nil, planet.owner)
					assert_equal([ship], @b.ships)
					assert_equal(9, ship.row)
					assert_equal(9, ship.col)
				when 7
					assert_equal(race, planet.owner)
					assert_equal([ship], @b.ships)
					assert_equal(9, ship.row)
					assert_equal(9, ship.col)
				else
					assert_fail("Unexpected roll")
			end
		end

		@b.clear
		race = @r.createRace(@b)
		planet = Planet.new(Planet::SMALL, 9,9)
		@b.addPlanet(planet)
		ship = Ship.create(Ship::CRUISER, race)
		ship.moveTo(9,9)
		@b.addShip(ship)
		
		ship1 = Ship.create(Ship::CRUISER, race)
		ship1.moveTo(10,9)
		@b.addShip(ship1)
		ship2 = Ship.create(Ship::CRUISER, race)
		ship2.moveTo(8,9)
		@b.addShip(ship2)
		ship3 = Ship.create(Ship::CRUISER, race)
		ship3.moveTo(9,10)
		@b.addShip(ship3)
		ship4 = Ship.create(Ship::CRUISER, race)
		ship4.moveTo(9,8)
		@b.addShip(ship4)

		cmd = Command.invade(race, ship, planet)
		@r.attemptInvade(@b, cmd) do 3 end
		assert_equal(nil, planet.owner)
		assert_equal(nil, @b.ships.index(ship))
		
		planet = Planet.new(Planet::SMALL, 0,0)
		@b.addPlanet(planet)
		ship1.moveTo(1,0)
		ship2.moveTo(0,1)
		ship = Ship.create(Ship::CRUISER, race)
		ship.moveTo(0,0)
		@b.addShip(ship)
		cmd = Command.invade(race, ship, planet)
		@r.attemptInvade(@b, cmd) do 4 end
		assert_equal(nil, planet.owner)
		assert_equal(nil, @b.ships.index(ship))
	end
	
	def testAttemptAttack
		@b.clear
		race1 = @r.createRace(@b)
		race2 = @r.createRace(@b)
		win = [2,1]
		tie = [2,2]
		lose = [1,2]
		[win, lose, tie].each do
			| outcome |
			ship1 = Ship.create(Ship::MONITOR, race1)
			ship2 = Ship.create(Ship::CRUISER, race2)
			ship1.moveTo(0,0)
			ship2.moveTo(1,0)
			@b.addShip(ship1)
			@b.addShip(ship2)

			cmd = Command.attack(race1, ship1, ship2)
			@r.attemptAttack(@b, cmd) do
				| die1, die2 |
				assert_equal(12, die1)
				assert_equal(4, die2)
				outcome
			end
			case outcome
				when win
					assert_equal(1, @b.ships(race1).size)
					assert_equal(0, @b.ships(race2).size)
				when tie, lose
					assert_equal(0, @b.ships(race1).size)
					assert_equal(1, @b.ships(race2).size)
			end

			@b.removeShip(ship1)
			@b.removeShip(ship2)
		end
	end
	
	def testIsLegal
		# covered by testIsXxxLegal
	end

	def testIsMoveLegal
		@b.clear
		race = @r.createRace(@b)
		ship = Ship.create(Ship::CRUISER, race)
		ship.moveTo(0,0)
		@b.addShip(ship)
		cmd = Command.move(race, ship, -1, 12)
		assert_equal(false, @r.isLegal(@b, cmd))
		cmd = Command.move(race, ship, 5, -1)
		assert_equal(false, @r.isLegal(@b, cmd))
		cmd = Command.move(race, ship, 9, 19)
		assert_equal(false, @r.isLegal(@b, cmd))
		cmd = Command.move(race, ship, 19, 3)
		assert_equal(false, @r.isLegal(@b, cmd))

		cmd = Command.move(race, ship, 0, 0)
		assert_equal(true, @r.isLegal(@b, cmd))
		cmd = Command.move(race, ship, 0, 17)
		assert_equal(true, @r.isLegal(@b, cmd))
		cmd = Command.move(race, ship, 17, 0)
		assert_equal(true, @r.isLegal(@b, cmd))
		cmd = Command.move(race, ship, 17, 17)
		assert_equal(true, @r.isLegal(@b, cmd))
		
		race2 = @r.createRace(@b)
		ship2 = Ship.create(Ship::CRUISER, race)
		ship2.moveTo(0,0)
		@b.addShip(ship2)
		cmd = Command.move(race, ship, ship2.row, ship2.col)
		assert_equal(false, @r.isLegal(@b, cmd))
		cmd = Command.move(race2, ship, ship2.row, ship2.col)
		assert_equal(false, @r.isLegal(@b, cmd))
		
	end	
	
	def testIsLaunchLegal
		@b.clear
		race = @r.createRace(@b)
		planet = @b.planets[0]
		cmd = Command.launch(race, planet)
		assert_equal(false, @r.isLegal(@b, cmd))

		4.times do
			race.build(Ship::CRUISER)
		end	
		assert_equal(0, race.newShipLeft)
		assert_equal(true, race.canLaunch)	

		badPlanet = Planet.new(Planet::HOME, 3,3)
		badPlanet.owner = race
		cmd = Command.launch(race, badPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))

		badPlanet = Planet.new(Planet::HOME, 3,3)
		@b.addPlanet(badPlanet)
		cmd = Command.launch(race, badPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))

		cmd = Command.launch(race, planet)
		assert_equal(true, @r.isLegal(@b, cmd))

		ship = race.launch
		ship.moveTo(planet.row, planet.col)
		@b.addShip(ship)
		cmd = Command.launch(race, planet)
		assert_equal(false, @r.isLegal(@b, cmd))

		4.times do
			race.build(Ship::CRUISER)
		end	
		assert_equal(0, race.newShipLeft)
		assert_equal(true, race.canLaunch)
		assert_equal(planet.row, ship.row)
		assert_equal(planet.col, ship.col)
		assert_equal(1, @b.ships.size)

		cmd = Command.launch(race, planet)
		assert_equal(false, @r.isLegal(@b, cmd))
		planet2 = @b.planets[1]
		planet2.owner = race
		cmd = Command.launch(race, planet2)
		assert_equal(true, @r.isLegal(@b, cmd))
		
	end
	
	def testIsColonizeLegal
		@b.clear
		race = @r.createRace(@b)
		goodShip = Ship.create(Ship::CRUISER, race)
		goodShip.moveTo(5,5)
		@b.addShip(goodShip)
		goodPlanet = Planet.new(Planet::SMALL, 5,5)
		@b.addPlanet(goodPlanet)
		
		badShip = Ship.create(Ship::CRUISER, 1)
		cmd = Command.colonize(race, badShip, goodPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))
		
		badShip = Ship.create(Ship::CRUISER, race)
		badShip.moveTo(5,5)
		cmd = Command.colonize(race, badShip, goodPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))
		
		ownedPlanet = Planet.new(Planet::SMALL, 8,8)
		ownedPlanet.owner = race
		@b.addPlanet(ownedPlanet)
		cmd = Command.colonize(race, goodShip, ownedPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))

		badPlanet = Planet.new(Planet::HOME, 4,4)
		cmd = Command.colonize(race, goodShip, badPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))
	end

	def testIsInvadeLegal
		@b.clear
		race = @r.createRace(@b)
		
		raceOther = @r.createRace(@b)
		ownedPlanet = Planet.new(Planet::SMALL, 8,8)
		ownedPlanet.owner = raceOther
		@b.addPlanet(ownedPlanet)
		
		badShip = Ship.create(Ship::CRUISER, 1)
		cmd = Command.invade(race, badShip, ownedPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))
		
		badShip = Ship.create(Ship::CRUISER, race)
		badShip.moveTo(8,8)
		cmd = Command.invade(race, badShip, ownedPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))
				
		goodShip = Ship.create(Ship::CRUISER, race)
		goodShip.moveTo(5,5)
		@b.addShip(goodShip)
		
		badPlanet = Planet.new(Planet::SMALL, 5,5)
		cmd = Command.invade(race, goodShip, badPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))
		
		badPlanet.owner = raceOther
		cmd = Command.invade(race, goodShip, badPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))

		@b.addPlanet(badPlanet)
		badPlanet.owner = nil
		cmd = Command.invade(race, goodShip, badPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))

		cmd = Command.invade(race, goodShip, ownedPlanet)
		assert_equal(false, @r.isLegal(@b, cmd))

		goodShip.moveTo(8,8)
		cmd = Command.invade(race, goodShip, ownedPlanet)
		assert_equal(true, @r.isLegal(@b, cmd))
	end
	
	def testIsBuildLegal
		@b.clear
		race = @r.createRace(@b)
		size = Ship::DREADNOUGHT
		size2 = Ship::BATTLESTAR
		cmd = Command.build(race, -1)
		assert_equal(false, @r.isLegal(@b, cmd))
		cmd = Command.build(race, size)
		assert_equal(true, @r.isLegal(@b, cmd))
		race.build(size)
		assert_equal(size, race.newShipSize)
		assert(race.newShipLeft > 0)
		cmd = Command.build(race, size2)
		assert_equal(false, @r.isLegal(@b, cmd))
		cmd = Command.build(race, size)
		assert_equal(true, @r.isLegal(@b, cmd))
		
		while (race.newShipLeft > 0)
			cmd = Command.build(race, size)
			assert_equal(true, @r.isLegal(@b, cmd))
			race.build(size)
		end
		assert_equal(false, @r.isLegal(@b, cmd))
			
	end

	def testIsDefenseLegal
		@b.clear
		race = @r.createRace(@b)
		planet = @b.planets(race)[0]
		cmd = Command.defense(race, planet)
		assert_equal(false, @r.isLegal(@b, cmd))

		planet = Planet.new(Planet::SMALL, 9,9)
		cmd = Command.defense(race, planet)
		assert_equal(false, @r.isLegal(@b, cmd))

		planet.owner = race		
		cmd = Command.defense(race, planet)
		assert_equal(false, @r.isLegal(@b, cmd))

		@b.addPlanet(planet)
		cmd = Command.defense(race, planet)
		assert_equal(true, @r.isLegal(@b, cmd))
	end
	
	def testIsAttackLegal
		@b.clear
		race1 = @r.createRace(@b)
		race2 = @r.createRace(@b)
		ship1 = Ship.create(Ship::MONITOR, race1)
		ship2 = Ship.create(Ship::CRUISER, race2)
		ship1.moveTo(0,0)
		ship2.moveTo(1,0)

		cmd = Command.attack(race1, ship1, ship2)
		assert_equal(false, @r.isLegal(@b, cmd))
		cmd = Command.attack(race2, ship2, ship1)
		assert_equal(false, @r.isLegal(@b, cmd))

		@b.addShip(ship1)
		cmd = Command.attack(race1, ship1, ship2)
		assert_equal(false, @r.isLegal(@b, cmd))
		cmd = Command.attack(race2, ship2, ship1)
		assert_equal(false, @r.isLegal(@b, cmd))
		
		@b.removeShip(ship1)
		@b.addShip(ship2)
		cmd = Command.attack(race1, ship1, ship2)
		assert_equal(false, @r.isLegal(@b, cmd))
		cmd = Command.attack(race2, ship2, ship1)
		assert_equal(false, @r.isLegal(@b, cmd))
		
		@b.addShip(ship1)
		cmd = Command.attack(race1, ship2, ship1)
		assert_equal(false, @r.isLegal(@b, cmd))
		cmd = Command.attack(race2, ship1, ship2)
		assert_equal(false, @r.isLegal(@b, cmd))
		
		cmd = Command.attack(race1, ship1, ship1)
		assert_equal(false, @r.isLegal(@b, cmd))
		cmd = Command.attack(race2, ship2, ship2)
		assert_equal(false, @r.isLegal(@b, cmd))

		cmd = Command.attack(race1, ship1, ship2)
		assert_equal(true, @r.isLegal(@b, cmd))
	end
	
	def testFindNewPlanetLocation
		@b.clear
		4.times do
			| quad |
			100.times do
				row, col = @r.findNewPlanetLocation(@b, quad)
				rows = quadRows(@b, quad)
				cols = quadCols(@b, quad)
				assert_in_range(rows, row)
				assert_in_range(cols, col)
			end
		end
		
		8.times do
			| rowCol |
			p = Planet.new(Planet::SMALL, rowCol + 1, rowCol + 1)
			@b.addPlanet(p)
		end
		assert_equal(8, countPlanets(@b))
		100.times do
			row, col = @r.findNewPlanetLocation(@b, 0)
			rows = quadRows(@b, 0)
			cols = quadCols(@b, 0)
			assert_in_range(rows, row)
			assert_in_range(cols, col)
			assert(row != col)
		end
		
		@b.clear
		8.times do
			| row |
			8.times do
				| col |
				p = Planet.new(Planet::SMALL, row + 1, col + 1)
				@b.addPlanet(p)
			end
		end
		begin
			row, col = @r.findNewPlanetLocation(@b, 0)
		rescue
			# expected
		else
			assert_fail("Should not have found a place")
		end			
	end

	def testCanPlacePlanet
		@b.clear
		assert_equal(0, countPlanets(@b))
		planet = Planet.new(Planet::SMALL, 2,5)
		@b.addPlanet(planet)
		row = planet.row
		col = planet.col
		(row-1..row+1).each do
			| r |
			(col-1..col+1).each do
				| c |
				assert(!@r.canPlacePlanet(@b, r, c))
			end
		end
		mid = @r.quadStart(@b, 3)[0] - 1
		eachPosition(@b) do
			| r, c |
			if ((row-r).abs <= 1 && (col-c).abs <= 1)
				assert(!@r.canPlacePlanet(@b, r,c))
			elsif (r == 0 || c == 0 || 
					r == @b.size-1 || c == @b.size-1 || 
					r == mid || c == mid)
				assert(!@r.canPlacePlanet(@b, r,c))
			else
				assert(@r.canPlacePlanet(@b, r,c))
			end
		end
	end
	
	def testQuadStart
		row, col = @r.quadStart(@b, 0)
		assert_equal(1, row)
		assert_equal(1, col)
		row, col = @r.quadStart(@b, 1)
		assert_equal(1, row)
		assert_equal(10, col)
		row, col = @r.quadStart(@b, 2)
		assert_equal(10, row)
		assert_equal(1, col)
		row, col = @r.quadStart(@b, 3)
		assert_equal(10, row)
		assert_equal(10, col)
	end
	
	def testAddPlanet
		@b.clear
		quad = 3
		planet = @r.addPlanet(@b, quad, Planet::HOME)
		assert_equal(1, @b.planets.size)
		assert_equal(nil, planet.owner)
		race = @r.createRace(@b)
		planet.owner = race
		assert_equal(Planet::HOME, planet.size)
		rows = quadRows(@b, quad)
		cols = quadCols(@b, quad)
		assert_in_range(rows, planet.row)
		assert_in_range(cols, planet.col)
	end

	
private
	def ensureNoStackedPlanets(board)
		board.planets.each do
			| p1 |
			board.planets.each do
				| p2 |
				if (p1 != p2)
					assert(p1.row != p2.row || p1.col != p2.col)
				end
			end
		end
	end

	def quadRows(board, quad)
		rowStart, colStart = @r.quadStart(@b, quad)
		rows = (rowStart...rowStart+@r.quadSize(@b.size))
		return rows
	end

	def quadCols(board, quad)
		rowStart, colStart = @r.quadStart(@b, quad)
		cols = (colStart...colStart+@r.quadSize(@b.size))
		return cols
	end
			
	def countSomePlanets(board, rows, cols)
		count = 0
		board.planets.each do
			| planet |
			if (rows === planet.row && cols === planet.col)
				count += 1
			end
		end
		return count
	end

	def eachPosition(board)
		board.size.times do
			| row |
			board.size.times do
				| col |
				yield(row, col)
			end
		end
	end

	def countPlanets(board)
		return board.planets.size
	end

	def countShips(board)
		return board.ships.size
	end

	def assert_in_range(range, element)
		if !(range === element)
			assert_fail
		end
	end

end

#--- main program ----
if __FILE__ == $0
  require 'runit/cui/testrunner'
  RUNIT::CUI::TestRunner.run(TestRules.suite)
end

