# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'runit/testcase'
require 'command'
require 'ship'
require 'planet'

class TestCommand < RUNIT::TestCase
	def setup
		@owner = 1
		@race = 1
	end

	def testMove
		race = @race
		ship = Ship.create(Ship::CRUISER, @owner)
		cmd = Command.move(race, ship, 1, 2)
		assert_equal(race, cmd.race)
		assert_equal(Command::MOVE, cmd.command)
		assert_equal(ship, cmd.ship)
		assert_equal(1, cmd.row)
		assert_equal(2, cmd.col)
		assert_equal(nil, cmd.planet)
		assert_equal(nil, cmd.shipSize)
		assert_equal(nil, cmd.targetShip)
	end

	def testLaunch
		race = @race
		planet = Planet.new(Planet::HOME, 1,1)
		planet.owner = race
		cmd = Command.launch(race, planet)
		assert_equal(race, cmd.race)
		assert_equal(Command::LAUNCH, cmd.command)
		assert_equal(planet, cmd.planet)
		assert_equal(nil, cmd.ship)
		assert_equal(nil, cmd.row)
		assert_equal(nil, cmd.col)
		assert_equal(nil, cmd.shipSize)
		assert_equal(nil, cmd.targetShip)
	end
	
	def testColonize
		race = @race
		ship = Ship.create(Ship::CRUISER, @owner)
		planet = Planet.new(Planet::SMALL, 1,1)
		planet.owner = race
		cmd = Command.colonize(race, ship, planet)
		assert_equal(race, cmd.race)
		assert_equal(Command::COLONIZE, cmd.command)
		assert_equal(planet, cmd.planet)
		assert_equal(ship, cmd.ship)
		assert_equal(nil, cmd.row)
		assert_equal(nil, cmd.col)
		assert_equal(nil, cmd.shipSize)
		assert_equal(nil, cmd.targetShip)
		
	end

	def testInvade
		race = @race
		ship = Ship.create(Ship::CRUISER, @owner)
		planet = Planet.new(Planet::SMALL, 1,1)
		cmd = Command.invade(race, ship, planet)
		assert_equal(race, cmd.race)
		assert_equal(Command::INVADE, cmd.command)
		assert_equal(planet, cmd.planet)
		assert_equal(ship, cmd.ship)
		assert_equal(nil, cmd.row)
		assert_equal(nil, cmd.col)
		assert_equal(nil, cmd.shipSize)
		assert_equal(nil, cmd.targetShip)
		
	end

	def testBuild
		race = @race
		shipSize = Ship::BATTLESTAR
		cmd = Command.build(race, shipSize)
		assert_equal(race, cmd.race)
		assert_equal(Command::BUILD, cmd.command)
		assert_equal(nil, cmd.planet)
		assert_equal(nil, cmd.ship)
		assert_equal(nil, cmd.row)
		assert_equal(nil, cmd.col)
		assert_equal(shipSize, cmd.shipSize)
		assert_equal(nil, cmd.targetShip)
		
	end

	def testDefense
		race = @race
		planet = Planet.new(Planet::SMALL, 1,1)
		cmd = Command.defense(race, planet)
		assert_equal(race, cmd.race)
		assert_equal(Command::DEFENSE, cmd.command)
		assert_equal(planet, cmd.planet)
		assert_equal(nil, cmd.ship)
		assert_equal(nil, cmd.row)
		assert_equal(nil, cmd.col)
		assert_equal(nil, cmd.shipSize)
		assert_equal(nil, cmd.targetShip)
		
	end
	
	def testAttack
		race = @race
		ship1 = Ship.create(Ship::MONITOR, race)
		ship2 = Ship.create(Ship::CRUISER, race)
		cmd = Command.attack(race, ship1, ship2)
		assert_equal(race, cmd.race)
		assert_equal(Command::ATTACK, cmd.command)
		assert_equal(nil, cmd.planet)
		assert_equal(ship1, cmd.ship)
		assert_equal(nil, cmd.row)
		assert_equal(nil, cmd.col)
		assert_equal(nil, cmd.shipSize)
		assert_equal(ship2, cmd.targetShip)
		
	end
end

#--- main program ----
if __FILE__ == $0
  require 'runit/cui/testrunner'
  RUNIT::CUI::TestRunner.run(TestCommand.suite)
end

