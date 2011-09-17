# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'runit/testcase'
require 'board'

class TestBoard < RUNIT::TestCase
	def setup
		srand(0)
	end

	def testInitialize
		b = Board.new(19)
		assert_equal(19, b.size)
		b = Board.new(17)
		assert_equal(17, b.size)
	end

	def testClear
		b = newBoard
		race = Race.new(0)
		b.addRace(race)
		
		planet = Planet.new(Planet::HOME, 3, 4)
		b.addPlanet(planet)

		ship = Ship.create(Ship::CRUISER, race)
		ship.moveTo(5,6)
		b.addShip(ship)

		assert_equal(1, b.races.size)
		assert_equal(1, countPlanets(b))
		assert_equal(1, countShips(b))

		b.clear
		assert_equal(0, b.races.size)
		assert_equal(0, countPlanets(b))
		assert_equal(0, countShips(b))
	end
	
	def testRemoveShip
		b = newBoard
		race = Race.new(0)
		planet = Planet.new(Planet::HOME, 3,4)
		ship = Ship.create(Ship::CRUISER, race)
		ship.moveTo(5,6)
		b.addRace(race)
		b.addPlanet(planet)
		b.addShip(ship)
		assert_equal(1, countShips(b))
		b.removeShip(nil)
		assert_equal(1, countShips(b))
		ship2 = Ship.create(Ship::CRUISER, race)
		b.removeShip(ship2)
		assert_equal(1, countShips(b))
		b.removeShip(ship)
		assert_equal(0, countShips(b))
	end
	
private
	def newBoard
		return Board.new(19)
	end

	def countPlanets(board)
		return board.planets.size
	end

	def countShips(board)
		return board.ships.size
	end

end

#--- main program ----
if __FILE__ == $0
  require 'runit/cui/testrunner'
  RUNIT::CUI::TestRunner.run(TestBoard.suite)
end

=begin
a = 'this is a test'
b = 'nope'
c = [a, b]
d = c.clone
p "#{c.inspect}, #{d.inspect}"
d[0].chop!
p "#{c.inspect}, #{d.inspect}"
c[1].chop!
p "#{c.inspect}, #{d.inspect}"
d[0] = b
p "#{c.inspect}, #{d.inspect}"
=end
