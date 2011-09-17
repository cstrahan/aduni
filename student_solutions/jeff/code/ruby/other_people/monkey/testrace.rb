# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'runit/testcase'
require 'race'

class TestRace < RUNIT::TestCase
	def setup
	end

	def testInitialize
		p = Race.new(0)
		assert_equal(0, p.quad)
	end
	
	def testBuild
		sizes = {	Ship::CRUISER => 4,
					Ship::BATTLESTAR => 8,
					Ship::DREADNOUGHT => 10,
					Ship::MONITOR => 12 }
		sizes.each do
			| size, cost |
			race = Race.new(0)
			assert_equal(nil, race.newShipSize)
			(cost-1).times do
				| turn |
				race.build(size)
				assert_equal(size, race.newShipSize)
				assert_equal(cost-turn-1, race.newShipLeft)
			end
			race.build(size)
			assert_equal(size, race.newShipSize)
			assert_equal(0, race.newShipLeft)
		end
		
		race = Race.new(0)
		size = Ship::CRUISER
		assert_equal(nil, race.newShipSize)
		race.build(sizes[size])
		assert_equal(size, race.newShipSize)
		assert_equal(sizes[size]-1, race.newShipLeft)
		
		size2 = Ship::MONITOR
		race.build(sizes[size2])
		assert_equal(size, race.newShipSize)
		assert_equal(sizes[size]-1, race.newShipLeft)
	end
	
	def testLaunch
		race = Race.new(0)
		size = Ship::CRUISER
		assert_equal(nil, race.launch)
		4.times do
			assert_equal(nil, race.launch)
			race.build(size)
		end
		assert_equal(0, race.newShipLeft)
		ship = race.launch
		assert(ship)
		assert_equal(size, ship.size)
		assert_equal(race, ship.owner)
		assert_equal(nil, race.launch)
	end
	
end

#--- main program ----
if __FILE__ == $0
  require 'runit/cui/testrunner'
  RUNIT::CUI::TestRunner.run(TestRace.suite)
end

