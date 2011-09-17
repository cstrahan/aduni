# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'runit/testcase'
require 'ship'
require 'race'

class TestShip < RUNIT::TestCase
	def setup
		@race = Race.new(0)

	end

	def testInitialize
		begin
			Ship.new
		rescue ScriptError
			# expected
		else
			Assert_fail("Should not be able to call Ship.new")
		end
		begin
			Ship.create(Ship::CRUISER)
		rescue
			# expected
		else
			Assert_fail("Cannot create a ship without an owner")
		end
		ship = Ship.create(Ship::CRUISER, @race)
		assert(!ship.row)
		assert(!ship.col)
		assert(ship.owner == @race)
	end
	
	def testMoveTo
		ship = Ship.create(Ship::CRUISER, @race)
		ship.moveTo(1,2)
		assert_equal(1, ship.row)
		assert_equal(2, ship.col)
	end
	
end

#--- main program ----
if __FILE__ == $0
  require 'runit/cui/testrunner'
  RUNIT::CUI::TestRunner.run(TestShip.suite)
end

