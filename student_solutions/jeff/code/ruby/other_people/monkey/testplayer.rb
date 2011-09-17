# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'runit/testcase'
require 'player'
require 'race'
require 'planet'

class TestPlayerSimple < RUNIT::TestCase
	def setup
		@p = PlayerSimple.new
	end
	
	def testClosestItemConditional
		list = [Planet.new(Planet::SMALL, 0,0), Planet.new(Planet::SMALL, 7,0), 
				Planet.new(Planet::SMALL, 0,3), Planet.new(Planet::SMALL, 4,0)]
		all = proc do | a, b | a != b end
		assert_equal(list[2], @p.closestItemConditional(list[0], list, &all))
		assert_equal(list[3], @p.closestItemConditional(list[1], list, &all))
		assert_equal(list[0], @p.closestItemConditional(list[2], list, &all))
		assert_equal(list[1], @p.closestItemConditional(list[3], list, &all))
	end

	def testDistance
		race = Race.new(0)
		s1 = Ship.create(Ship::CRUISER, race)
		s2 = Ship.create(Ship::CRUISER, race)
		
		s1.moveTo(9,9)
		s2.moveTo(0,0)
		assert_equal(18, @p.distance(s1, s2))
		s2.moveTo(0,18)
		assert_equal(18, @p.distance(s1, s2))
		s2.moveTo(18, 0)
		assert_equal(18, @p.distance(s1, s2))
		s2.moveTo(18,18)
		assert_equal(18, @p.distance(s1, s2))
		s2.moveTo(0, 8)
		assert_equal(10, @p.distance(s1, s2))
	end	
end

#--- main program ----
if __FILE__ == $0
  require 'runit/cui/testrunner'
  RUNIT::CUI::TestRunner.run(TestPlayerSimple.suite)
end

