# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'runit/testcase'
require 'planet'

class TestPlanet < RUNIT::TestCase
	def setup
	end

	def testInitialize
		p = Planet.new(Planet::HOME, 1,2)
		p = Planet.new(Planet::LARGE, 3,4)
		p = Planet.new(Planet::MEDIUM, 5,6)
		p = Planet.new(Planet::SMALL, 7,8)
		assert_equal(Planet::UNKNOWN, p.size)
		p = Planet.new(Planet::HOME, 2,3)
		assert_equal(2, p.row)
		assert_equal(3, p.col)
		p = Planet.new(Planet::LARGE, 2,3)
		assert_equal(2, p.row)
		assert_equal(3, p.col)
		p = Planet.new(Planet::MEDIUM, 2,3)
		assert_equal(2, p.row)
		assert_equal(3, p.col)
		p = Planet.new(Planet::SMALL, 2,3)
		assert_equal(2, p.row)
		assert_equal(3, p.col)
	end
	
	def testSize
		p = Planet.new(Planet::HOME, 1,1)
		assert_equal(Planet::UNKNOWN, p.size)
		p.owner = 1
		assert_equal(Planet::HOME, p.size)

		p = Planet.new(Planet::LARGE, 1,1)
		p.owner = 1
		assert_equal(Planet::LARGE, p.size)

		p = Planet.new(Planet::MEDIUM, 1,1)
		p.owner = 1
		assert_equal(Planet::MEDIUM, p.size)

		p = Planet.new(Planet::SMALL, 1,1)
		p.owner = 1
		assert_equal(Planet::SMALL, p.size)
	end
	
	def testOwner
		p = Planet.new(Planet::HOME, 1,1)
		assert(!p.owner)
		p.owner = 1
	end
	
end

#--- main program ----
if __FILE__ == $0
  require 'runit/cui/testrunner'
  RUNIT::CUI::TestRunner.run(TestPlanet.suite)
end

