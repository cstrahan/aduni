# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'runit/testcase'
require 'game'

class MockPlayer
	attr_reader :game, :race
	attr_writer :refuseToJoin
	
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
end

class TestGame < RUNIT::TestCase
	def setup
		srand(0)
	end
	
	def testClear
		game = Game.new
		p1 = MockPlayer.new
		assert_equal(true, game.addPlayer(p1))
		race = p1.race
		assert_equal(1, game.planets(race).size)
		game.planets.each do
			| planet |
			planet.owner = race
		end
		assert_equal(21, game.planets.size)
		assert_equal(21, game.planets(race).size)
		game.clear
		assert_equal(0, game.players.size)
		assert_equal(nil, p1.game)
		assert_equal(nil, p1.race)
		assert_equal(0, game.planets(race).size)
	end

	def testAddPlayer
		game = Game.new
		p1 = MockPlayer.new
		assert_equal(true, game.addPlayer(p1))
		assert_equal(game, p1.game)
		assert_equal(false, game.addPlayer(p1))

		p2 = MockPlayer.new
		assert_equal(true, game.addPlayer(p2))
	end

	def testWhoseTurnNextTurn
		game = Game.new
		assert_equal(nil, game.whoseTurn?)

		badPlayer = MockPlayer.new
		badPlayer.refuseToJoin = true
		assert_equal(false, game.addPlayer(badPlayer))

		players = []
		4.times do
			| t |
			players[t] = MockPlayer.new
			assert_equal(true, game.addPlayer(players[t]))
		end
		assert_equal(players[0], game.whoseTurn?)
		assert_equal(players[0], game.whoseTurn?)
		
		badPlayer.refuseToJoin = false
		assert_equal(false, game.addPlayer(badPlayer))
		
		assert_equal(4, game.players.size)
		
		3.times do
			| t |
			assert_equal(players[t], game.whoseTurn?)
			assert_equal(nil, game.nextTurn)
			assert_equal(players[t+1], game.whoseTurn?)
		end
		assert_equal(nil, game.nextTurn)
		assert_equal(players[0], game.whoseTurn?)
	end
	
end

#--- main program ----
if __FILE__ == $0
  require 'runit/cui/testrunner'
  RUNIT::CUI::TestRunner.run(TestGame.suite)
end

