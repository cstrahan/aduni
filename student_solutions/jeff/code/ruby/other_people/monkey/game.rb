# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'observer'
require 'rules'

class Game
	include Observable
	
	attr_reader :players, :turn

	def initialize
		@whoseTurn = 0
		@players = []
		
		@rules = Rules.new
		@board = Board.new(@rules.size)
		clear
	end
	
	def clear
		@players.each do
			| player |
			player.leaveGame(self)
		end
		@players = []
		@board.clear
		@rules.createGenericPlanets(@board)
		@turn = 0
		return nil
	end
	

	#########################################
	def addPlayer(player)
		race = @rules.createRace(@board)
		if (!race)
			return false
		end
		if (!player.joinGame(self, race))
			@rules.destroyRace(@board, race)
			return false
		end
		@players << player
		changed(true)
		notify_observers
		return true
	end
	
	def removePlayer(player)
		@players.delete(player)
		player.leaveGame(self)
	end

	def whoseTurn?
		if (@whoseTurn >= @players.size)
			@whoseTurn = 0
		end
		return @players[@whoseTurn]
	end
	
	def nextTurn
		@turn += 1
		@whoseTurn += 1
		if (@whoseTurn >= @players.size)
			@whoseTurn = 0
		end
		return nil
	end
	
	def hasLost(player)
		return @rules.hasLost(@board, player.race)
	end
	
	def hasWon(player)
		return @rules.hasWon(@board, player.race)
	end
	
	def victoryPoints(player)
		return @rules.victoryPoints(@board, player.race)
	end
	
	#########################################
	def isLegal(cmd)
		if (!cmd)
			return false
		end
		return @rules.isLegal(@board, cmd)
	end
	
	def doCommand(cmd)
		worked = false
		if (!cmd)
			puts "Nil command from #{whoseTurn.inspect}"
		elsif (isLegal(cmd))
			case cmd.command
				when Command::MOVE
					doMove(cmd)
				when Command::LAUNCH
					doLaunch(cmd)
				when Command::COLONIZE
					doColonize(cmd)
				when Command::INVADE
					doInvade(cmd)
				when Command::BUILD
					doBuild(cmd)
				when Command::DEFENSE
					doDefense(cmd)
				when Command::ATTACK
					doAttack(cmd)
				else
					raise "Unknown command"
			end
			worked = true
		else
			puts "Attempted illegal #{cmd.inspect}"
		end
		nextTurn
		changed(true)
		notify_observers
		return worked
	end
	
	#########################################
	def boardSize
		return @board.size
	end
	
	def races
		return @board.races
	end
	
	def planets(race = nil)
		return @board.planets(race)
	end
	
	def ships(race = nil)
		return @board.ships(race)
	end

protected	
	def doMove(cmd)
		cmd.ship.moveTo(cmd.row, cmd.col)
		return nil
	end
	
	def doLaunch(cmd)
		ship = cmd.race.launch
		ship.moveTo(cmd.planet.row, cmd.planet.col)
		@board.addShip(ship)
		return nil
	end
	
	def doColonize(cmd)
		cmd.planet.owner = cmd.race
		return nil
	end
	
	def doInvade(cmd)
		@rules.attemptInvade(@board, cmd) do
			|max|
			rand(max)
		end
		return nil
	end
	
	def doBuild(cmd)
		cmd.race.build(cmd.shipSize)
		return nil
	end
	
	def doDefense(cmd)
		cmd.planet.defenseUpBy(1)
		return nil
	end
	
	def doAttack(cmd)
		@rules.attemptAttack(@board, cmd) do
			| die1, die2 |
			[rand(die1), rand(die2)]
		end
	end
end
