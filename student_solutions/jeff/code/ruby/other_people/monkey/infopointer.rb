# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'observer'

class InfoPointer
	include Observable
	
	attr_reader :row, :col
	
	def initialize
		@row = 0
		@col = 0
	end
	
	def attach(viewBoard)
		if (@viewBoard)
			raise "Can't attach to two viewboards"
		end
		@viewBoard = viewBoard
		@boardSize = @viewBoard.game.boardSize
		return nil
	end
	
	def moveTo(newRow, newCol)
		if (newRow < 0)
			newRow = 0
		end
		if (newRow >= @boardSize)
			newRow = @boardSize - 1
		end
		if (newCol < 0)
			newCol = 0
		end
		if (newCol >= @boardSize)
			newCol = @boardSize - 1
		end
		if (newRow != @row || newCol != @col)
			@row = newRow
			@col = newCol
			changed(true)
			notify_observers(@row, @col)
			@viewBoard.update
		end
		return nil	
	end
		
end
