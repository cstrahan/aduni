# samerobot.rb
# part of Ruby SameGame
#  AUTHOR:   Jeffrey Radcliffe
# LICENSE:   GPL version 2.0
#    DATE:   Sun Feb 18, 2001

module SameRobot
  # initiates a robot move
  def robotMove
    return false if gameOver? == true    
    result = robotFindBest
    robotResults(result[0], result[1])
  end

  # finds the best move for the current depth level.
  # WARNING -- This algorithm gets very big at high
  # depth levels.  Adjust @maxDepth accordingly...
  def robotFindBest
    @maxDepth = 3		# not very accurate, but safe
    max = 0
    state = saveState
    @final = @initial = best = []
    robotFindMoves {|x,y| @initial.push([x,y]) }
    @initial.each do |move| 
      loadState(state)
      doRobot(move[0], move[1])
      value = robotSearch(saveState, @score)
      puts "(#{move[0]},#{move[1]}) -> #{value}"
      if value > max 
	best = [move[0], move[1]] 
	max = value
      end
    end 
    loadState(state)
    puts "Best: #{max}"
    return best
  end

  # reports robot results by highlighting the best move the robot found
  def robotResults(x, y)
    clearAll ; clearOptimal
    floodFill(x, y, @board[x][y].state, 'OPTIMAL')
    @parent.report("Score:  #{@score}  optimal (depth #{@maxDepth})")
    repaint
  end

  # finds all the possible moves in a board configuration
  def robotFindMoves
    clearMarks
    everyNode do |x, y|
      next if @board[x][y].state == -1
      if @board[x][y].mark != true 
	value = floodFill(x, y, @board[x][y].state, 'MARK') 
	yield(x,y) if value > 1
      end
    end
  end

  def doRobot(x, y)
    clearAll
    points = kill(x,y) 
    @score = @score + ((points - 2) ** 2)
    compress ; clearMarks
    if gameOver? 
      @score = @score + 1000 if @board[@left][@bottom].state == -1
      return true
    end
    false
  end

  # a depth first search into all the possiblities of the board.
  def robotSearch(state, max)
    loadState(state)
    return max if (state.depth + 1) == @maxDepth
    moves = []
    robotFindMoves { |x,y| moves.push([x, y]) }
    nextLevel = []
    moves.each do |move|
      clearMarks
      loadState(state)
      ret = doRobot(move[0], move[1])
      max = @score if @score > max
      newState = saveState
      newState.depth = state.depth + 1
      nextLevel.push(newState) if ret == false
    end
    nextLevel.each { |x| robotSearch(x, max) }
    return max
  end
  
  # robot player (not ready yet ; works, but not spectacular)
  #    def robotPlay
  #      @roboPlay = true
  #      while true do
  #        move = robotFindBest
  #        clearAll ; repaint
  #        points = kill(move[0], move[1]) 
  #        reportScore(points - 2)
  #        compress ; clearMarks ; repaint
  #        gameOver ; break if gameOver? == true
  #        sleep 1
  #      end
  #      clearAll ; repaint
  #      @roboPlay = false
  #    end
end




