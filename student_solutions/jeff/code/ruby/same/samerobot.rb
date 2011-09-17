module SameRobot
  def robotMove
    return false if gameOver? == true    
    @parent.report("Searching...")
    result = robotFindBest
    robotResults(result[0], result[1])
  end

  def robotFindBest
    max = 0
    state = saveState
    @final = @initial = best = []
    robotFindMoves {|x,y| @initial.push([x,y]) }
    if @initial.length > 8
      @maxDepth = 3
    else
      @maxDepth = 4
    end
    @initial.each do |move| 
      loadState(state)
      doRobot(move[0], move[1])
      value = robotSearch(saveState, @score)
      puts "(#{move[0]},#{move[1]}) -> #{value}"
      if value > max 
	best = [move[0], move[1]] 
	max = value
      end
      GC.start			# garbage collect
    end 
    loadState(state)
    puts "Best: #{max}"
    return best
  end

  def robotResults(x, y)
    clearAll ; clearOptimal
    floodFill(x, y, @board[x][y].state, 'OPTIMAL')
    @parent.report("Optimal (depth #{@maxDepth})")
    repaint
  end

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
    compress
    if gameOver? 
      @score = @score + 1000 if @board[@left][@bottom].state == -1
      return true
    end
    false
  end

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

  def robotPlay
    @roboPlay = true
    while true do
      move = robotFindBest
      clearAll ; repaint
      points = kill(move[0], move[1]) 
      reportScore(points - 2)
      compress ; clearMarks ; repaint
      gameOver ; break if gameOver? == true
      sleep 1
    end
    clearAll ; repaint
    @roboPlay = false
  end
end




