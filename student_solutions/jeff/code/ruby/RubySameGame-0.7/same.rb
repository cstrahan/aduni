#!/usr/local/bin/ruby

# same.rb
# Part of Ruby SameGame - a Ruby/Gtk version of SameGame
#  AUTHOR:   Jeffrey Radcliffe
# LICENSE:   GPL version 2.0
#    DATE:   Sun Feb 18, 2001

require 'gtk'
require 'gtkcanvas'
require 'samerobot'

class SameGame < Gtk::Window
  attr_reader :cellSize, :row, :column
  def initialize(row, column)
    @row = row ; @column = column
    @cell = 40			
    super(Gtk::WINDOW_TOPLEVEL)
    set_title("Ruby SameGame")
    signal_connect("delete_event")  { exit }
    signal_connect("destroy_event") { exit }
    realize
    @vbox = Gtk::VBox.new
    @vbox.pack_start(createMenu, false, true, 0)
    @vbox.pack_start(createBoard, false, true, 0)
    @vbox.pack_start(createBottom, false, true, 0)
    add(@vbox)
    show_all
    @board.repaint
    Gtk.main
  end
  
  # receives messages sent from the game proper and
  # pushes them onto the status bar
  def report(message)
    @status.pop(@context)
    @status.push(@context, message)
  end
  
  # creates the game proper
  def createBoard
    @board = Game.new(self, @cell, @row, @column)
  end

  # creates a very basic menu bar
  def createMenu
    menubar = Gtk::MenuBar::new()
    menubar.show
    menu = createFileMenu
    menuitem = Gtk::MenuItem::new("File")    
    menuitem.set_submenu(menu)
    menubar.append(menuitem)
    menuitem.show
    return menubar
  end

  # creates the File submenu
  def createFileMenu
    menu = Gtk::Menu::new
    menuitem = Gtk::MenuItem::new("New Game")
    menuitem.signal_connect("activate") { @board.newGame }
    menu.append(menuitem)

    menuitem = Gtk::MenuItem::new("Replay Last")
    menuitem.signal_connect("activate") { @board.replayLast }
    menu.append(menuitem)

    # The robot player is not ready yet...
    #    menuitem = Gtk::MenuItem::new("RobotPlay")
    #    menuitem.signal_connect("activate") { @board.robotPlay }
    #    menu.append(menuitem)

    menuitem = Gtk::MenuItem::new("Exit Game")
    menuitem.signal_connect("activate") { exit }
    menu.append(menuitem)
    return menu
  end
  
  # creates a status bar and a hint button on the bottom of the window
  def createBottom
    hbox = Gtk::HBox.new
    @status = Gtk::Statusbar.new
    @context = @status.get_context_id("4")
    @status.push(@context, "Welcome to SameGame!")
    hbox.pack_start(@status)
    button = Gtk::Button.new("Hint")
    button.signal_connect("button_press_event") { @board.robotMove }
    hbox.pack_start(button, false, true, 0)
    return hbox
  end
end
#===============================================================
# a basic node structure, happily storing some data
class GameNode
  attr_writer :state, :highlight, :mark, :optimal
  attr_reader :state, :highlight, :mark, :optimal
end
#===============================================================
class Game < GtkCanvas
  include SameRobot  # load robot player Module
  def initialize(parent, cell, row, column)
    super()
    signal_connect("size_request") { |w, req| doSizeRequest(w, req) }
    signal_connect("configure_event") { |w, e| doConfigure(w, e) }
    signal_connect("motion_notify_event") { |w, e| doMotion(w, e) }
    signal_connect("button_press_event") { |w, e| doPress(w, e) }
    signal_connect("leave_notify_event") { clearAll ; repaint  }
    set_events(Gdk::EXPOSURE_MASK |
	       Gdk::BUTTON_PRESS_MASK |
	       Gdk::ENTER_NOTIFY_MASK |
	       Gdk::LEAVE_NOTIFY_MASK |
	       Gdk::POINTER_MOTION_MASK )
    @parent = parent
    @cell = cell
    @row = row
    @column = column
    @top = @left = @score = 0 
    @bottom = @column - 1
    @right = @row - 1
    @x = @y = -100
    size(@cell * @row, @cell * @column)
    setBoard(@row, @column)
    loadTileImages
  end

  def getCoord(w, e)
    x = e.x / @cell
    y = e.y / @cell
    return @x, @y if y > @bottom
    return x, y
  end

  def doSizeRequest(w, req)
    # code here
  end

  def doConfigure(w, e)
    # code here
  end

  def doPress(w, e)
    x, y = getCoord(w, e)
    clearAll ; clearOptimal
    if checkValid(x,y) == true and @board[x][y].state != DEAD
      points = kill(x,y) 
      reportScore(points - 2)
      compress
      clearMarks
      gameOver if gameOver?
      highlight(x,y) if checkValid(x,y) == true
      repaint
    end
  end

  def doMotion(w, e)
    x, y = getCoord(w, e)
    if x != @x or y != @y
      clearAll
      @x = x ; @y = y
      highlight(x,y) if checkValid(x,y) == true
      repaint
    end
  end
  #==============================================================
  # reports the score to the GUI.
  def reportScore(points)
    @score += points ** 2
    @parent.report("Score:  #{@score}")
  end

  # initiates a new game.
  def newGame
    everyNode do |x, y| 
      @board[x][y].state = rand(3) 
      @board[x][y].highlight = false
    end
    @x = @y = -100
    @score = 0
    @parent.report("Score: 0")
    @originalState = saveState	# save the state for replay
    repaint
  end

  # checks if the game is over by checking if there are any
  # valid moves to make. Will return as soon as it finds
  # one move that can be made.
  def gameOver?
    clearMarks
    everyNode do |x, y|  
      next if @board[x][y].state == DEAD
      return false if checkValid(x, y) == true
    end
    return true
  end
  
  # run when the game is over. Sets bonus (if there is one)
  # and reports the final results.
  def gameOver
    # calculate the bonus
    @score += 1000 if @board[@left][@bottom].state == DEAD
    @gameOver = true
    @parent.report("Score:  #{@score}   Game Over")
  end

  # loads the state at the beginning of the game and resets
  # the score.
  def replayLast
    loadState(@originalState)
    @parent.report("Score: 0")
    clearAll ; repaint
  end
  #==============================================================
  def clearMarks
    everyNode { |x, y| @board[x][y].mark = false }
  end

  def clearOptimal
    everyNode { |x, y| @board[x][y].optimal = false }
  end

  def clearAll
    everyNode do |x, y|
      @board[x][y].mark = @board[x][y].highlight = false
    end
  end

  # initiates a depth-first search, and performs various tasks
  # based on the message passed along
  def floodFill(x, y, state, message)
    c = 0
    node = @board[x][y]
    return c if node.state != state
    return c if node.mark == true

    c = 1			
    node.mark = true
    node.highlight = true if message == 'HIGHLIGHT'
    node.state = -1 if message == 'DIE'
    node.optimal = true if message == 'OPTIMAL'
    
    c += floodFill(x+1, y, state, message) if x+1 < @row
    c += floodFill(x-1, y, state, message) if x-1 >= 0
    c += floodFill(x, y+1, state, message) if y+1 < @column    
    c += floodFill(x, y-1, state, message) if y-1 >= 0
    return c
  end

  def checkValid(x, y)
    clearAll
    value = floodFill(x, y, @board[x][y].state, 'MARK')
    return true if value > 1 
    return false 
  end

  def highlight(x, y)
    clearMarks
    value = floodFill(x, y, @board[x][y].state, 'HIGHLIGHT')
  end

  def kill(x, y)
    clearMarks
    value = floodFill(x, y, @board[x][y].state, 'DIE')
  end

  #====================================================
  # The following few methods deal with the compression
  # algorithm which compacts the board after a move is
  # made and tiles are removed.  It is at present not
  # optimized, but works reliably.
  #====================================================
  
  # initail call to compress. Compresses first vertically,
  # then horizontally if needed
  def compress
    @needsCompress = false
    verticalCompress(@left, @bottom)
    horizontalCompress(@left, @bottom) while @needsCompress == true
  end

  def verticalCompress(x, y)
    node = @board[x][y]
    if node.state != DEAD
      if y != @top
	verticalCompress(x, y-1)
	if y == @bottom and x != @right
	  verticalCompress(x+1, y) 
	end
      end
      return
    end
    
    return if y == @top
    tempState = swapVertical(x, y-1, node.state)
    if tempState == DEAD
      if y == @bottom
	return if x == @right
	@needsCompress = true
      end
    else
      node.state = tempState
      verticalCompress(x, y-1)
    end
    if y == @bottom and x != @right
      verticalCompress(x+1, y)
    end
  end

  # looks for the first non-dead tile vertically
  # and swaps state with that tile.
  def swapVertical(x, y, color)
    node = @board[x][y]
    if node.state != DEAD
      temp = node.state
      node.state = DEAD
      return temp
    end
    return DEAD if y == @top
    return swapVertical(x, y-1, color)
  end

  # finds the next non-dead column if there is one
  # and swaps state with it.
  def horizontalCompress(x, y)
    node = @board[x][y]
    if x == @right
      @needsCompress = false
      return
    end
    if node.state != DEAD
      horizontalCompress(x+1, y)
      return
    end
    column = locateNextColumn(x, y)
    if column == DEAD
      @needsCompress = false
      return
    end
    for j in 0...@column
      @board[x][j].state = @board[column][j].state
      @board[column][j].state = DEAD
    end
  end

  def locateNextColumn(x, y)
    node = @board[x][y]
    return x if node.state != DEAD
    return DEAD if x == @right
    return locateNextColumn(x+1, y)
  end
  
  #==============================================================

  # the initial setting up of the board. Creates the board array,
  # and sets each state randomly
  def setBoard(w, h)
    @board = [] ; w.times { @board << [] }
    everyNode do |x, y|
      @board[x][y] = GameNode.new
      @board[x][y].state = rand(3)
    end
    @originalState = saveState	# saves the state for Replay
  end

  # repaints all tiles on the board
  def repaint
    everyNode { |x, y| drawPiece(x, y, stateAt(x, y)) }
  end

  # draws a pixmap onto the drawing area
  def drawPiece(x, y, pix)
    gc = Gdk::GC.new(window)
    @buffer.draw_pixmap(gc, pix, 0, 0, x * @cell, y * @cell, @cell, @cell)
    queue_draw		
  end
  
  # given x, y coordinates, returns the pixmap associated with the state of
  # that location.
  def stateAt(x, y)
    node = @board[x][y] 
    return @pix8 if node.optimal == true
    case node.state
    when 0
      if node.highlight != true
	return @pix1 else return @pix4
      end
    when 1 
      if node.highlight != true
	return @pix2 else return @pix5
      end
    when 2 
      if node.highlight != true 
	return @pix3 else return @pix6
      end
    else
      return @pix7
    end
  end

  # loads the set of game tile images.
  def loadTileImages
    @pix1, @mask1 = makePixmap("tile1.jpg")
    @pix2, @mask2 = makePixmap("tile2.jpg")
    @pix3, @mask3 = makePixmap("tile3.jpg")
    @pix4, @mask4 = makePixmap("tile4.jpg")
    @pix5, @mask5 = makePixmap("tile5.jpg")
    @pix6, @mask6 = makePixmap("tile6.jpg")
    @pix7, @mask7 = makePixmap("empty.png")
    @pix8, @mask8 = makePixmap("good1.jpg")
  end
  
  # Converts the images into pixmap format
  def makePixmap(filename)
    im = GdkImlib::Image.new(filename)
    im.render(im.rgb_width, im.rgb_height)
    return im.get_pixmap
  end

  # saves the current state and score of the game
  def saveState
    board = []
    @row.times { board << [] }
    everyNode do |x, y|
      board[x][y] = @board[x][y].state
    end
    return StateObject.new(board, @score)
  end
  
  # loads a previously saved state
  def loadState(object)
    @score = object.score
    everyNode do |x, y|
      @board[x][y].state = object.board[x][y]
    end
    clearAll ; clearOptimal
    repaint
  end

  # iterates through the game board, starting from
  # the bottom left through the top right.
  def everyNode
    @bottom.downto(@top) do |y|
      @left.upto(@right) do |x|
	yield(x, y)		# back to the calling method
      end
    end
  end

  DEAD = -1
end
#===============================================================
# An object to store the state of the game at any point.
# This is used both for the Replay function, and for the 
# robot player
class StateObject
  attr_reader :board, :score, :depth
  attr_writer :depth

  def initialize(board, score)
    @board = board
    @score = score
    @depth = 1
  end
end
#=====================================================================
def main
  # set the size of the board. No error checking is done yet.
  if ARGV.length == 2
    w, h = ARGV[0].to_i , ARGV[1].to_i
  else
    w, h = 15, 10
  end
  SameGame.new(w, h)
end

main
