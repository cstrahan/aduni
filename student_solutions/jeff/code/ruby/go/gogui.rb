#!/usr/local/bin/ruby
# gogui.rb
# JMR Wed Feb 14, 2001  9:37 AM

require 'gtk'
require 'go'

class GoMain < Gtk::Window
  def initialize
    cellSize = 30
    w = 19 ; h = 19
    super(Gtk::WINDOW_TOPLEVEL)
    set_title("Go Move-Finder")
    set_default_size(cellSize * w, cellSize * h)
    signal_connect("destroy_event") do exit end
    signal_connect("delete_event") do exit end
    signal_connect("button_press_event") do |w, e| doPress(w, e) end
    set_events(Gdk::POINTER_MOTION_MASK | 
	       Gdk::BUTTON_PRESS_MASK | 
	       Gdk::BUTTON_RELEASE_MASK)
    realize

    vbox = Gtk::VBox.new
    vbox.pack_start(makeMenu)
    vbox.pack_start(makeTable)    # creates and packs a table
    vbox.pack_start(makeButtons)  # creates and packs a HBox of buttons
    add(vbox)

    show_all		
    Gtk::main
  end

  def checkLegalMoves(color)
    @goBoard.checkLegalMoves(color)
    0.upto(18) do |y|
      0.upto(18) do |x| 
	c = @loc[x][y].color 
	if c == 'x'
	  pixmap, mask = Gdk::Pixmap::create_from_xpm(window, style.bg(Gtk::STATE_NORMAL), "badmove.xpm")
	  @board[x][y].set(pixmap, mask)
	  @board[x][y].show
	end
      end
    end
  end

  def doPress(widget, event)
    # code here eventually
  end

  def clear
    0.upto(18) do |y|
      0.upto(18) do |x| 
	c = @loc[x][y] 
	if c.color == 'x'
	  pixmap, mask = Gdk::Pixmap::create_from_xpm(window, style.bg(Gtk::STATE_NORMAL), "empty.xpm")
	  @board[x][y].set(pixmap, mask)
	  @board[x][y].show
	  c.color = '0'
	end
      end
    end
  end

  #------------------------------------------------------------------
  # Everything below this point is involved in making the various gui
  # bits.
  #------------------------------------------------------------------

  def makeMenu
    menubar = Gtk::MenuBar::new()
    menu = createFileMenu
    menuitem = Gtk::MenuItem::new("File")
    menuitem.set_submenu(menu)
    menubar.append(menuitem)

    menu = createMoveMenu
    menuitem = Gtk::MenuItem::new("Move")
    menuitem.set_submenu(menu)
    menubar.append(menuitem)
    return menubar
  end

  def createFileMenu
    menu = Gtk::Menu::new
    menuitem = Gtk::MenuItem::new("Exit Program")
    menuitem.signal_connect("activate") do exit end
    menu.append(menuitem)
    return menu
  end

  def createMoveMenu
    menu = Gtk::Menu::new
    menuitem = Gtk::MenuItem::new("Check White")
    menuitem.signal_connect("activate") do 
      clear; checkLegalMoves('W')
    end
    menu.append(menuitem)
    menuitem = Gtk::MenuItem::new("Check Black")
    menuitem.signal_connect("activate") do 
      clear; checkLegalMoves('B')
    end
    menu.append(menuitem)
    return menu
  end
  
  def makeTable
    @goBoard = GoBoard.new
    @loc = @goBoard.loc
    @goBoard.load("input.txt")
    @table = Gtk::Table.new(19, 19, false)
    @board = [] ; 0.upto(18) { @board << [] } 

    for y in 0...19
      for x in 0...19
	c = @loc[x][y].color 
	if c == 'W'
	  pix = new_pixmap("white.xpm", window, style.bg(Gtk::STATE_NORMAL))
	elsif c == 'B'
	  pix = new_pixmap("black.xpm", window, style.bg(Gtk::STATE_NORMAL))	  
	else 
	  pix = new_pixmap("empty.xpm", window, style.bg(Gtk::STATE_NORMAL))	  
	end
	@board[x][y] = pix
	@table.attach(@board[x][y], x, x+1, y, y+1, nil, nil, 0, 0)
	@board[x][y].show
      end
    end
    return @table
  end
  
  def drawTable
    # insert them into a table
    @goBoard = GoBoard.new
    @loc = @goBoard.loc
    @goBoard.load("input.txt")
    for y in 0...19
      for x in 0...19
	c = @loc[x][y].color
	if c == 'W'
	  pixmap, mask = Gdk::Pixmap::create_from_xpm(window, style.bg(Gtk::STATE_NORMAL), "white.xpm")
	elsif c == 'B'
	  pixmap, mask = Gdk::Pixmap::create_from_xpm(window, style.bg(Gtk::STATE_NORMAL), "black.xpm")
	else 
	  pixmap, mask = Gdk::Pixmap::create_from_xpm(window, style.bg(Gtk::STATE_NORMAL), "empty.xpm")
	end
	@board[x][y].set(pixmap, mask)
      end
    end
    @table.show
  end

  def makeButtons
    # make a button panel for some buttons
    hbox = Gtk::HBox.new
    button = Gtk::Button.new("Illegal moves for black")
    button.signal_connect("button_press_event") do clear ; checkLegalMoves('B') end
    hbox.pack_start(button)

    button = Gtk::Button.new("Illegal moves for white")
    button.signal_connect("button_press_event") do clear ; checkLegalMoves('W') end
    hbox.pack_start(button)

    button = Gtk::Button.new("Clear")
    button.signal_connect("button_press_event") do clear  end
    hbox.pack_start(button)

    button = Gtk::Button.new("Reload state")
    button.signal_connect("button_press_event") do 
      drawTable
    end
    hbox.pack_start(button)
    return hbox
  end
  
  def new_pixmap(filename, window, background)
    pixmap, mask = Gdk::Pixmap::create_from_xpm(window, background, filename)
    wpixmap = Gtk::Pixmap::new(pixmap, mask)
  end
end

GoMain.new
