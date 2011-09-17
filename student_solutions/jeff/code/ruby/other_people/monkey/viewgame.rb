# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'gtk'
require 'viewboard'
require 'viewscore'
require 'viewplanet'
require 'viewship'
require 'viewcommand'
require 'game'
require 'player'

class ViewGame < Gtk::Window
	def initialize
@debug = false
		super(Gtk::WINDOW_TOPLEVEL)
		set_title('Space Monkeys')
		set_default_size(600,300)
		signal_connect("delete_event") do exit end
		signal_connect("destroy_event") do exit end
		set_events(Gdk::KEY_PRESS_MASK)
		realize
		
		@game = Game.new
		@infoPointer = InfoPointer.new

		createViews
		show_all
		
		createPlayers
		
		signal_connect("key_press_event") do |event| @viewCommand.key(event) end
		grab_focus
		Gtk::idle_add do doTick end
		Gtk.main
	end
	
	def createViews
		hbox = Gtk::HBox.new
		
		@viewBoard = ViewBoard.new(@game, @infoPointer)
		frame = Gtk::Frame.new("Galaxy")
		frame.add(@viewBoard)
		hbox.pack_start(frame)
		
		vbox = Gtk::VBox.new

		@viewScore = ViewScore.new(@game)
		frame1 = Gtk::Frame.new("Scores")
		frame1.add(@viewScore)		
		vbox.pack_start(frame1)
		
		viewPlanet = ViewPlanet.new(@game, @infoPointer)
		frame2 = Gtk::Frame.new("Planet")
		frame2.add(viewPlanet)
		vbox.pack_start(frame2)
		
		viewShip = ViewShip.new(@game, @infoPointer)
		frame3 = Gtk::Frame.new("Ship")
		frame3.add(viewShip)
		vbox.pack_start(frame3)
		
		@viewCommand = ViewCommand.new(@infoPointer)
		frame4 = Gtk::Frame.new("Command")
		frame4.add(@viewCommand)
		vbox.pack_start(frame4)
		
		hbox.pack_start(vbox)
		
		add(hbox)
		return nil
	end
	
	# level definitions
	# 0 = all play original green/blue
	# 1 = blue/green are skill 0, yellow is skill 1
	# 2 = all play skill 1
	# 3 = all play skill 2
	def createPlayers
		level = 1
		4.times do
			| quad |
			if (quad == 0)
				player = PlayerHuman.new(@infoPointer)
				player.setViewCommand(@viewCommand)
			elsif (level == 1 && quad == 1)
				player = PlayerSimple.new(1)
			else
				if(level >= 3)
					player = PlayerSimple.new(2)
				elsif(level == 2)
					player = PlayerSimple.new(1)
				else
					player = PlayerSimple.new(0)
				end
			end
			@game.addPlayer(player)
		end
	end

	# called by Gtk timer
	def doTick
		player = @game.whoseTurn?
		if (@game.hasWon(player))
puts "Player #{player.race.quad} has won in #{@game.turn} turns!"
			return false
		end
		while (@game.hasLost(player))
puts "Player #{player.race.quad} has lost!"
			@game.removePlayer(player)
			player = @game.whoseTurn?
		end
		player.takeTurn
		return true
	end
end

ViewGame.new
