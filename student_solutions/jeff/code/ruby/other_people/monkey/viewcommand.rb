# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

class ViewCommand < Gtk::VBox
	def initialize(infoPointer)
		super(false, 0)
		
		@ships = {}
		
		@infoPointer = infoPointer

		@buildSmall = Gtk::RadioButton.new(nil, "Build Cruiser")
		@buildLarge = Gtk::RadioButton.new(@buildSmall, "Build Monitor")
		@launch = Gtk::RadioButton.new(@buildLarge, "Launch")
		@defense = Gtk::RadioButton.new(@launch, "Defense")
		@colonize = Gtk::RadioButton.new(@defense, "Colonize")
		@invade = Gtk::RadioButton.new(@colonize, "Invade")
		@move = Gtk::RadioButton.new(@defense, "Move")
		@attack = Gtk::RadioButton.new(@move, "Attack")

		@north = Gtk::RadioButton.new(nil, "N")
		@west = Gtk::RadioButton.new(@north, "W")
		@east = Gtk::RadioButton.new(@west, "E")
		@south = Gtk::RadioButton.new(@east, "S")
		
		@ok = Gtk::Button.new("OK")
		
		@buttons = {}
		@buttons['a'] = @attack
		@buttons['c'] = @colonize
		@buttons['d'] = @defense
		@buttons['e'] = @east
		@buttons['i'] = @invade
		@buttons['l'] = @launch
		@buttons['m'] = @move
		@buttons['n'] = @north
		@buttons['s'] = @south
		@buttons['w'] = @west
		@buttons['b'] = @buildLarge

		planetSelect = Gtk::Button.new("Select")
		planetSelect.signal_connect("clicked") do doPlanetSelect end
		@planet = Gtk::Label.new("")

		shipSelect = Gtk::Button.new("Select")
		shipSelect.signal_connect("clicked") do doShipSelect end
		@ship = Gtk::Label.new("")

		box = Gtk::HBox.new(false, 0)
		box.pack_start(@buildSmall)
		box.pack_start(@buildLarge)
		pack_start(box)
		
		pack_start(Gtk::HSeparator.new)
		
		box = Gtk::HBox.new(false, 0)
		label = Gtk::Label.new("Planet")
		box.pack_start(label, false)
		box.pack_start(@planet)
		box.pack_start(planetSelect, false)
		pack_start(box)
				
		box = Gtk::HBox.new(false, 0)
		box.pack_start(@launch)
		box.pack_start(@defense)
		pack_start(box)

		pack_start(Gtk::HSeparator.new)
		
		box = Gtk::HBox.new(false, 0)
		label = Gtk::Label.new("Ship")
		box.pack_start(label, false)
		box.pack_start(@ship)
		box.pack_start(shipSelect, false)
		pack_start(box)

		bigBox = Gtk::HBox.new(false, 0)		
			left = Gtk::VBox.new(false, 0)
			left.pack_start(@colonize)
			left.pack_start(@invade)

			right = Gtk::VBox.new(false, 0)
			hbox = Gtk::HBox.new(false, 0)
			hbox.pack_start(@move)
			hbox.pack_start(@attack)
			right.pack_start(hbox)

			table = Gtk::Table.new(2, 4, false)
			table.attach(@north, 2, 3, 0, 1)
			table.attach(@west, 1, 2, 1, 2)
			table.attach(@east, 3, 4, 1, 2)
			table.attach(@south, 2, 3, 1, 2)
			right.pack_start(table)
			
			bigBox.pack_start(left)
			bigBox.pack_start(Gtk::VSeparator.new)
			bigBox.pack_start(right)
		pack_start(bigBox)
				
		@ok.signal_connect("clicked") do doOk end
		@ok.grab_default
		pack_start(@ok)
		
		show_all
		grab_focus
	end

	def key(event)
		case event[1].keyval
			when 65361	# left
				@infoPointer.moveTo(@infoPointer.row, @infoPointer.col-1)
			when 65362	# up
				@infoPointer.moveTo(@infoPointer.row-1, @infoPointer.col)
			when 65363	# right
				@infoPointer.moveTo(@infoPointer.row, @infoPointer.col+1)
			when 65364	# down
				@infoPointer.moveTo(@infoPointer.row+1, @infoPointer.col)
			when 65293	# enter
				doOk
			else
				s = event[1].string
				if(s == "\r")
					return doOk
				elsif(s == "p")
					doPlanetSelect
					doShipSelect
					return true
				end
				control = @buttons[s.downcase]
				if(control)
					control.set_active(true)
				else
					p "ignoring #{s} aka #{event[1].keyval}"
				end
		end
		@ok.grab_focus
		return false
	end

	def doOk
		if (@buildSmall.active?)
			@cmd = "b c"
		elsif (@buildLarge.active?)
			@cmd = "b m"
		elsif (@launch.active?)
			planet = @currentPlanet
			if (planet)
				row = planet.row
				col = planet.col
				@cmd = "l #{row} #{col}"
			end
		elsif (@defense.active?)
			planet = @currentPlanet
			if (planet)
				row = planet.row
				col = planet.col
				@cmd = "d #{row} #{col}"
			end
		elsif (@colonize.active?)
			ship = @currentShip
			if (ship)
				row = ship.row
				col = ship.col
				@cmd = "c #{row} #{col}"
			end
		elsif (@invade.active?)
			ship = @currentShip
			if (ship)
				row = ship.row
				col = ship.col
				@cmd = "i #{row} #{col}"
			end
		elsif (@move.active?)
			ship = @currentShip
			if (ship)
				row = ship.row
				col = ship.col
				@cmd = "m #{row} #{col} #{getDir}"
			end
		elsif (@attack.active?)
			ship = @currentShip
			if (ship)
				row = ship.row
				col = ship.col
				@cmd = "a #{row} #{col} #{getDir}"
			end
		else
			raise "No command button selected!"
		end
	end
	
	def setPlayer(player)
		@player = player
		@player.game.add_observer(self)
	end

	def clear
		@cmd = nil
		return nil
	end
	
	def getCommand
		@ok.grab_focus
		while (!@cmd && Gtk::events_pending)
			Gtk::main_iteration
		end
		return @cmd
	end
	
	def update
		@ok.grab_focus
		if (!@currentPlanet)
			@currentPlanet = getFirstPlanet
		end
		updateShipText
		updatePlanetText
		return nil
	end
	
protected
	def updateShipText
		@ship.set_text(getShipText(@currentShip))
	end
	
	def updatePlanetText
		@planet.set_text(getPlanetText(@currentPlanet))
	end

	def getDir
		if (@north.active?)
			dir = 'n'
		elsif (@west.active?)
			dir = 'w'
		elsif (@east.active?)
			dir = 'e'
		elsif (@south.active?)
			dir = 's'
		else
			raise "No direction selected"
		end
		return dir
	end
	
	def getFirstPlanet
		@player.game.planets(@player.race).each do
			| planet |
			return planet
		end
		return nil
	end
	
	def getPlanetText(planet)
		if (!planet)
			return "(none)"
		end
		return "#{planet.size} at #{planet.row},#{planet.col}"
	end
	
	def doPlanetSelect
		@player.game.planets(@player.race).each do
			| planet |
			if (planet.row == @infoPointer.row && 
					planet.col == @infoPointer.col)
				@currentPlanet = planet
				break
			end
		end
		updatePlanetText
		return nil
	end

	def getShipText(ship)
		if (!ship)
			return "(none)"
		end
		return "#{Ship.name(ship.size)} at #{ship.row},#{ship.col}"
	end
	
	def doShipSelect
		@player.game.ships(@player.race).each do
			| ship |
			if (ship.row == @infoPointer.row && 
					ship.col == @infoPointer.col)
				@currentShip = ship
				break
			end
		end
		updateShipText
		return nil
	end
end
