# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

class ObservableProxy
	def initialize(observable, observer, method)
		@observer = observer
		@method = method
		observable.add_observer(self)
	end
	
	def update(*args)
		@observer.method(@method).call(*args)
	end
end

class ViewPlanet < Gtk::CList
	def initialize(game, observable)
		super(5)
		
		set_sensitive(false)
		
		titles = ["Owner", "Row", "Col", "Value", "Defense"]
		titles.each_index do
			| col |
			set_column_title(col, titles[col])
		end
		column_titles_show
		
		show
		
		@info = ObservableProxy.new(observable, self, :updateInfo)

		@game = game
		@game.add_observer(self)
	end
	
	# user wants to look at a different planet
	def updateInfo(row, col)
		@planet = @game.planets.find do
			| planet |
			planet.row == row && planet.col == col
		end
		if (@planet)
			@owner = @planet.owner
			@defense = @planet.defense
		end
		update
		return nil
	end
	
	def nameOfRace(race)
		names = ['red', 'green', 'blue', 'yellow']
		if (race)
			return names[race.quad]
		end
		return ""
	end
	
	
	# infopointer or game has changed
	def update
		freeze
		if (@planet)
			text = []
			text << nameOfRace(@planet.owner)
			text << @planet.row.to_s
			text << @planet.col.to_s
			text << @planet.size.to_s
			text << @planet.defense.to_s
		else
			text = ["","","","",""]
		end
		clear
		append(text)
		thaw
		return nil
	end
	
	
end
