# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

class ViewShip < Gtk::CList
	def initialize(game, observable)
		super(4)
		
		set_sensitive(false)
		
		titles = ["Owner", "Row", "Col", "Size"]
		titles.each_index do
			| col |
			set_column_title(col, titles[col])
		end
		column_titles_show
		append(["","","",""])
		
		show

		@info = ObservableProxy.new(observable, self, :updateInfo)

		@game = game
		@game.add_observer(self)
	end
	
	# user wants to look at a different ship
	def updateInfo(row, col)
		@row = row
		@col = col
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
		if (@oldShip == getShip)
			return nil
		end
		
		freeze	
		@oldShip = getShip
		if (@oldShip)
			text = []
			text << nameOfRace(@oldShip.owner)
			text << @oldShip.row.to_s
			text << @oldShip.col.to_s
			text << Ship.name(@oldShip.size) + " (#{@oldShip.size})"
		else
			text = ["","","",""]
		end
		4.times do
			| col |
			set_text(0, col, text[col])
		end
		thaw
		return nil
	end
	

=begin	
	def doExpose(w, e)
		window.clear
		ship = getShip
		if (ship)
			names = ['red', 'green', 'blue', 'yellow']
			gc = style.fg_gc(0)
			row = 0
			text = "SHIP at #{ship.row},#{ship.col}"
			row += style.font.string_height(text) + 2
			window.draw_string(style.font, gc, 0, row, text)
			name = names[ship.owner.quad]
			text = "Owner: #{name}"
			row += style.font.string_height(text) + 2
			window.draw_string(style.font, gc, 0, row, text)
			text = "Size: #{Ship.name(ship.size)} (#{ship.size})"
			row += style.font.string_height(text) + 2
			window.draw_string(style.font, gc, 0, row, text)
		end
	end
=end

	def getShip
		ship = @game.ships.find do
			| ship |
			ship.row == @row && ship.col == @col
		end
		return ship
	end
	
end
