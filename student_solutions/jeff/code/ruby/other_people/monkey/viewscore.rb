# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

class ViewScore < Gtk::CList
	def initialize(game)
		super(4)
		@oldText = {}
		
		set_sensitive(false)
		
		titles = ["Player", "Score", "Building", "Left"]
		titles.each_index do
			| col |
			set_column_title(col, titles[col])
		end
		column_titles_show
		
		show

		@game = game
		update
		
		@game.add_observer(self)
	end
	
	def nameOf(player)
		return ['red', 'green', 'blue', 'yellow'][player.race.quad]
	end
	
	def shipOf(player)
		return Ship.name(player.race.newShipSize)
	end
	
	def turnsLeftOf(player)
		return player.race.newShipLeft.to_s
	end
	
	def update
		freeze
		if (rows != @game.players.size)
			clear
			@oldText = {}
		end
		@game.players.each do
			| player |
			text = []
			text << nameOf(player)
			text << @game.victoryPoints(player).to_s
			text << shipOf(player)
			text << turnsLeftOf(player)
			if (text != @oldText[player])
				@oldText[player] = text
				row = find_row_from_data(player)
				if (row >= 0)
					4.times do
						| col |
						set_text(row, col, text[col])
					end
				else
					row = append(text)
					set_row_data(row, player)
				end
			end
		end
		thaw
	end
	
	def find_row_from_data(data)
		rows.times do
			| row |
			if (get_row_data(row) == data)
				return row
			end
		end
		return -1
	end
	
end
