# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

require 'observer'
require 'gtkcanvas'
require 'infopointer'

class ViewBoard < GtkCanvas
	attr_reader :infoPointer, :game
	
	def initialize(game, infoPointer)
		super()
		signal_connect("size_request") { |w, req| doSizeRequest(w,req) }
		signal_connect("configure_event") { |w, e| doConfigure(w,e) }
		signal_connect("motion_notify_event") do |w,e| doMotion(w, e) end
		signal_connect("button_press_event") do |w,e| doPress(w, e) end
		signal_connect("button_release_event") do |w,e| doRelease(w, e) end
		set_events(Gdk::POINTER_MOTION_MASK | 
				Gdk::BUTTON_PRESS_MASK | 
				Gdk::BUTTON_RELEASE_MASK)
		show

		setupColors
		@game = game
		@game.add_observer(self)
		@boardSize = @game.boardSize
		@infoPointer = infoPointer
		@infoPointer.attach(self)
	end
	
	def doSizeRequest(w, req)
		req.width = 400
		req.height = 400
	end
	
	def doConfigure(w, e)
		size = window.get_size
		puts "#{size[0]}x#{size[1]}"
		@width = size[0]
		@height = size[1]
    	return true
	end
	
	def doPress(widget, event)
		@mouseDown = true
		if (@infoPointer)
			newRow = event.y/cellSize
			newCol = event.x/cellSize
			@infoPointer.moveTo(newRow, newCol)
		end
	end
	
	def doRelease(widget, event)
		@mouseDown = false
	end
	
	def doMotion(widget, event)
		if (@mouseDown && @infoPointer)
			newRow = event.y/cellSize
			newCol = event.x/cellSize
			@infoPointer.moveTo(newRow, newCol)
		end
	end
	
	# called by @game via Observer interface
	def update
		clear
		gc = Gdk::GC.new(window)
		
		if (@infoPointer)
			gc.set_line_attributes(4, Gdk::LINE_SOLID, 
					Gdk::CAP_BUTT, Gdk::JOIN_MITER)
			@buffer.draw_rectangle(gc, false, 
					x(@infoPointer.col), y(@infoPointer.row), 
					cellSize, cellSize)
		end		
		
		@game.planets.each do
			| planet |
			drawPlanet(gc, planet)
		end
		
		@game.ships.each do
			| ship |
			drawShip(gc, ship)
		end
		
		gc.destroy
		queue_draw
		return
	end
	
	def drawPlanet(gc, planet)
		gc.set_foreground(@black)
		degrees = 0
		case planet.size
			when Planet::HOME, Planet::UNKNOWN
				degrees = 360
			when Planet::LARGE
				degrees = 270
			when Planet::MEDIUM
				degrees = 180
			when Planet::SMALL
				degrees = 90
			else
				raise "Unknown planet color"
		end		
		@buffer.draw_arc(gc, true, 
					x(planet.col), y(planet.row), cellSize, cellSize, 
					0, degrees*64)
		
		drawPlanetOwner(gc, planet)
	end
	
	def drawPlanetOwner(gc, planet)			
		owner = planet.owner
		if (owner)
			gc.set_foreground(@ownerColors[owner.quad])
			@buffer.draw_rectangle(gc, true,
					x(planet.col), y(planet.row) + cellSize/2, 
					cellSize, cellSize/6)
		end
					
	end
	
	def drawShip(gc, ship)
		gc.set_foreground(@ownerColors[ship.owner.quad])
		@buffer.draw_rectangle(gc, true,
					x(ship.col) + cellSize/2, y(ship.row),
					cellSize/6, cellSize)
	end
	
	def cellSize
		width, height = window.get_size
		fullSize = height
		if (width < height)
			fullSize = width
		end
		return (fullSize - 1) / @boardSize
	end
	
	def x(col)
		return cellStart(col)
	end
	
	def y(row)
		return cellStart(row)
	end
	
	def cellStart(rowCol)
		return rowCol * cellSize
	end

	def setupColors
		@black = createColor(0, 0, 0)
		@white = createColor(0xFFFF, 0xFFFF, 0xFFFF)
		
		red = createColor(0xFFFF, 0, 0)
		green = createColor(0, 0xC000, 0)
		blue = createColor(0, 0, 0xFFFF)
		yellow = createColor(0xFFFF, 0xFFFF, 0)
		
		@ownerColors = {}
		@ownerColors[0] = red
		@ownerColors[1] = green
		@ownerColors[2] = blue
		@ownerColors[3] = yellow
		
	end
	
	def createColor(r, g, b)
		color = Gdk::Color.new(r, g, b)
		colorMap = Gdk::Colormap.get_system
		colorMap.alloc_color(color, false, true)
		return color
	end
	
end
