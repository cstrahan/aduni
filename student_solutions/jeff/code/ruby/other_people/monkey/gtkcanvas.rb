# Monkeys software Copyright 2001 by Kevin B. Smith
#     Released under GPL 2.0
# Original Space Monkeys game rules (C) 1999 Stephen Glenn,
#     used by permission of the author

class GtkCanvas < Gtk::DrawingArea
  def initialize
    super
    signal_connect("expose_event") { |w,e| expose_event(w,e) }
    signal_connect("configure_event") { |w, e| configure_event(w,e) }
    @buffer = nil
    @bgc = nil
  end

  def expose_event(w,e)
    if ! @buffer.nil?
      rec = e.area
      w.window.draw_pixmap(@bgc, @buffer, rec.x, rec.y,
			   rec.x, rec.y, rec.width, rec.height)
    end
    false
  end

  def clear(b = @buffer)
    return if b.nil?

	if @bgc.nil?
	    @bgc = self.style.bg_gc(self.state)
	end
    if (@width > 0 && @height > 0)
      b.draw_rectangle(@bgc, true, 0,0, @width, @height)
    end
  end

  def configure_event(w,e)
    g = w.window.get_geometry
	@width = g[2]
	@height = g[3]
    if (@width > 0 && @height > 0)
      b = Gdk::Pixmap.new(w.window, @width, @height, -1)
      clear(b)
      if not @buffer.nil?
		g = @buffer.get_geometry
		b.draw_pixmap(@bgc, @buffer, 0,0,
		      0, 0, @width, @height)
      end
      @buffer = b
    end
    true
  end
end


