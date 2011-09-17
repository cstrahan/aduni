#! /usr/local/bin/ruby

#  This program is an attempt to create a simple message window that can
#  be called from another program.

require 'gtk'

class MessageWindow < Gtk::Window
  def initialize(title, message)
    super(Gtk::WINDOW_TOPLEVEL)
    @title = title
    @message = message
    set_title(@title)
    signal_connect("destroy_event") do exit end
    signal_connect("delete_event") do exit end
    border_width(3)

    text = Gtk::Text.new
    text.set_editable(false)
    text.insert_text(@message, text.get_point)
    box = Gtk::VBox.new
    add(box)
    box.pack_start(text)

    button = Gtk::Button.new("Close")
    button.signal_connect("clicked") do exit end
    box.pack_start(button)
    # button.grab_focus
    show_all
    Gtk::main
    puts "#{@title}: #{@message}"
  end
end

# parse the command line for title and message
#  ind = 0
#  title = "Message Window"
#  message = "Dummy message."
#  if ARGV.length != 0
#    while true
#      # these things should be done as regular expressions
#      if ARGV[ind] == '-t' 
#        ind += 1
#        title = ARGV[ind].to_s ; ind += 1
#      elsif ARGV[ind] == '-m'
#        ind += 1
#        message = ARGV[ind] ; ind += 1
#      end
#      break if ind >= ARGV.length
#    end
#  end





















