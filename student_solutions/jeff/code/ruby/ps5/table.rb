#!/usr/local/bin/ruby
=begin

  clist.rb - a part of testgtk.c rewritten in ruby-gtk

=end

require 'gtk'
require 'series'
class Test < Gtk::Window
  def initialize(n)
    super(Gtk::WINDOW_TOPLEVEL)
    set_title("World Series")
    signal_connect("destroy_event") do exit end
    signal_connect("delete_event") do exit end
    set_default_size(n * 70,n * 30)
    realize
    vbox = Gtk::VBox::new()
    scrolled_win = Gtk::ScrolledWindow::new()
    scrolled_win.border_width = 5
    scrolled_win.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC)


    s = Series.new(n)
    @table = s.stuff

    # create GtkCList here so we have a pointer to throw at the 
    # button callbacks -- more is done with it later
    @clist = Gtk::CList::new(n)
    scrolled_win.add(@clist)
    vbox.pack_start(scrolled_win)
    @clist.set_row_height(25)

    for i in 0...n do
      @clist.set_column_width(i, 60)
    end
    for i in 0...n do
      temp = [] ; @table[i].each { |x| temp << x.to_s }
      @clist.append(temp)
    end

    separator = Gtk::HSeparator::new()
    vbox.pack_start(separator, false, true, 0)

    hbox = Gtk::HBox::new()
    vbox.pack_start(hbox, false, true, 0)

    @add_remove = false
    @style1 = nil
    @style2 = nil
    @style3 = nil
    add(vbox)
    show_all
    Gtk.main
  end
end

Test.new(7)
