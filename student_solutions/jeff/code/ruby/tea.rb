#!/usr/local/bin/ruby

require 'simple_window'

class Timer < Thread
  def initialize(time)
    @time = time
    puts "going to sleep for #{@time} seconds"
    sleep @time
    puts "woke up"
    MessageWindow.new("YO!", "Tea is ready")
  end
end

if ARGV.length == 1
  time = ARGV[0].to_i 
else
  time = 3.5
end

t = Timer.new(time * 60)
