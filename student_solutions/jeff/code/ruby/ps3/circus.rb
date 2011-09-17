# circus.rb
# Author: Jeffrey M. Radcliffe
# Date:   Mon Feb 12, 2001  2:52 PM

class Performer
  attr_writer :connections
  attr_reader :weight, :height, :connections

  def initialize(w, h)
    @weight = w ; @height = h ; @connections = []
  end

  def addConnection(p)
    @connections.push(p)
  end

  def listConnections
    @connections.each {|i| i.inspect } if @connections.length > 0
  end
  
  def inspect
    puts "Performer of height #{@height} and weight #{@weight}"
  end
end

# create an array performers
input = [ [65, 100], [70, 150], [56, 90], [75,190], [60, 95], [68, 110] ]
perf = []
input.each { |x| perf.push(Performer.new(x[0], x[1])) }

# make appropriate connections
perf.each do |i|
  perf.each do |j|
    if i != j and i.weight < j.weight and i.height < j.height
      i.addConnection(j)
    end
  end
end

# now, find the longest path in this graph and we're done



      
  
