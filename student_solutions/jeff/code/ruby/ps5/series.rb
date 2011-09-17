#! /usr/local/bin/ruby
# Jeffrey M. Radcliffe
# Thu Feb 22, 2001  4:39 PM

class Series
  def initialize(val)
    @max = val
    @table = []
    @max.times do @table << [] end
    @max -= 1
  end

  def stuff
    @max.times { @table[@max].push (0) }
    for i in 0..(@max - 1); @table[i][@max] = 1 ; end
    @table[@max][@max] = 0
    (@max - 1).downto(0) do |x|
      (@max - 1).downto(0) do |y|
	foo = (@table[x][y+1] + @table[x+1][y]) / 2.0
	@table[x][y] = foo
      end
    end
    display
    return @table
  end

  def display
    puts "\n              BEST #{@max/2 + 1} OF #{@max + 1} "
    puts "----------------------------------------------"
    
    for i in 0..@max
      @table[i].each {|x| printf"%f  ",x }
      puts
    end
  end
end

if ARGV.length == 1
  val = ARGV[0].to_i
else
  val = 7
end

s = Series.new(val)
s.stuff
